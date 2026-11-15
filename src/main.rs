// TODO: is there a better way to structure modules vs files?
mod cmd;
mod error;
mod opts;
mod post;
mod result;
mod rewrite;
mod store;
mod template;

use std::io::Write;
use clap::Parser;

use crate::result::Result;

fn handle_glob(glob_pattern: &str,
               router: &dyn Fn(&std::path::Path) -> Result<std::path::PathBuf>,
               handler: &dyn Fn(&std::path::Path, &std::path::Path) -> Result<()>) -> Result<()> {
    log::trace!("Handle {glob_pattern:?} glob");
    for pb in glob::glob(glob_pattern)? {
        let p = pb?;
        if p.is_dir() { continue }
        let op = router(&p)?;
        handler(&p, op.as_path())?;
    }
    Ok(())
}

fn main() -> Result<()> {
    let o = crate::opts::Opts::parse();
    env_logger::Builder::new()
        .format(|buf, record| writeln!(buf, "{}: {}", record.level(), record.args()))
        .filter_level(o.verbose.log_level_filter())
        .init();

    struct FeedMeta {
        title: &'static str,
        desc: &'static str,
        root: &'static str,
        author: &'static str,
        email: &'static str,
        history: usize,
    }

    let feed_meta = FeedMeta {
        title: "trofi - All posts",
        desc: "trofi's blog",
        root: "https://trofi.github.io",
        author: "Sergei Trofimovich",
        email: "slyich@gmail.com",
        history: 30,
    };

    let store = store::Store::new(&o.out)?;

    struct RenderedPost {
        path: Box<std::path::Path>,
        post: crate::post::Post,
    }
    let posts: std::cell::RefCell<Vec<RenderedPost>> = std::cell::RefCell::new(Vec::new());

    let id_router = |p: &std::path::Path| -> Result<std::path::PathBuf> {
        Ok(p.to_path_buf())
    };
    let html_router = |p: &std::path::Path| -> Result<std::path::PathBuf> {
        Ok(p.with_extension("html"))
    };
    let svg_router = |p: &std::path::Path| -> Result<std::path::PathBuf> {
        Ok(p.with_extension("svg"))
    };

    let copy_file_handler = |p: &std::path::Path, op: &std::path::Path| -> Result<()> {
        let data = std::fs::read(p)?;
        let _ = store.store_file(op.to_str().expect("unencodable str"), &data);
        Ok(())
    };
    let dot_handler = |p: &std::path::Path, op: &std::path::Path| -> Result<()> {
        let data = std::fs::read_to_string(p)?;
        let svg = crate::cmd::run_cmd("dot", &["-Tsvg"], &data)?;

        store.store_file(op.to_str().expect("unencodable str"), &svg)?;
        Ok(())
    };
    let md_handler = |p: &std::path::Path, op: &std::path::Path| -> Result<()> {
        let md = std::fs::read_to_string(p)?;

        let r = crate::post::Post::new_from_str(&md, op, &store)?;

        // TODO: split handlers for posts and other pages
        if r.metadata.date.is_some() {
            posts.borrow_mut().push(RenderedPost{
                path: op.into(),
                post: r.clone(),
            });
        }

        let url = format!("{}/{}", feed_meta.root, op.to_str().expect("unencodable str"));
        let post = template::render_page(&r.md_html, &r.metadata);
        let rpost = crate::rewrite::relativize_html(&post, &url)?;

        store.store_file(op.to_str().expect("unencodable str"), rpost.as_bytes())?;
        Ok(())
    };

    handle_glob("images/*", &id_router, &copy_file_handler)?;
    handle_glob("css/*", &id_router, &copy_file_handler)?;
    handle_glob("posts.data/**/*.dot", &svg_router, &dot_handler)?;
    handle_glob("posts.data/**/*", &id_router, &copy_file_handler)?;
    handle_glob("pages/*.md",
                &|p| Ok(p.strip_prefix("pages/").expect("wrong prefix").with_extension("html")),
                &md_handler)?;
    handle_glob("posts/*.md", &html_router, &md_handler)?;
    handle_glob("static/README.md", &|_| Ok(std::path::PathBuf::from("README.md")), &copy_file_handler)?;

    let mut posts = posts.take();
    posts.sort_by_key(|p| {
        p.post.metadata.date
    });

    {
        let html = maud::html! {
            "All posts on a single page:"
            ul {
               @for p in posts.iter().rev() {
                   li {
                       a href={ "/" (p.path.to_str().expect("unencodable str")) } { (p.post.metadata.title) }
                       " - "
                       @if let Some(ref date) = p.post.metadata.date {
                           (date.format(crate::post::BLOG_DATE_FORMAT))
                       }
                   }
               }
            }
        }.into_string();
        let metadata = crate::post::Metadata {
            title: "blog".to_string(),
            date: None,
            root: None,
        };

        let post = template::render_page(&html, &metadata);
        let rpost = crate::rewrite::relativize_html(&post, &format!("{}/blog.html", feed_meta.root))?;

        store.store_file("blog.html", rpost.as_bytes())?;
    }

    {
        let feed_path = "feed/atom.xml";
        let feed = atom_syndication::FeedBuilder::default()
            .title(feed_meta.title)
            .subtitle(
                atom_syndication::TextBuilder::default()
                    .value(feed_meta.desc)
                    .build())
            .updated(posts.iter().filter(|e|
                // TODO: move out of RSS handling and make less ugly
                e.post.metadata.date.is_some()).last().map(|e|
                    e.post.metadata.date.expect("a date").fixed_offset()).expect("a value"))
            .id(format!("{}/{}", feed_meta.root, feed_path))
            .links(vec![
                atom_syndication::LinkBuilder::default()
                    .rel("self")
                    .href(format!("{}/{}", feed_meta.root, feed_path))
                    .build(),
                atom_syndication::LinkBuilder::default()
                    .href(feed_meta.root)
                    .build(),
            ])
            .author(
                atom_syndication::PersonBuilder::default()
                    .name(feed_meta.author)
                    .email(Some(feed_meta.email.to_string()))
                    .build(),
            )
            .entries(
                posts.iter().rev().take(feed_meta.history).map(|e| {
                    let post_url = format!("{}/{}", feed_meta.root, e.path.to_str().expect("unencodable str"));
                    let post_link = format!("{}/{}", e.post.metadata.root.as_ref().unwrap_or(&post_url), e.path.to_str().expect("unencodable str"));
                    atom_syndication::EntryBuilder::default()
                        .published(e.post.metadata.date.expect("a date").fixed_offset())
                        .updated(e.post.metadata.date.expect("a date").fixed_offset())
                        .id(post_link.clone())
                        .title(e.post.metadata.title.clone())
                        .link(
                            atom_syndication::LinkBuilder::default()
                                .href(post_link.clone())
                                .build())
                        .summary(
                             atom_syndication::TextBuilder::default()
                                .r#type(atom_syndication::TextType::Html)
                                .value(rewrite::absolutize_html(&e.post.md_html_for_feed, &post_url).expect("atom absolutized"))
                                .build())
                        .build()
                }).collect::<Vec<_>>()
            )
            .build()
            .to_string();

        store.store_file(feed_path, feed.as_bytes())?;
    }

    {
        let feed_path = "feed/rss.xml";
        let rfc822_time_format = "%a, %d %b %Y %H:%M:%S %z";
        let feed = rss::ChannelBuilder::default()
            .title(feed_meta.title)
            .link(feed_meta.root)
            .last_build_date(posts.iter().filter(|e|
                // TODO: move out of RSS handling and make less ugly
                e.post.metadata.date.is_some()).last().map(|e|
                    e.post.metadata.date.expect("a date").format(rfc822_time_format).to_string()))
            .atom_ext(
                rss::extension::atom::AtomExtensionBuilder::default()
                    .link(
                        atom_syndication::LinkBuilder::default()
                            .rel("self")
                            .href(format!("{}/{}", feed_meta.root, feed_path))
                            .mime_type(Some("application/rss+xml".to_string()))
                            .build(),
                    )
                    .build()
            )
            .description(feed_meta.desc)
            .items(
                posts.iter().rev().take(feed_meta.history).map(|e| {
                    let post_url = format!("{}/{}", feed_meta.root, e.path.to_str().expect("unencodable str"));
                    let post_link = format!("{}/{}", e.post.metadata.root.as_ref().unwrap_or(&post_url), e.path.to_str().expect("unencodable str"));
                    rss::ItemBuilder::default()
                        .dublin_core_ext(
                            rss::extension::dublincore::DublinCoreExtensionBuilder::default()
                                .creator(feed_meta.author.to_string())
                                .build()
                        )
                        .title(e.post.metadata.title.clone())
                        .link(post_link.clone())
                        .guid(
                            rss::GuidBuilder::default()
                                .value(post_link.clone())
                                .build()
                        )
                        .description(rewrite::absolutize_html(&e.post.md_html_for_feed, &post_url).expect("rss absolutized"))
                        .pub_date(
                            e.post.metadata.date.expect("a date").format(rfc822_time_format).to_string()
                        )
                        .build()
                }).collect::<Vec<_>>()
            )
            .build()
            .to_string();

        store.store_file(feed_path, feed.as_bytes())?;
    }

    Ok(())
}

// TODO: is there a better way to structure modules vs files?
mod cmd;
mod error;
mod feed;
mod opts;
mod post;
mod result;
mod rewrite;
mod store;
mod template;

use std::io::Write;
use chrono::Datelike;
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

        let url = format!("{}/{}", o.root, op.to_str().expect("unencodable str"));
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
            @for py in posts.chunk_by(|l,r| l.post.metadata.date.expect("L").year() == r.post.metadata.date.expect("R").year()).rev() {
                @if let Some(ref date) = py[0].post.metadata.date {
                    h2 {
                        (date.year())
                        " ("
                        (py.len())
                        ")"
                    }
                }
                ul {
                    @for p in py.iter().rev() {
                        li {
                            @if let Some(ref date) = p.post.metadata.date {
                                (date.format("%b %e"))
                            }
                            ": "
                            a href={ "/" (p.path.to_str().expect("unencodable str")) } { (p.post.metadata.title) }
                            @if let Some(ref tags) = p.post.metadata.tags {
                                @for (ix, tag) in tags.iter().enumerate() {
                                    @if ix > 0 { "," }
                                    " #"
                                    (tag)
                                }
                            }
                        }
                    }
                }
            }
        }.into_string();
        let metadata = crate::post::Metadata {
            title: "blog".to_string(),
            date: None,
            root: None,
            tags: None,
        };

        let post = template::render_page(&html, &metadata);
        let rpost = crate::rewrite::relativize_html(&post, &format!("{}/blog.html", o.root))?;

        store.store_file("blog.html", rpost.as_bytes())?;
    }

    let feed_meta = crate::feed::FeedMeta {
        title: "trofi - All posts",
        desc: "trofi's blog",
        root: "https://trofi.github.io",
        author: "Sergei Trofimovich",
        email: "slyich@gmail.com",
        atom_rel_path: "feed/atom.xml",
        rss_rel_path: "feed/rss.xml",
    };
    let feed_history = 30;
    let feed_posts: Vec<crate::feed::FeedPost> =posts
        .iter()
        .rev()
        .take(feed_history)
        .map(|p|
            crate::feed::FeedPost{
                path: &p.path,
                post: &p.post,
            }
        ).collect();

    let feed = crate::feed::render_feed(&feed_meta, &feed_posts)?;
    store.store_file(feed_meta.atom_rel_path, feed.atom.as_bytes())?;
    store.store_file(feed_meta.rss_rel_path, feed.rss.as_bytes())?;

    Ok(())
}

use crate::result::Result;

pub(crate) struct FeedMeta<'a> {
    pub(crate) title: &'a str,
    pub(crate) desc: &'a str,
    pub(crate) root: &'a str,
    pub(crate) author: &'a str,
    pub(crate) email: &'a str,
    // Where to store outputs, what self-links to use.
    pub(crate) atom_rel_path: &'a str,
    pub(crate) rss_rel_path: &'a str,
}

pub(crate) struct FeedPost<'a> {
    pub(crate) path: &'a std::path::Path,
    pub(crate) post: &'a crate::post::Post,
}

pub(crate) struct Rendered {
    pub(crate) atom: String,
    pub(crate) rss: String,
}

pub(crate) fn render_feed(feed_meta: &FeedMeta, posts: &[FeedPost]) -> Result<Rendered> {
    let atom = render_atom(feed_meta, posts)?;
    let rss = render_rss(feed_meta, posts)?;
    Ok(Rendered{
        atom,
        rss,
    })
}

fn render_atom(feed_meta: &FeedMeta, posts: &[FeedPost]) -> Result<String> {
    Ok(atom_syndication::FeedBuilder::default()
        .title(feed_meta.title)
        .subtitle(
            atom_syndication::TextBuilder::default()
                .value(feed_meta.desc)
                .build())
        .updated(posts.iter().filter(|e|
            // TODO: move out of RSS handling and make less ugly
            e.post.metadata.date.is_some()).next().map(|e|
                e.post.metadata.date.expect("a date").fixed_offset()).expect("a value"))
        .id(format!("{}/{}", feed_meta.root, feed_meta.atom_rel_path))
        .links(vec![
            atom_syndication::LinkBuilder::default()
                .rel("self")
                .href(format!("{}/{}", feed_meta.root, feed_meta.atom_rel_path))
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
            posts.iter().map(|e| {
                let post_url = format!("{}/{}", feed_meta.root, e.path.to_str().expect("unencodable str"));
                let post_link = format!("{}/{}", e.post.metadata.root.clone().unwrap_or(feed_meta.root.to_string()), e.path.to_str().expect("unencodable str"));
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
                            .value(crate::rewrite::absolutize_html(&e.post.md_html_for_feed, &post_url).expect("atom absolutized"))
                            .build())
                    .build()
            }).collect::<Vec<_>>()
        )
        .build()
        .to_string())
}

fn render_rss(feed_meta: &FeedMeta, posts: &[FeedPost]) -> Result<String> {
    let rfc822_time_format = "%a, %d %b %Y %H:%M:%S %z";
    Ok(rss::ChannelBuilder::default()
        .title(feed_meta.title)
        .link(feed_meta.root)
        .last_build_date(posts.iter().filter(|e|
            // TODO: move out of RSS handling and make less ugly
            e.post.metadata.date.is_some()).next().map(|e|
                e.post.metadata.date.expect("a date").format(rfc822_time_format).to_string()))
        .atom_ext(
            rss::extension::atom::AtomExtensionBuilder::default()
                .link(
                    atom_syndication::LinkBuilder::default()
                        .rel("self")
                        .href(format!("{}/{}", feed_meta.root, feed_meta.rss_rel_path))
                        .mime_type(Some("application/rss+xml".to_string()))
                        .build(),
                )
                .build()
        )
        .description(feed_meta.desc)
        .items(
            posts.iter().map(|e| {
                let post_url = format!("{}/{}", feed_meta.root, e.path.to_str().expect("unencodable str"));
                let post_link = format!("{}/{}", e.post.metadata.root.clone().unwrap_or(feed_meta.root.to_string()), e.path.to_str().expect("unencodable str"));
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
                    .description(crate::rewrite::absolutize_html(&e.post.md_html_for_feed, &post_url).expect("rss absolutized"))
                    .pub_date(
                        e.post.metadata.date.expect("a date").format(rfc822_time_format).to_string()
                    )
                    .build()
            }).collect::<Vec<_>>()
        )
        .build()
        .to_string())
}

pub(crate) fn render_page(html: &str, meta: &crate::post::Metadata) -> String {
    maud::html! {
        (maud::PreEscaped("<!doctype html>"))
        html lang="en" {
        head {
            meta charset="utf-8";
            meta name="viewport" content="width=device-width, initial-scale=1.0";
            title { (meta.title) }
            link rel="stylesheet" type="text/css" href="/css/default.css";
            link rel="stylesheet" type="text/css" href="/css/code.css";
            link rel="alternate" type="application/atom+xml" href="/feed/atom.xml" title="Atom 1.0";
            link rel="alternate" type="application/rss+xml" href="/feed/rss.xml" title="RSS 2.0";
            link rel="shortcut icon" href="/images/favicon.ico";
        }
        body {
            div id="navigation" {
                a href="/index.html" { "home" }
                "\n"
                a href="/blog.html" { "blog" }
                "\n"
                a href="/log.html" { "log" }
                "\n"
                a href="/feed/atom.xml" { "atom" }
                "\n"
                a href="/feed/rss.xml" { "rss" }
                "\n"
                a href="/about.html" { "about" }
                "\n"
                a href="/contact.html" { "contact" }
            }
            div id="content" {
                h1 { (meta.title) }
                @if let Some(ref date) = meta.date {
                    div.info {
                        (date.format(crate::post::BLOG_DATE_FORMAT))

                        @if let Some(ref tags) = meta.tags {
                            br;
                            @for (ix, tag) in tags.iter().enumerate() {
                                @if ix > 0 { "," }
                                " #"
                                (tag)
                            }
                        }
                    }
                }
                (maud::PreEscaped(html))
            }
        }
        }
    }.into_string()
}

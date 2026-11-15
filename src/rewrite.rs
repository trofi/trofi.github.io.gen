use crate::result::Result;

// base_url is an absolute page address: not a directory, no anchors.
// Returns relative `target` if there is  abase to build relation against
// TODO: worth returning absolute URLs when relative ones are inconstructible?
fn relativize_url(base: &str, target: &str) -> Result<Option<String>> {
    let base_url = url::Url::parse(base)?;
    let abs_target = base_url.clone().join(target)?;
    Ok(base_url.make_relative(&abs_target))
}

fn absolutize_url(base: &str, target: &str) -> Result<String> {
    let base_url = url::Url::parse(base)?;
    let abs_target = base_url.clone().join(target)?;
    Ok(abs_target.to_string())
}

pub(crate) fn relativize_html(html: &str, page_url: &str) -> Result<String> {
    let mut o = vec![];

    {
        let mut rewriter = lol_html::HtmlRewriter::new(
            lol_html::Settings::new().append_element_content_handler(
                lol_html::element!("[href], [src]", |el| {
                for attr in ["href", "src"] {
                    if let Some(attr_val) = el.get_attribute(attr) {
                        if let Some(rel_val) = relativize_url(page_url, &attr_val)? {
                            el.set_attribute(attr, &rel_val)?;
                        }
                    }
                }

                Ok(())
            })),
            |c: &[u8]| o.extend_from_slice(c),
        );

        rewriter.write(html.as_bytes())?;
        rewriter.end()?;
    }

    Ok(String::from_utf8(o)?)
}

pub(crate) fn absolutize_html(html: &str, page_url: &str) -> Result<String> {
    let mut o = vec![];

    {
        let mut rewriter = lol_html::HtmlRewriter::new(
            lol_html::Settings::new().append_element_content_handler(
                lol_html::element!("[href], [src]", |el| {
                for attr in ["href", "src"] {
                    if let Some(attr_val) = el.get_attribute(attr) {
                        let abs_val = absolutize_url(page_url, &attr_val)?;
                        el.set_attribute(attr, abs_val.as_str())?;
                    }
                }

                Ok(())
            })),
            |c: &[u8]| o.extend_from_slice(c),
        );

        rewriter.write(html.as_bytes())?;
        rewriter.end()?;
    }

    Ok(String::from_utf8(o)?)
}

#[cfg(test)]
mod tests {
    #[test]
    fn to_relative_html_test() {
        struct Test {
            page: &'static str,
            html: &'static str,
            want: &'static str,
        }
        let v: Vec<Test> = vec![
            Test {
                page: "https://b.log/foo.html",
                html: "<html><a href=\"/bar.html\"></html>",
                want: "<html><a href=\"bar.html\"></html>",
            },
            Test {
                page: "https://b.log/foo.html",
                html: "<html><a href=\"foo.html\"></html>",
                want: "<html><a href=\"\"></html>",
            },
            Test {
                page: "https://b.log/bar/foo.html",
                html: "<html><a href=\"/baz.html\"></html>",
                want: "<html><a href=\"../baz.html\"></html>",
            },
        ];
        for t in v {
            assert_eq!(crate::rewrite::relativize_html(t.html, t.page).expect("converted"), t.want)
        }
    }
    #[test]
    fn to_absolute_html_test() {
        struct Test {
            page: &'static str,
            html: &'static str,
            want: &'static str,
        }
        let v: Vec<Test> = vec![
            Test {
                page: "https://b.log/foo.html",
                html: "<html><a href=\"/bar.html\"></html>",
                want: "<html><a href=\"https://b.log/bar.html\"></html>",
            },
        ];
        for t in v {
            assert_eq!(crate::rewrite::absolutize_html(t.html, t.page).expect("converted"), t.want)
        }
    }
}

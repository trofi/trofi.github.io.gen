use serde::Deserialize;

use crate::result::Result;

#[derive(Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct Metadata {
    pub(crate) title: String,

    // The blog uses custom date format for posts
    #[serde(default, with = "blog_date_format")]
    pub(crate) date: Option<chrono::DateTime<chrono::Utc>>,

    // Overwrite ID in RSS feeds.
    // TODO: remove with blog post data
    pub(crate) root: Option<String>,

    pub(crate) tags: Option<Vec<String>>,
}

pub(crate) const BLOG_DATE_FORMAT: &str = "%B %e, %Y";
mod blog_date_format {
    use serde::{self, Deserialize, Deserializer};

    pub fn deserialize<'de, D>(
        deserializer: D,
    ) -> Result<Option<chrono::DateTime<chrono::Utc>>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let dt = chrono::NaiveDate::parse_from_str(&s, crate::post::BLOG_DATE_FORMAT)
            .map_err(serde::de::Error::custom)?
            .and_hms_opt(0,0,0).expect("good h/m/s")
            .and_utc();
        Ok(Some(dt))
    }
}

#[derive(Clone)]
pub(crate) struct Post {
    pub(crate) metadata: Metadata,
    pub(crate) md_html: String,
    pub(crate) md_html_for_feed: String,
}

#[derive(Eq,Hash,PartialEq,Clone,Copy)]
enum FileType {
    Graphviz,
    Gnuplot,
}

// Rewrite post contents with various passes
struct PostMangler<'a> {
    root: comrak::Node<'a>,
    op: &'a std::path::Path,
    store: &'a crate::store::Store,
    ft_counter: usize,
}

impl<'a> PostMangler<'a> {
    fn new(root: comrak::Node<'a>, op: &'a std::path::Path, store: &'a crate::store::Store) -> Self {
        Self {
            root,
            op,
            store,
            ft_counter: 0,
        }
    }

    fn gen_file_name(&mut self, ft: FileType) -> String {
        let n = self.ft_counter;
        self.ft_counter += 1;
        let post_stem = self.op.file_prefix().expect("missing prefix").to_str().expect("unencodable str");
        let prefix = format!("posts.data.inline/{}", post_stem);
        match ft {
            FileType::Graphviz => format!("{prefix}/fig-{n}.gv.svg"),
            FileType::Gnuplot =>  format!("{prefix}/fig-{n}.gp.svg"),
        }
    }

    fn mangle_inplace_code_blocks(&mut self) -> Result<()> {
        for node in self.root.descendants() {
            let mut data = node.data.borrow_mut();

            // Handle LaTeX -> MathML conversion. Used the idea of
            //   https://github.com/kivikakk/comrak/discussions/435
            // But we use math-core crate instead.
            if let comrak::nodes::NodeValue::Math(ref math) = data.value {
                    let config = math_core::MathCoreConfig::default();
                    let converter = math_core::LatexToMathML::new(config, vec![])?;
                    let block_mode = match math.display_math {
                        true => math_core::MathDisplay::Block,
                        false => math_core::MathDisplay::Inline,
                    };
                    let result = converter.convert_with_local_state(&math.literal, block_mode)?;

                    data.value = if math.display_math {
                        comrak::nodes::NodeValue::HtmlBlock(comrak::nodes::NodeHtmlBlock {
                            block_type: 0,
                            literal: result.mathml,
                        })
                    } else {
                        comrak::nodes::NodeValue::HtmlInline(result.mathml)
                    };
            }

            if let comrak::nodes::NodeValue::CodeBlock(ref ncb) = data.value && ncb.info == "{render=dot}" {
                let svg = crate::cmd::run_cmd("dot", &["-Tsvg"], &ncb.literal)?;
    
                let filename = self.gen_file_name(FileType::Graphviz);
                self.store.store_file(&filename, &svg)?;
    
                data.value = comrak::nodes::NodeValue::Image(
                    Box::new(
                        comrak::nodes::NodeLink {
                            url: format!("/{filename}"),
                            title: "".to_owned(),
                        }));
            }

            if let comrak::nodes::NodeValue::CodeBlock(ref ncb) = data.value && ncb.info == "{render=gnuplot}" {
                let svg = crate::cmd::run_cmd("gnuplot", &["--default-settings"
                                              , "-e", "set terminal svg"
                                              , "-"], &ncb.literal)?;

                let filename = self.gen_file_name(FileType::Gnuplot);
                self.store.store_file(&filename, &svg)?;
    
                data.value = comrak::nodes::NodeValue::Image(
                    Box::new(
                        comrak::nodes::NodeLink {
                            url: format!("/{filename}"),
                            title: "".to_owned(),
                        }));
            }
        }
        Ok(())
    }
}

// extract one of:
//   "---\n<yaml>\n---\n<md>"
//   "+++\n<toml>\n+++\n<md>"
// TODO: drop YAML once all posts are converted
fn get_header_metadata(header_and_md_str: &str) -> Result<(Metadata, &str)> {

    // yaml
    if let Some(v) = header_and_md_str.strip_prefix("---\n") {
        if let Some((yaml, md_str)) = v.split_once("---\n") {
            return Ok((yaml_serde::from_str(yaml)?, md_str));
        }
    }

    // toml
    if let Some(v) = header_and_md_str.strip_prefix("+++\n") {
        if let Some((toml, md_str)) = v.split_once("+++\n") {
            return Ok((toml::from_str(toml)?, md_str));
        }
    }

    Err(crate::error::BlogError::Logic("No header"))
}

impl Post {
    pub(crate) fn new_from_str(header_and_md_str: &str, op: &std::path::Path, store: &crate::store::Store) -> Result<Self> {
        let (metadata, md_str) = get_header_metadata(header_and_md_str)?;
        let arena = comrak::Arena::new();
    
        let mut options = comrak::Options::default();
        options.extension = comrak::options::Extension::builder()
            .strikethrough(true)
            .table(true)
            .math_dollars(true)
            .subscript(true)
            .superscript(true)
            // TODO: auto-generate `id="..."` to <h1> & co headers.
            // .header_id_prefix("".to_owned())
            .build();
        // allow inline HTML from dot and gnuplot
        options.render.r#unsafe = true;
    
        let root = comrak::parse_document(&arena, md_str, &options);
    
        let mut mangler = PostMangler::new(root, op, store);
        mangler.mangle_inplace_code_blocks()?;

        // TODO: re-enable syntax highlighting once css is simpler.
        // render fully highlighed posts for browsers
        //let adapter = comrak::plugins::syntect::SyntectAdapterBuilder::new().css().build();
        //let adapter = comrak::plugins::syntect::SyntectAdapterBuilder::new().theme("base16-ocean.dark").build();
        //let mut plugins = comrak::options::Plugins::default();
        //plugins.render.codefence_syntax_highlighter = Some(&adapter);
        //let mut md_html = String::new();
        //comrak::format_html_with_plugins(root, &options, &mut md_html, &plugins)?;
        let mut md_html = String::new();
        comrak::format_html(root, &options, &mut md_html)?;

        // render unhighlighed posts for RSS readers
        let mut md_html_for_feed = String::new();
        comrak::format_html(root, &options, &mut md_html_for_feed)?;

        Ok(Post {
            metadata,
            md_html,
            md_html_for_feed,
        })
    }
}

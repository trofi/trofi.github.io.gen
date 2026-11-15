use std::io::Write;

use crate::result::Result;

// API of a target directory where the final site will reside.

pub(crate) struct Store {
    store_dir: String,
}

impl Store {
    pub(crate) fn new(store_dir: &str) -> Result<Store> {
        std::fs::create_dir_all(store_dir)?;

        Ok(Store{
            store_dir: store_dir.to_string(),
        })
    }

    pub(crate) fn store_file(&self, relative_path: &str, content: &[u8]) -> Result<()> {
        let path = std::path::Path::new(&self.store_dir).join(relative_path);
        std::fs::create_dir_all(path.parent().expect("has parent")).expect("CDA");
        // TODO: we currently allow overwrites. Should we reuse files instead?
        let mut f = std::fs::File::options().create(true).write(true).truncate(true).open(path)?;
        f.write_all(content)?;
        Ok(())
    }
}

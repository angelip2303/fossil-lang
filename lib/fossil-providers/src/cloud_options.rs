use fossil_lang::runtime::storage::StorageConfig;
use polars::prelude::cloud::CloudOptions;
use polars::prelude::{PlPath, PlPathRef};

/// Converts StorageConfig into Polars CloudOptions for a given URL.
/// Returns None for local paths or when config is empty (fallback to env vars).
pub fn build_cloud_options(url: &str, config: &StorageConfig) -> Option<CloudOptions> {
    if config.is_empty() {
        return None;
    }

    let path = PlPath::from_str(url);
    match path.as_ref() {
        PlPathRef::Local(_) => None,
        PlPathRef::Cloud(cloud_path) => {
            let scheme = cloud_path.scheme();
            let pairs: Vec<(&str, &str)> = config
                .as_map()
                .iter()
                .map(|(k, v)| (k.as_str(), v.as_str()))
                .collect();

            CloudOptions::from_untyped_config(Some(&scheme), pairs).ok()
        }
    }
}

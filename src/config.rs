use std::fs;
use std::path::{Path, PathBuf};
use globset::{Glob, GlobMatcher};
use serde::{Deserialize, Deserializer};

pub const CONFIG_FILE_NAME: &str = "ron.toml";

#[derive(Debug, Deserialize, Default)]
pub struct Config {
    /// Map glob pattern to file path
    #[serde(default)]
    types: Vec<TypePattern>,

    #[serde(default)]
    pub root_dir: Option<PathBuf>,
}

impl Config {
    /// Load config from the ron.toml file, if present. Returns the default config otherwise
    pub fn try_load_from_dir<P: AsRef<Path>>(dir: P) -> anyhow::Result<Option<Self>> {
        let path = dir.as_ref().join(CONFIG_FILE_NAME);

        let config = if path.exists() && path.is_file() {
            let text = fs::read_to_string(&path)?;
            let mut config: Config = toml::from_str(&text)?;
            config.root_dir.get_or_insert(PathBuf::from(dir.as_ref()));
            Some(config)
        } else {
            None
        };

        Ok(config)
    }

    pub fn match_module_path<P: AsRef<Path>>(&self, file_path: P) -> Option<&String> {
        let file_path = file_path.as_ref();
        let rel = self
            .root_dir
            .as_ref()
            .and_then(|root| file_path.strip_prefix(root).ok())
            .unwrap_or(file_path);

        self.types.iter().find_map(|TypePattern { glob, path }| {
            if glob.is_match(rel) {
                Some(path)
            } else {
                None
            }
        })
    }
}

#[derive(Debug, Deserialize)]
struct TypePattern {
    #[serde(deserialize_with = "deserialize_glob")]
    glob: GlobMatcher,
    #[serde(rename = "type")]
    path: String,
}

fn deserialize_glob<'de, D>(deserializer: D) -> Result<GlobMatcher, D::Error> where D: Deserializer<'de> {
    String::deserialize(deserializer)
        .and_then(|glob| Glob::new(&glob).map_err(serde::de::Error::custom))
        .map(|glob| glob.compile_matcher())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_match_module_path() {
        // Use PathBuf::from components to build platform-appropriate paths
        let root_dir: PathBuf = ["my", "project"].iter().collect();
        let config = Config {
            types: vec![
                TypePattern {
                    glob: Glob::new("**/post.ron").unwrap().compile_matcher(),
                    path: "crate::models::Post".to_string(),
                }
            ],
            root_dir: Some(root_dir.clone()),
        };

        let matching_path: PathBuf = root_dir.join("example").join("data").join("post.ron");
        let result = config.match_module_path(&matching_path);
        assert_eq!(result, Some(&"crate::models::Post".to_string()));

        let non_matching_path: PathBuf = root_dir.join("example").join("data").join("user.ron");
        let result = config.match_module_path(&non_matching_path);
        assert_eq!(result, None);
    }
}
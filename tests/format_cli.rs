#![cfg(feature = "cli")]

use std::{fs, process::Command};

#[test]
fn formats_files_and_directories_in_place() {
    let root = std::env::temp_dir().join(format!("ron-format-cli-{}", std::process::id()));
    fs::create_dir_all(root.join("nested")).unwrap();
    let source = "#![enable(implicit_some)]\nData(name: \"hello\")";
    fs::write(root.join("one.ron"), source).unwrap();
    fs::write(root.join("nested/two.ron"), source).unwrap();
    fs::write(root.join("untouched.txt"), source).unwrap();
    let binary = env!("CARGO_BIN_EXE_ron-lsp");
    assert!(Command::new(binary)
        .arg("format")
        .arg(root.join("one.ron"))
        .status()
        .unwrap()
        .success());
    let formatted = fs::read_to_string(root.join("one.ron")).unwrap();
    assert!(formatted.contains("    name: \"hello\","));
    assert!(formatted.starts_with("#![enable(implicit_some)]"));
    assert!(Command::new(binary)
        .arg("format")
        .current_dir(&root)
        .status()
        .unwrap()
        .success());
    assert_eq!(
        fs::read_to_string(root.join("nested/two.ron")).unwrap(),
        formatted
    );
    assert_eq!(fs::read_to_string(root.join("one.ron")).unwrap(), formatted);
    assert_eq!(
        fs::read_to_string(root.join("untouched.txt")).unwrap(),
        source
    );
    assert!(!Command::new(binary)
        .arg("format")
        .arg(root.join("missing"))
        .output()
        .unwrap()
        .status
        .success());
    fs::remove_dir_all(root).unwrap();
}

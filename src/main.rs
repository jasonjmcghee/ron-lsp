mod annotation_parser;
mod completion;
mod diagnostic_reporter;
mod diagnostics;
mod ron_parser;
mod rust_analyzer;

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
pub struct Document {
    content: String,
    type_annotation: Option<String>,
}

pub struct Backend {
    client: Client,
    documents: Arc<RwLock<HashMap<String, Document>>>,
    rust_analyzer: Arc<rust_analyzer::RustAnalyzer>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
            rust_analyzer: Arc::new(rust_analyzer::RustAnalyzer::new()),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Get workspace root
        if let Some(workspace_folders) = params.workspace_folders {
            if let Some(folder) = workspace_folders.first() {
                let root = folder.uri.to_file_path().unwrap();
                self.client
                    .log_message(
                        MessageType::INFO,
                        format!("Setting workspace root to: {:?}", root),
                    )
                    .await;
                self.rust_analyzer.set_workspace_root(&root).await;

                // Log what types were found
                let types = self.rust_analyzer.get_all_types().await;
                self.client
                    .log_message(
                        MessageType::INFO,
                        format!("Found {} types in workspace", types.len()),
                    )
                    .await;
                for type_info in types.iter().take(10) {
                    self.client
                        .log_message(MessageType::INFO, format!("  - {}", type_info.name))
                        .await;
                }
            }
        } else if let Some(root_uri) = params.root_uri {
            let root = root_uri.to_file_path().unwrap();
            self.client
                .log_message(
                    MessageType::INFO,
                    format!("Setting workspace root to: {:?}", root),
                )
                .await;
            self.rust_analyzer.set_workspace_root(&root).await;

            // Log what types were found
            let types = self.rust_analyzer.get_all_types().await;
            self.client
                .log_message(
                    MessageType::INFO,
                    format!("Found {} types in workspace", types.len()),
                )
                .await;
            for type_info in types.iter().take(10) {
                self.client
                    .log_message(MessageType::INFO, format!("  - {}", type_info.name))
                    .await;
            }
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![
                        ":".to_string(),
                        " ".to_string(),
                        ",".to_string(),
                    ]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "RON LSP server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        let content = params.text_document.text;

        let type_annotation = annotation_parser::parse_type_annotation(&content);

        self.documents.write().await.insert(
            uri.clone(),
            Document {
                content: content.clone(),
                type_annotation: type_annotation.clone(),
            },
        );

        // Publish diagnostics
        self.publish_diagnostics(&uri, &content, type_annotation.as_deref())
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        if let Some(change) = params.content_changes.into_iter().next() {
            let content = change.text;
            let type_annotation = annotation_parser::parse_type_annotation(&content);

            self.documents.write().await.insert(
                uri.clone(),
                Document {
                    content: content.clone(),
                    type_annotation: type_annotation.clone(),
                },
            );

            // Publish diagnostics
            self.publish_diagnostics(&uri, &content, type_annotation.as_deref())
                .await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        self.documents.write().await.remove(&uri);
    }

    async fn did_save(&self, _params: DidSaveTextDocumentParams) {
        // No-op for now
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let position = params.text_document_position.position;

        let documents = self.documents.read().await;
        if let Some(doc) = documents.get(&uri) {
            if let Some(type_path) = &doc.type_annotation {
                // Get type info from Rust analyzer
                if let Some(type_info) = self.rust_analyzer.get_type_info(type_path).await {
                    let completions = completion::generate_completions(
                        &doc.content,
                        position,
                        &type_info,
                        self.rust_analyzer.clone(),
                    )
                    .await;
                    return Ok(Some(CompletionResponse::Array(completions)));
                }
            }
        }

        Ok(None)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        let documents = self.documents.read().await;
        if let Some(doc) = documents.get(&uri) {
            if let Some(type_path) = &doc.type_annotation {
                if let Some(type_info) = self.rust_analyzer.get_type_info(type_path).await {
                    if let Some(field_name) =
                        ron_parser::get_field_at_position(&doc.content, position)
                    {
                        if let Some(fields) = type_info.fields() {
                            if let Some(field) = fields.iter().find(|f| f.name == field_name) {
                                return Ok(Some(Hover {
                                    contents: HoverContents::Markup(MarkupContent {
                                        kind: MarkupKind::Markdown,
                                        value: format!(
                                            "```rust\n{}: {}\n```\n\n{}",
                                            field.name,
                                            field.type_name,
                                            field.docs.as_deref().unwrap_or("")
                                        ),
                                    }),
                                    range: None,
                                }));
                            }
                        }
                    }
                }
            }
        }

        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        let documents = self.documents.read().await;
        let doc = match documents.get(&uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        // Get the word at cursor position - early return if none
        let word = match get_word_at_position(&doc.content, position) {
            Some(w) => w,
            None => return Ok(None),
        };

        // If we have a type annotation, get the type info once
        let type_info = if let Some(type_path) = &doc.type_annotation {
            self.rust_analyzer.get_type_info(type_path).await
        } else {
            None
        };

        // Check if the word is a valid field name in this type
        if let Some(ref info) = type_info {
            if let Some(field) = info.find_field(&word) {
                return create_location_response(&info.source_file, field.line, field.column);
            }

            // Check if the word is a valid enum variant in this type
            if let Some(variant) = info.find_variant(&word) {
                return create_location_response(&info.source_file, variant.line, variant.column);
            }

            // Check if the word matches the type name itself
            if info.name.ends_with(&format!("::{}", word)) || info.name == word {
                return create_location_response(&info.source_file, info.line, info.column);
            }

            // Check if we're on a field, and if the word is a variant of that field's type
            // e.g., in "post_type: Short", if cursor is near Short, check if it's a variant of PostType
            if let Some(field_name) = ron_parser::get_field_at_position(&doc.content, position) {
                if let Some(field) = info.find_field(&field_name) {
                    // Get the type of this specific field
                    if let Some(field_type_info) =
                        self.rust_analyzer.get_type_info(&field.type_name).await
                    {
                        // Check if word is a variant of this field's type
                        if let Some(variant) = field_type_info.find_variant(&word) {
                            return create_location_response(
                                &field_type_info.source_file,
                                variant.line,
                                variant.column,
                            );
                        }
                    }
                }
            }
        }

        // If no type annotation, or word doesn't match a field, try to find as a type
        if let Some(info) = self.rust_analyzer.get_type_info(&word).await {
            return create_location_response(&info.source_file, info.line, info.column);
        }

        Ok(None)
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let position = params.text_document_position.position;
        let new_name = params.new_name;

        let documents = self.documents.read().await;
        if let Some(doc) = documents.get(&uri) {
            if let Some(field_name) = ron_parser::get_field_at_position(&doc.content, position) {
                // Find all occurrences of this field name in the document
                let mut changes = Vec::new();

                for (line_num, line) in doc.content.lines().enumerate() {
                    if let Some(start) = line.find(&field_name) {
                        // Check if this is actually the field name (followed by a colon)
                        let after = &line[start + field_name.len()..];
                        if after.trim_start().starts_with(':') {
                            changes.push(TextEdit {
                                range: Range::new(
                                    Position::new(line_num as u32, start as u32),
                                    Position::new(
                                        line_num as u32,
                                        (start + field_name.len()) as u32,
                                    ),
                                ),
                                new_text: new_name.clone(),
                            });
                        }
                    }
                }

                if !changes.is_empty() {
                    let mut map = std::collections::HashMap::new();
                    map.insert(tower_lsp::lsp_types::Url::parse(&uri).unwrap(), changes);

                    return Ok(Some(WorkspaceEdit {
                        changes: Some(map),
                        ..Default::default()
                    }));
                }
            }
        }

        Ok(None)
    }
}

fn create_location_response(
    source_file: &Option<PathBuf>,
    line: Option<usize>,
    column: Option<usize>,
) -> Result<Option<GotoDefinitionResponse>> {
    let source_file = match source_file.as_ref() {
        Some(f) => f,
        None => return Ok(None),
    };

    let line = line.unwrap_or(1).saturating_sub(1) as u32;
    let column = column.unwrap_or(0) as u32;

    let uri = match tower_lsp::lsp_types::Url::from_file_path(source_file) {
        Ok(u) => u,
        Err(_) => return Ok(None),
    };

    Ok(Some(GotoDefinitionResponse::Scalar(Location {
        uri,
        range: Range::new(Position::new(line, column), Position::new(line, column)),
    })))
}

fn get_word_at_position(content: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = content.lines().collect();

    if position.line as usize >= lines.len() {
        return None;
    }

    let line = lines[position.line as usize];
    let col = position.character as usize;

    if col > line.len() {
        return None;
    }

    // Check if we're currently on a non-word character
    // If so, look backward to find a word
    let actual_col = if col > 0 && col <= line.len() {
        let ch = line.chars().nth(col.saturating_sub(1));
        if matches!(ch, Some(c) if !c.is_alphanumeric() && c != '_') {
            // We're on punctuation, look backward for a word
            col.saturating_sub(1)
        } else {
            col
        }
    } else {
        col
    };

    // Find word boundaries around actual_col
    let start = line[..actual_col]
        .rfind(|c: char| !c.is_alphanumeric() && c != '_')
        .map(|i| i + 1)
        .unwrap_or(0);

    let end = line[actual_col..]
        .find(|c: char| !c.is_alphanumeric() && c != '_')
        .map(|i| actual_col + i)
        .unwrap_or(line.len());

    if start < end {
        let word = line[start..end].to_string();
        // Only return if it's a valid identifier (starts with letter or underscore, not a number)
        if word
            .chars()
            .next()
            .map(|c| c.is_alphabetic() || c == '_')
            .unwrap_or(false)
        {
            return Some(word);
        }
    }

    None
}

impl Backend {
    async fn publish_diagnostics(&self, uri: &str, content: &str, type_annotation: Option<&str>) {
        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "Publishing diagnostics for {} with type annotation: {:?}",
                    uri, type_annotation
                ),
            )
            .await;

        let diagnostics = if let Some(type_path) = type_annotation {
            if let Some(type_info) = self.rust_analyzer.get_type_info(type_path).await {
                self.client
                    .log_message(
                        MessageType::INFO,
                        format!("Found type info for {}: {:?}", type_path, type_info.name),
                    )
                    .await;
                let diags = diagnostics::validate_ron_with_analyzer(
                    content,
                    &type_info,
                    self.rust_analyzer.clone(),
                )
                .await;
                self.client
                    .log_message(
                        MessageType::INFO,
                        format!("Generated {} diagnostics", diags.len()),
                    )
                    .await;
                diags
            } else {
                self.client
                    .log_message(
                        MessageType::WARNING,
                        format!("Could not find type: {}", type_path),
                    )
                    .await;
                vec![Diagnostic {
                    range: Range::new(Position::new(0, 0), Position::new(0, 1)),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: format!("Could not find type: {}", type_path),
                    ..Default::default()
                }]
            }
        } else {
            self.client
                .log_message(MessageType::INFO, "No type annotation found")
                .await;
            vec![]
        };

        self.client
            .log_message(
                MessageType::INFO,
                format!("Publishing {} diagnostics", diagnostics.len()),
            )
            .await;
        self.client
            .publish_diagnostics(uri.parse().unwrap(), diagnostics, None)
            .await;
    }
}

#[cfg(feature = "cli")]
use clap::{Parser, Subcommand};

#[cfg(feature = "cli")]
#[derive(Parser)]
#[command(name = "ron-lsp")]
#[command(about = "LSP server and validator for RON files", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[cfg(feature = "cli")]
#[derive(Subcommand)]
enum Commands {
    /// Check RON files in a directory for errors
    Check {
        /// Directory to check (defaults to current directory)
        #[arg(default_value = ".")]
        directory: PathBuf,
    },
}

#[tokio::main]
async fn main() {
    #[cfg(feature = "cli")]
    {
        let cli = Cli::parse();

        match cli.command {
            Some(Commands::Check { directory }) => {
                run_check(directory).await;
                return;
            }
            None => {
                // Fall through to LSP server
            }
        }
    }

    // Run as LSP server
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(feature = "cli")]
async fn run_check(path: PathBuf) {
    use std::fs;
    use walkdir::WalkDir;

    // Check if path is a file or directory
    let is_file = path.is_file();

    // Find workspace root by looking for Cargo.toml
    let workspace_root = {
        let start_dir = if is_file {
            path.parent().unwrap_or_else(|| std::path::Path::new("."))
        } else {
            &path
        };

        let mut current = start_dir;
        loop {
            if current.join("Cargo.toml").exists() {
                break current;
            }
            match current.parent() {
                Some(parent) => current = parent,
                None => break std::path::Path::new("."),
            }
        }
    };

    eprintln!(
        "Checking RON files in: {:?}",
        path.canonicalize().unwrap_or(path.clone())
    );

    // Initialize the Rust analyzer with workspace root
    let analyzer = Arc::new(rust_analyzer::RustAnalyzer::new());
    analyzer.set_workspace_root(workspace_root).await;

    let types = analyzer.get_all_types().await;
    eprintln!("Found {} types in workspace", types.len());

    let mut total_files = 0;
    let mut files_with_errors = 0;
    let mut total_errors = 0;

    // Collect files to check
    let files_to_check: Vec<PathBuf> = if is_file {
        vec![path.clone()]
    } else {
        WalkDir::new(&path)
            .follow_links(true)
            .into_iter()
            .filter_map(|e| e.ok())
            .map(|e| e.path().to_path_buf())
            .filter(|p| p.extension().and_then(|s| s.to_str()) == Some("ron"))
            .collect()
    };

    // Process each file
    for file_path in files_to_check {
        total_files += 1;

        // Read the file
        let content = match fs::read_to_string(&file_path) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("Error reading {}: {}", file_path.display(), e);
                continue;
            }
        };

        // Parse type annotation
        let type_annotation = annotation_parser::parse_type_annotation(&content);

        if let Some(type_path) = type_annotation {
            if let Some(type_info) = analyzer.get_type_info(&type_path).await {
                // Validate the file
                let diagnostics =
                    diagnostics::validate_ron_portable(&content, &type_info, analyzer.clone())
                        .await;

                if !diagnostics.is_empty() {
                    files_with_errors += 1;
                    total_errors += diagnostics.len();

                    // Report diagnostics using ariadne
                    for diagnostic in diagnostics {
                        diagnostic.report_ariadne(&file_path, &content);
                    }
                }
            } else {
                files_with_errors += 1;
                total_errors += 1;
                eprintln!(
                    "\n{}: Could not find type: {}\n",
                    file_path.display(),
                    type_path
                );
            }
        }
        // Files without type annotations are skipped silently
    }

    eprintln!("\nChecked {} RON files", total_files);
    if total_errors > 0 {
        eprintln!(
            "{} files with errors ({} total errors)",
            files_with_errors, total_errors
        );
        std::process::exit(1);
    } else {
        eprintln!("All files valid!");
    }
}

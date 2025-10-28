mod annotation_parser;
mod code_actions;
mod completion;
mod diagnostic_reporter;
mod diagnostics;
mod format;
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
    // Cache context lookups - map from (line, character) to type contexts
    // We use a simple cache that stores recent lookups
    context_cache: std::collections::HashMap<(u32, u32), Vec<ron_parser::TypeContext>>,
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

    /// Get type contexts with caching
    async fn get_type_contexts(&self, uri: &str, position: Position, content: &str) -> Vec<ron_parser::TypeContext> {
        let pos_key = (position.line, position.character);

        // Try to get from cache first
        {
            let documents = self.documents.read().await;
            if let Some(doc) = documents.get(uri) {
                if let Some(cached) = doc.context_cache.get(&pos_key) {
                    return cached.clone();
                }
            }
        }

        // Not in cache, compute it
        let contexts = ron_parser::find_type_context_at_position(content, position);

        // Store in cache
        {
            let mut documents = self.documents.write().await;
            if let Some(doc) = documents.get_mut(uri) {
                doc.context_cache.insert(pos_key, contexts.clone());
            }
        }

        contexts
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
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_range_formatting_provider: Some(OneOf::Left(true)),
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
                context_cache: std::collections::HashMap::new(),
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
                    context_cache: std::collections::HashMap::new(),
                },
            );

            // Publish diagnostics
            self.publish_diagnostics(&uri, &content, type_annotation.as_deref())
                .await;
        }
    }

    async fn did_save(&self, _params: DidSaveTextDocumentParams) {
        // No-op for now
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        self.documents.write().await.remove(&uri);
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

        // Get document data we need (clone to avoid holding lock during async operations)
        let (content, type_path) = {
            let documents = self.documents.read().await;
            match documents.get(&uri) {
                Some(doc) => (doc.content.clone(), doc.type_annotation.clone()),
                None => return Ok(None),
            }
        };

        // Get the word at cursor position - early return if none
        let word = match get_word_at_position(&content, position) {
            Some(w) => w,
            None => {
                self.client.log_message(MessageType::INFO, "No word at position").await;
                return Ok(None);
            }
        };

        self.client.log_message(MessageType::INFO, format!("Word at position: {}", word)).await;

        // If we have a type annotation, find the nested context
        let current_type_info = if let Some(type_path) = type_path {
            self.client.log_message(MessageType::INFO, format!("Type annotation: {}", type_path)).await;
            // Use cached context lookup
            let contexts = self.get_type_contexts(&uri, position, &content).await;
            self.client.log_message(MessageType::INFO, format!("Found {} type contexts", contexts.len())).await;

            // Use shared navigation helper
            let info = self.navigate_to_innermost_type(&type_path, &contexts).await;
            self.client.log_message(MessageType::INFO, format!("Final type info: {:?}", info.as_ref().map(|i| &i.name))).await;
            info
        } else {
            self.client.log_message(MessageType::INFO, "No type annotation found").await;
            None
        };

        if current_type_info.is_none() {
            self.client.log_message(MessageType::INFO, "current_type_info is None!").await;
        }

        // Check if the word is a valid field name in the current context type
        if let Some(ref info) = current_type_info {
            self.client.log_message(MessageType::INFO, format!("current_type_info: {}", info.name)).await;

            // First check if we're inside an enum variant
            if let Some(variant_name) = ron_parser::find_current_variant_context(&content, position) {
                self.client.log_message(MessageType::INFO, format!("Found variant context: {}", variant_name)).await;

                // We might be in a variant of the current type OR a variant of a field's type
                // Try the current type first
                if let Some(variant) = info.find_variant(&variant_name) {
                    self.client.log_message(MessageType::INFO, format!("Found variant in current type: {}", variant_name)).await;
                    // Check if word is a field of this variant
                    if let Some(field) = variant.fields.iter().find(|f| f.name == word) {
                        return create_location_response(&info.source_file, field.line, field.column);
                    }
                } else {
                    self.client.log_message(MessageType::INFO, format!("Variant {} not found in type {}", variant_name, info.name)).await;
                }

                // If not found, check if we're in a field that contains a variant
                // e.g., we're on "length" inside "post_type: Detailed(length: 1)"
                if let Some(field_name) = ron_parser::get_containing_field_context(&content, position) {
                    self.client.log_message(MessageType::INFO, format!("Found containing field: {}", field_name)).await;

                    if let Some(field) = info.find_field(&field_name) {
                        self.client.log_message(MessageType::INFO, format!("Found field {} with type {}", field_name, field.type_name)).await;

                        // Get the type of this field (e.g., PostType)
                        if let Some(field_type_info) = self.rust_analyzer.get_type_info(&field.type_name).await {
                            self.client.log_message(MessageType::INFO, format!("Found type info for {}", field_type_info.name)).await;

                            // Check if this field's type has the variant we're in
                            if let Some(variant) = field_type_info.find_variant(&variant_name) {
                                self.client.log_message(MessageType::INFO, format!("Found variant {} in type {}", variant_name, field_type_info.name)).await;

                                // Check if word is a field of this variant
                                if let Some(variant_field) = variant.fields.iter().find(|f| f.name == word) {
                                    self.client.log_message(MessageType::INFO, format!("Found field {} in variant!", word)).await;
                                    return create_location_response(&field_type_info.source_file, variant_field.line, variant_field.column);
                                } else {
                                    self.client.log_message(MessageType::INFO, format!("Field {} not found in variant fields", word)).await;
                                }
                            } else {
                                self.client.log_message(MessageType::INFO, format!("Variant {} not found in field type {}", variant_name, field_type_info.name)).await;
                            }
                        } else {
                            self.client.log_message(MessageType::INFO, format!("Could not get type info for {}", field.type_name)).await;
                        }
                    } else {
                        self.client.log_message(MessageType::INFO, format!("Field {} not found in type {}", field_name, info.name)).await;
                    }
                } else {
                    self.client.log_message(MessageType::INFO, "No containing field context found").await;
                }
            } else {
                self.client.log_message(MessageType::INFO, "No variant context found").await;
            }

            // Check if the word is a struct field name
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
            if let Some(field_name) = ron_parser::get_field_at_position(&content, position) {
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

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        // Get document data we need (clone to avoid holding lock during async operations)
        let (content, type_path) = {
            let documents = self.documents.read().await;
            match documents.get(&uri) {
                Some(doc) => (doc.content.clone(), doc.type_annotation.clone()),
                None => return Ok(None),
            }
        };

        // Get the word at cursor position
        let word = match get_word_at_position(&content, position) {
            Some(w) => w,
            None => return Ok(None),
        };

        if let Some(type_path) = type_path {
            // Use cached context lookup
            let contexts = self.get_type_contexts(&uri, position, &content).await;

            // Use shared navigation helper
            let current_type_info = self.navigate_to_innermost_type(&type_path, &contexts).await;

            // Now check what the word at cursor is
            if let Some(type_info) = current_type_info {
                // Case 1: Hovering over a field name
                if let Some(field_name) = ron_parser::get_field_at_position(&content, position) {
                    if field_name == word {
                        // First check if we're in an enum variant's fields
                        if let Some(variant_name) = ron_parser::find_current_variant_context(&content, position) {
                            if let Some(variant) = type_info.find_variant(&variant_name) {
                                if let Some(field) = variant.fields.iter().find(|f| f.name == field_name) {
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

                        // Otherwise check struct fields
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

                // Case 2: Hovering over a variant name
                if let Some(variant) = type_info.find_variant(&word) {
                    let mut hover_text = format!("```rust\nenum {}\n```\n\n**Variant:** `{}`",
                        type_info.name.split("::").last().unwrap_or(&type_info.name),
                        variant.name
                    );

                    if let Some(ref docs) = variant.docs {
                        hover_text.push_str(&format!("\n\n{}", docs));
                    }

                    if !variant.fields.is_empty() {
                        hover_text.push_str("\n\n**Fields:**\n");
                        for field in &variant.fields {
                            hover_text.push_str(&format!("- `{}`: `{}`", field.name, field.type_name));
                            if let Some(ref field_docs) = field.docs {
                                hover_text.push_str(&format!(" - {}", field_docs));
                            }
                            hover_text.push('\n');
                        }
                    }

                    return Ok(Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: hover_text,
                        }),
                        range: None,
                    }));
                }

                // Case 3: Hovering over the type name itself (or check if word matches the current type)
                if type_info.name.ends_with(&format!("::{}", word)) || type_info.name == word {
                    return Ok(Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: self.format_type_hover(&type_info),
                        }),
                        range: None,
                    }));
                }

                // Case 4: Check if hovering over a field value that's a variant (like "Short" in "post_type: Short")
                if let Some(field_name) = ron_parser::get_field_at_position(&content, position) {
                    if let Some(field) = type_info.find_field(&field_name) {
                        // Get the type of this field
                        if let Some(field_type_info) = self.rust_analyzer.get_type_info(&field.type_name).await {
                            // Check if word is a variant of this field's type
                            if let Some(variant) = field_type_info.find_variant(&word) {
                                let mut hover_text = format!("```rust\nenum {}\n```\n\n**Variant:** `{}`",
                                    field_type_info.name.split("::").last().unwrap_or(&field_type_info.name),
                                    variant.name
                                );

                                if let Some(ref docs) = variant.docs {
                                    hover_text.push_str(&format!("\n\n{}", docs));
                                }

                                if !variant.fields.is_empty() {
                                    hover_text.push_str("\n\n**Fields:**\n");
                                    for vfield in &variant.fields {
                                        hover_text.push_str(&format!("- `{}`: `{}`", vfield.name, vfield.type_name));
                                        if let Some(ref vfield_docs) = vfield.docs {
                                            hover_text.push_str(&format!(" - {}", vfield_docs));
                                        }
                                        hover_text.push('\n');
                                    }
                                }

                                return Ok(Some(Hover {
                                    contents: HoverContents::Markup(MarkupContent {
                                        kind: MarkupKind::Markdown,
                                        value: hover_text,
                                    }),
                                    range: None,
                                }));
                            }
                        }
                    }
                }
            }
        }

        // Case 5: No type annotation - try to find type by name
        if let Some(info) = self.rust_analyzer.get_type_info(&word).await {
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: self.format_type_hover(&info),
                }),
                range: None,
            }));
        }

        Ok(None)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let position = params.text_document_position.position;

        let (content, type_path) = {
            let documents = self.documents.read().await;
            match documents.get(&uri) {
                Some(doc) => (doc.content.clone(), doc.type_annotation.clone()),
                None => return Ok(None),
            }
        };

        if let Some(type_path) = type_path {
            // Get type contexts
            let contexts = self.get_type_contexts(&uri, position, &content).await;

            // Navigate to innermost type using shared helper
            let current_type_info = self.navigate_to_innermost_type(&type_path, &contexts).await;

            if let Some(type_info) = current_type_info {
                let completions = completion::generate_completions_for_type(
                    &content,
                    position,
                    &type_info,
                    self.rust_analyzer.clone(),
                )
                .await;
                return Ok(Some(CompletionResponse::Array(completions)));
            }
        }

        Ok(None)
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = params.text_document.uri.to_string();

        self.client.log_message(MessageType::INFO, format!("Code action requested for: {}", uri)).await;

        let (content, type_path) = {
            let documents = self.documents.read().await;
            match documents.get(&uri) {
                Some(doc) => (doc.content.clone(), doc.type_annotation.clone()),
                None => {
                    self.client.log_message(MessageType::INFO, "No document found").await;
                    return Ok(None);
                }
            }
        };

        if let Some(type_path) = type_path {
            self.client.log_message(MessageType::INFO, format!("Type annotation: {}", type_path)).await;
            if let Some(type_info) = self.rust_analyzer.get_type_info(&type_path).await {
                self.client.log_message(MessageType::INFO, format!("Type info kind: {:?}", type_info.kind)).await;
                let actions = code_actions::generate_code_actions(
                    &content,
                    &type_info,
                    &uri,
                    self.rust_analyzer.clone(),
                    &self.client,
                )
                .await;

                self.client.log_message(MessageType::INFO, format!("Generated {} code actions", actions.len())).await;
                if !actions.is_empty() {
                    return Ok(Some(actions));
                }
            } else {
                self.client.log_message(MessageType::INFO, "Could not get type info").await;
            }
        } else {
            self.client.log_message(MessageType::INFO, "No type annotation").await;
        }

        Ok(None)
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri.to_string();

        let documents = self.documents.read().await;
        if let Some(doc) = documents.get(&uri) {
            // Basic RON formatting: normalize whitespace and indentation
            let formatted = Backend::format_ron(&doc.content);

            if formatted != doc.content {
                return Ok(Some(vec![TextEdit {
                    range: Range::new(Position::new(0, 0), Position::new(u32::MAX, u32::MAX)),
                    new_text: formatted,
                }]));
            }
        }

        Ok(None)
    }

    async fn range_formatting(
        &self,
        params: DocumentRangeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri.to_string();
        let range = params.range;

        let documents = self.documents.read().await;
        if let Some(doc) = documents.get(&uri) {
            // Extract the text in the range
            let lines: Vec<&str> = doc.content.lines().collect();
            let start_line = range.start.line as usize;
            let end_line = range.end.line as usize;

            if start_line < lines.len() && end_line < lines.len() {
                // Get the selected text
                let mut selected_text = String::new();
                for (i, line) in lines
                    .iter()
                    .enumerate()
                    .skip(start_line)
                    .take(end_line - start_line + 1)
                {
                    if i == start_line && i == end_line {
                        // Single line selection
                        let start_char = range.start.character as usize;
                        let end_char = range.end.character as usize;
                        selected_text
                            .push_str(&line[start_char.min(line.len())..end_char.min(line.len())]);
                    } else if i == start_line {
                        let start_char = range.start.character as usize;
                        selected_text.push_str(&line[start_char.min(line.len())..]);
                        selected_text.push('\n');
                    } else if i == end_line {
                        let end_char = range.end.character as usize;
                        selected_text.push_str(&line[..end_char.min(line.len())]);
                    } else {
                        selected_text.push_str(line);
                        selected_text.push('\n');
                    }
                }

                // Format the selected text
                let formatted = Backend::format_ron(&selected_text);

                if formatted != selected_text {
                    return Ok(Some(vec![TextEdit {
                        range,
                        new_text: formatted,
                    }]));
                }
            }
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
    fn format_ron(content: &str) -> String {
        format::format_ron(content)
    }

    /// Navigate through nested type contexts to find the innermost type
    /// This is used by goto_definition, hover, and completion
    async fn navigate_to_innermost_type(
        &self,
        top_level_type_path: &str,
        contexts: &[ron_parser::TypeContext],
    ) -> Option<rust_analyzer::TypeInfo> {
        // Start with the top-level type
        let mut current_type_info = self.rust_analyzer.get_type_info(top_level_type_path).await;

        // Navigate through the nested contexts (skip first since it's the top-level type we already have)
        for context in contexts.iter().skip(1) {
            // Clone the current type info to avoid borrow checker issues
            let info = match current_type_info {
                Some(ref info) => info.clone(),
                None => break,
            };

            // Try to find the context type as a field's type
            // Use exact match on the last component of the type name to avoid substring matches
            // (e.g., don't match "Post" with "PostType")
            if let Some(fields) = info.fields() {
                let context_name = &context.type_name;
                if let Some(field) = fields.iter().find(|f| {
                    let field_type_last = f.type_name.split("::").last().unwrap_or(&f.type_name);
                    // Remove generic parameters for comparison
                    let field_type_base = field_type_last.split('<').next().unwrap_or(field_type_last);
                    field_type_base == context_name
                }) {
                    current_type_info = self.rust_analyzer.get_type_info(&field.type_name).await;
                    continue;
                }
            }

            // Try as direct type lookup
            let direct_lookup = self.rust_analyzer.get_type_info(&context.type_name).await;
            if direct_lookup.is_some() {
                current_type_info = direct_lookup;
            } else {
                // The context name might be a variant name
                let mut found_via_variant = false;

                // Check if current type is an enum with this variant
                if let Some(variant) = info.find_variant(&context.type_name) {
                    // For tuple variants with one field, navigate to that field's type
                    if variant.fields.len() == 1 {
                        let field_type = &variant.fields[0].type_name;
                        current_type_info = self.rust_analyzer.get_type_info(field_type).await;
                        found_via_variant = true;
                    }
                }

                // If not found as a variant of the current type, try to find it in field types
                if !found_via_variant {
                    if let Some(fields) = info.fields() {
                        for field in fields {
                            if let Some(field_type_info) = self.rust_analyzer.get_type_info(&field.type_name).await {
                                // Check if this field's type has a variant with the context name
                                if field_type_info.find_variant(&context.type_name).is_some() {
                                    current_type_info = Some(field_type_info);
                                    found_via_variant = true;
                                    break;
                                }
                            }
                        }
                    }
                }

                if !found_via_variant {
                    current_type_info = None;
                }
            }
        }

        current_type_info
    }

    fn format_type_hover(&self, type_info: &rust_analyzer::TypeInfo) -> String {
        use crate::rust_analyzer::TypeKind;

        let type_name = type_info.name.split("::").last().unwrap_or(&type_info.name);
        let mut hover_text = String::new();

        match &type_info.kind {
            TypeKind::Struct(fields) => {
                hover_text.push_str(&format!("```rust\nstruct {}\n```", type_name));

                if let Some(ref docs) = type_info.docs {
                    hover_text.push_str(&format!("\n\n{}", docs));
                }

                if !fields.is_empty() {
                    hover_text.push_str("\n\n**Fields:**\n");
                    for field in fields {
                        hover_text.push_str(&format!("- `{}`: `{}`", field.name, field.type_name));
                        if let Some(ref field_docs) = field.docs {
                            hover_text.push_str(&format!(" - {}", field_docs));
                        }
                        hover_text.push('\n');
                    }
                }
            }
            TypeKind::Enum(variants) => {
                hover_text.push_str(&format!("```rust\nenum {}\n```", type_name));

                if let Some(ref docs) = type_info.docs {
                    hover_text.push_str(&format!("\n\n{}", docs));
                }

                if !variants.is_empty() {
                    hover_text.push_str("\n\n**Variants:**\n");
                    for variant in variants {
                        if variant.fields.is_empty() {
                            hover_text.push_str(&format!("- `{}`", variant.name));
                        } else {
                            hover_text.push_str(&format!("- `{}` with fields:", variant.name));
                            for field in &variant.fields {
                                hover_text.push_str(&format!("\n  - `{}`: `{}`", field.name, field.type_name));
                            }
                        }
                        if let Some(ref variant_docs) = variant.docs {
                            hover_text.push_str(&format!(" - {}", variant_docs));
                        }
                        hover_text.push('\n');
                    }
                }
            }
        }

        hover_text
    }

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


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_ron_named_struct() {
        let input = r#"User(
    id: 1,
    name: "Alice",
    age: 28
)"#;
        let formatted = Backend::format_ron(input);

        // Should be able to parse the result
        let parsed = ron::from_str::<ron::Value>(&formatted);
        assert!(parsed.is_ok(), "Formatted RON should be parseable. Got: {}", formatted);

        // Original should also be parseable
        let original_parsed: ron::Value = ron::from_str(input).expect("Original should parse");
        let formatted_parsed: ron::Value = ron::from_str(&formatted).expect("Formatted should parse");

        // Both should represent the same value
        assert_eq!(original_parsed, formatted_parsed, "Formatted RON should preserve value");
    }

    #[test]
    fn test_format_ron_with_annotation() {
        let input = r#"/* @[crate::models::User] */

User(
    id: 1,
    name: "Alice"
)"#;
        let formatted = Backend::format_ron(input);

        // Should preserve annotation
        assert!(formatted.contains("/* @[crate::models::User] */"),
                "Should preserve type annotation. Got: {}", formatted);

        // Extract RON part (after annotation)
        let ron_part = formatted.split("*/").nth(1).unwrap().trim();

        // Should be parseable
        let parsed = ron::from_str::<ron::Value>(ron_part);
        assert!(parsed.is_ok(),
                "Formatted RON (without annotation) should be parseable. Got: {}\nError: {:?}",
                ron_part, parsed.as_ref().err());
    }

    #[test]
    fn test_format_ron_unnamed_struct() {
        let input = r#"(
    id: 1,
    name: "Alice",
    roles: ["admin", "user"]
)"#;
        let formatted = Backend::format_ron(input);

        // Should be able to parse both
        let original_parsed: ron::Value = ron::from_str(input).expect("Original should parse");
        let formatted_parsed = ron::from_str::<ron::Value>(&formatted);

        assert!(formatted_parsed.is_ok(),
                "Formatted RON should be parseable.\nInput:\n{}\n\nFormatted:\n{}\n\nError: {:?}",
                input, formatted, formatted_parsed.as_ref().err());

        let formatted_parsed = formatted_parsed.unwrap();
        assert_eq!(original_parsed, formatted_parsed,
                   "Formatted RON should preserve value.\nOriginal: {:?}\nFormatted: {:?}",
                   original_parsed, formatted_parsed);
    }

    #[test]
    fn test_format_ron_real_example() {
        // This is actual RON syntax from user.ron
        let input = r#"/* @[crate::models::User] */

User(
    id: 1,
    name: "Alice Johnson",
    email: "alice@example.com",
    age: 28,
    bio: Some("Full-stack developer passionate about Rust and web technologies."),
    is_active: true,
    roles: ["admin", "developer"],
)"#;

        let formatted = Backend::format_ron(input);

        // Should preserve annotation
        assert!(formatted.contains("/* @[crate::models::User] */"),
                "Should preserve type annotation");

        // Extract RON part (after annotation)
        let ron_part = formatted.split("*/").nth(1).unwrap().trim();
        let original_ron_part = input.split("*/").nth(1).unwrap().trim();

        // Both should parse
        let original_parsed: ron::Value = ron::from_str(original_ron_part)
            .expect("Original should parse");
        let formatted_parsed = ron::from_str::<ron::Value>(ron_part);

        assert!(formatted_parsed.is_ok(),
                "Formatted RON should be parseable.\n\nOriginal RON:\n{}\n\nFormatted RON:\n{}\n\nError: {:?}",
                original_ron_part, ron_part, formatted_parsed.as_ref().err());

        let formatted_parsed = formatted_parsed.unwrap();
        assert_eq!(original_parsed, formatted_parsed,
                   "Formatted RON should preserve value.\nOriginal: {:?}\nFormatted: {:?}",
                   original_parsed, formatted_parsed);
    }

    #[test]
    fn test_format_ron_outputs_valid_ron() {
        // Test with the actual mixed_syntax.ron content
        let input = r#"/* @[crate::models::Post] */

// This demonstrates mixing explicit and unnamed struct syntax
// throughout the document

Post(
    id: 123,
    title: "Mixed Syntax Example",
    content: "Demonstrating both explicit and unnamed struct syntax",

    // Explicit type name for author
    author: (
        id: 5,
        name: "Charlie",
        email: "charlie@example.com",
        age: 28,
        bio: None,
        is_active: true,
        roles: ["editor"],
    ),

    likes: 50,
    tags: ["example", "syntax"],
    published: true,
    post_type: Short,
)"#;

        let formatted = Backend::format_ron(input);

        println!("===== FORMATTED OUTPUT =====");
        println!("{}", formatted);
        println!("===== END =====");

        // Extract RON part
        let ron_part = if formatted.contains("*/") {
            formatted.split("*/").nth(1).unwrap().trim()
        } else {
            formatted.trim()
        };

        // The formatted output MUST be valid RON
        let parse_result = ron::from_str::<ron::Value>(ron_part);
        assert!(parse_result.is_ok(),
                "Formatted output must be valid RON.\n\nFormatted:\n{}\n\nError: {:?}",
                ron_part, parse_result.err());

        // And it should NOT look like JSON (no leading braces for maps)
        assert!(!ron_part.trim().starts_with('{'),
                "Formatted RON should not start with '{{' (that's JSON syntax). Got:\n{}",
                ron_part);
    }

    #[test]
    fn test_find_variant_context_without_type_prefix() {
        // Test finding variant when type prefix isn't present (common case)
        let content = r#"Detailed(
            length: 1,
        )"#;
        let position = Position::new(1, 12); // On "length" line

        let variant = ron_parser::find_current_variant_context(content, position);
        assert_eq!(variant, Some("Detailed".to_string()), "Should detect Detailed variant");
    }

    #[test]
    fn test_find_current_variant_context_with_full_syntax() {
        // Test the explicit TypeName::VariantName syntax
        let content = r#"PostType::Detailed(
            length: 1,
        )"#;
        let position = Position::new(1, 20); // On "length" line, after colon

        let variant_name = ron_parser::find_current_variant_context(content, position);
        assert!(variant_name.is_some(), "Should detect enum variant context with :: syntax");
        assert_eq!(variant_name.unwrap(), "Detailed", "Should detect Detailed variant");
    }

    #[test]
    fn test_get_word_at_position_on_field() {
        let content = "    length: 1,";
        let position = Position::new(0, 8); // On "length"

        let word = get_word_at_position(content, position);
        assert_eq!(word, Some("length".to_string()));
    }

    #[test]
    fn test_enum_variant_field_detection_full_scenario() {
        // The ACTUAL content from enum_variants.ron
        let content = r#"/* @[crate::models::Message] */
// Complex nested enum variant with Post containing User
PostReference(
    Post(
        id: 42,
        title: "Enum Variants in RON",
        content: "Shows tuple variants, struct variants, and nesting",
        author: User(
            id: 1,
            name: "Alice",
            email: "alice@example.com",
            age: 30,
            bio: Some(
                "Rust developer",
            ),
            is_active: true,
            roles: [
                "admin",
            ],
            invalid_field: "should error",
        ),
        likes: 100,
        tags: [
            "rust",
            "ron",
        ],
        published: true,
        post_type: Detailed(
            length: 1,
        ),
    )
)"#;

        // Position on "length" - line 28 (0-indexed), character 16
        let position = Position::new(28, 16);

        // Test what we detect
        let word = get_word_at_position(content, position);
        println!("Word at position: {:?}", word);
        assert_eq!(word, Some("length".to_string()), "Should detect 'length' word");

        let variant = ron_parser::find_current_variant_context(content, position);
        println!("Variant context: {:?}", variant);
        assert_eq!(variant, Some("Detailed".to_string()), "Should detect Detailed variant");

        let containing_field = ron_parser::get_containing_field_context(content, position);
        println!("Containing field: {:?}", containing_field);
        assert_eq!(containing_field, Some("post_type".to_string()), "Should detect post_type field");

        // This test shows that all the building blocks work!
        // The problem must be in how goto_definition uses them
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
                let mut diagnostics =
                    diagnostics::validate_ron_portable(&content, &type_info, analyzer.clone())
                        .await;

                if !diagnostics.is_empty() {
                    files_with_errors += 1;
                    total_errors += diagnostics.len();

                    // Sort diagnostics by line and column (first to last)
                    diagnostics.sort();

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

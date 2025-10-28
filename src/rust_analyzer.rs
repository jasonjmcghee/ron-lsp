use std::collections::HashMap;
use std::fs;
use std::path::{Component, Path, PathBuf};
use syn::{Attribute, Fields, Item, ItemEnum, ItemStruct, ItemType, Type};
use tokio::sync::RwLock;
use walkdir::WalkDir;

#[derive(Debug, Clone)]
pub struct FieldInfo {
    pub name: String,
    pub type_name: String,
    pub docs: Option<String>,
    pub line: Option<usize>,
    pub column: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Vec<FieldInfo>,
    pub docs: Option<String>,
    pub line: Option<usize>,
    pub column: Option<usize>,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Struct(Vec<FieldInfo>),
    Enum(Vec<EnumVariant>),
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub name: String,
    pub kind: TypeKind,
    pub docs: Option<String>,
    pub source_file: Option<PathBuf>,
    pub line: Option<usize>,
    pub column: Option<usize>,
    pub has_default: bool,
}

impl TypeInfo {
    pub fn fields(&self) -> Option<&Vec<FieldInfo>> {
        match &self.kind {
            TypeKind::Struct(fields) => Some(fields),
            TypeKind::Enum(_) => None,
        }
    }

    pub fn find_field(&self, field_name: &str) -> Option<&FieldInfo> {
        match &self.kind {
            TypeKind::Struct(fields) => fields.iter().find(|f| f.name == field_name),
            TypeKind::Enum(_) => None,
        }
    }

    pub fn find_variant(&self, variant_name: &str) -> Option<&EnumVariant> {
        match &self.kind {
            TypeKind::Enum(variants) => variants.iter().find(|v| v.name == variant_name),
            TypeKind::Struct(_) => None,
        }
    }
}

pub struct RustAnalyzer {
    workspace_root: RwLock<Option<PathBuf>>,
    type_cache: RwLock<HashMap<String, TypeInfo>>,
    type_aliases: RwLock<HashMap<String, String>>,
}

impl RustAnalyzer {
    pub fn new() -> Self {
        Self {
            workspace_root: RwLock::new(None),
            type_cache: RwLock::new(HashMap::new()),
            type_aliases: RwLock::new(HashMap::new()),
        }
    }

    pub async fn set_workspace_root(&self, root: &Path) {
        *self.workspace_root.write().await = Some(root.to_path_buf());
        // Trigger initial scan
        self.scan_workspace().await;
    }

    async fn scan_workspace(&self) {
        let root = {
            let guard = self.workspace_root.read().await;
            match guard.as_ref() {
                Some(r) => r.clone(),
                None => return,
            }
        };

        let mut type_cache = self.type_cache.write().await;
        let mut type_aliases = self.type_aliases.write().await;

        // Find all .rs files in the workspace
        for entry in WalkDir::new(&root)
            .follow_links(true)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().and_then(|s| s.to_str()) == Some("rs"))
        {
            if let Ok(content) = fs::read_to_string(entry.path()) {
                if let Ok(syntax_tree) = syn::parse_file(&content) {
                    self.extract_types_from_file(
                        &syntax_tree,
                        entry.path(),
                        &mut type_cache,
                        &mut type_aliases,
                    );
                }
            }
        }
    }

    fn extract_types_from_file(
        &self,
        syntax_tree: &syn::File,
        file_path: &Path,
        type_cache: &mut HashMap<String, TypeInfo>,
        type_aliases: &mut HashMap<String, String>,
    ) {
        // Extract module path from file path
        let module_prefix = self.file_path_to_module_path(file_path);

        for item in &syntax_tree.items {
            if let Item::Struct(struct_item) = item {
                if let Some(type_info) =
                    self.extract_struct_info(struct_item, &module_prefix, file_path)
                {
                    type_cache.insert(type_info.name.clone(), type_info);
                }
            } else if let Item::Enum(enum_item) = item {
                if let Some(type_info) =
                    self.extract_enum_info(enum_item, &module_prefix, file_path)
                {
                    type_cache.insert(type_info.name.clone(), type_info);
                }
            } else if let Item::Type(type_item) = item {
                self.extract_type_alias(type_item, &module_prefix, type_aliases);
            } else if let Item::Mod(mod_item) = item {
                // Handle inline modules
                if let Some((_, items)) = &mod_item.content {
                    let mod_name = mod_item.ident.to_string();
                    let nested_prefix = if module_prefix.is_empty() {
                        mod_name
                    } else {
                        format!("{}::{}", module_prefix, mod_name)
                    };

                    for item in items {
                        if let Item::Struct(struct_item) = item {
                            if let Some(type_info) =
                                self.extract_struct_info(struct_item, &nested_prefix, file_path)
                            {
                                type_cache.insert(type_info.name.clone(), type_info);
                            }
                        } else if let Item::Enum(enum_item) = item {
                            if let Some(type_info) =
                                self.extract_enum_info(enum_item, &nested_prefix, file_path)
                            {
                                type_cache.insert(type_info.name.clone(), type_info);
                            }
                        } else if let Item::Type(type_item) = item {
                            self.extract_type_alias(type_item, &nested_prefix, type_aliases);
                        }
                    }
                }
            }
        }
    }


    /// Convert a file path to a module path
    ///
    /// # Examples
    /// ```
    /// let module_path = self.file_file_path_to_module_path("src/models/user.rs");
    /// assert_eq!(module_path, "crate::models::user");
    /// ```
    fn file_path_to_module_path(&self, file_path: &Path) -> String {
        let components: Vec<_> = file_path
            .components()
            .filter_map(|c| match c {
                Component::Normal(os) => os.to_str(),
                _ => None,
            })
            .collect();

        if let Some(src_index) = components.iter().position(|c| *c == "src") {
            let relative = &components[src_index + 1..];
            if relative.is_empty() {
                return "crate".to_string();
            }

            let mut parts = relative.to_vec();
            if let Some(last) = parts.last_mut() {
                if last.ends_with(".rs") {
                    let stem = last.trim_end_matches(".rs");
                    *last = stem;
                }
                if *last == "lib" || *last == "main" {
                    parts.pop();
                    if parts.is_empty() {
                        return "crate".to_string();
                    }
                } else if *last == "mod" {
                    parts.pop();
                }
            }

            if parts.is_empty() {
                "crate".to_string()
            } else {
                format!("crate::{}", parts.join("::"))
            }
        } else {
            String::new()
        }
    }

    fn extract_struct_info(
        &self,
        struct_item: &ItemStruct,
        module_prefix: &str,
        file_path: &Path,
    ) -> Option<TypeInfo> {
        let struct_name = struct_item.ident.to_string();
        let full_path = if module_prefix.is_empty() {
            struct_name.clone()
        } else {
            format!("{}::{}", module_prefix, struct_name)
        };

        let docs = extract_docs(&struct_item.attrs);

        let fields = match &struct_item.fields {
            Fields::Named(fields) => fields
                .named
                .iter()
                .map(|field| {
                    let field_name = field.ident.as_ref().unwrap().to_string();
                    let type_name = type_to_string(&field.ty);
                    let field_docs = extract_docs(&field.attrs);
                    let (line, column) = field
                        .ident
                        .as_ref()
                        .map(|i| {
                            let start = i.span().start();
                            (start.line, start.column)
                        })
                        .unzip();

                    FieldInfo {
                        name: field_name,
                        type_name,
                        docs: field_docs,
                        line,
                        column,
                    }
                })
                .collect(),
            Fields::Unnamed(fields) => fields
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, field)| {
                    let type_name = type_to_string(&field.ty);
                    FieldInfo {
                        name: i.to_string(),
                        type_name,
                        docs: None,
                        line: None,
                        column: None,
                    }
                })
                .collect(),
            Fields::Unit => vec![],
        };

        let start = struct_item.ident.span().start();
        let line = Some(start.line);
        let column = Some(start.column);
        let has_default = has_default_derive(&struct_item.attrs);

        Some(TypeInfo {
            name: full_path,
            kind: TypeKind::Struct(fields),
            docs,
            source_file: Some(file_path.to_path_buf()),
            line,
            column,
            has_default,
        })
    }

    fn extract_enum_info(
        &self,
        enum_item: &ItemEnum,
        module_prefix: &str,
        file_path: &Path,
    ) -> Option<TypeInfo> {
        let enum_name = enum_item.ident.to_string();
        let full_path = if module_prefix.is_empty() {
            enum_name.clone()
        } else {
            format!("{}::{}", module_prefix, enum_name)
        };

        let docs = extract_docs(&enum_item.attrs);

        let variants = enum_item
            .variants
            .iter()
            .map(|variant| {
                let variant_name = variant.ident.to_string();
                let variant_docs = extract_docs(&variant.attrs);
                let variant_start = variant.ident.span().start();
                let variant_line = Some(variant_start.line);
                let variant_column = Some(variant_start.column);

                let fields = match &variant.fields {
                    Fields::Named(fields) => fields
                        .named
                        .iter()
                        .map(|field| {
                            let field_name = field.ident.as_ref().unwrap().to_string();
                            let type_name = type_to_string(&field.ty);
                            let field_docs = extract_docs(&field.attrs);
                            let (line, column) = field
                                .ident
                                .as_ref()
                                .map(|i| {
                                    let start = i.span().start();
                                    (start.line, start.column)
                                })
                                .unzip();

                            FieldInfo {
                                name: field_name,
                                type_name,
                                docs: field_docs,
                                line,
                                column,
                            }
                        })
                        .collect(),
                    Fields::Unnamed(fields) => fields
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(i, field)| {
                            let type_name = type_to_string(&field.ty);
                            FieldInfo {
                                name: i.to_string(),
                                type_name,
                                docs: None,
                                line: None,
                                column: None,
                            }
                        })
                        .collect(),
                    Fields::Unit => vec![],
                };

                EnumVariant {
                    name: variant_name,
                    fields,
                    docs: variant_docs,
                    line: variant_line,
                    column: variant_column,
                }
            })
            .collect();

        let start = enum_item.ident.span().start();
        let line = Some(start.line);
        let column = Some(start.column);
        let has_default = has_default_derive(&enum_item.attrs);

        Some(TypeInfo {
            name: full_path,
            kind: TypeKind::Enum(variants),
            docs,
            source_file: Some(file_path.to_path_buf()),
            line,
            column,
            has_default,
        })
    }

    fn extract_type_alias(
        &self,
        type_item: &ItemType,
        module_prefix: &str,
        type_aliases: &mut HashMap<String, String>,
    ) {
        let alias_name = type_item.ident.to_string();
        let full_alias_path = if module_prefix.is_empty() {
            alias_name.clone()
        } else {
            format!("{}::{}", module_prefix, alias_name)
        };

        let target_type = type_to_string(&type_item.ty);

        type_aliases.insert(full_alias_path, target_type);
    }

    pub async fn get_type_info(&self, type_path: &str) -> Option<TypeInfo> {
        // Resolve type aliases first
        let resolved_type = {
            let aliases = self.type_aliases.read().await;
            aliases
                .get(type_path)
                .cloned()
                .unwrap_or_else(|| type_path.to_string())
        };

        // First check cache with exact match
        {
            let cache = self.type_cache.read().await;
            if let Some(info) = cache.get(&resolved_type) {
                return Some(info.clone());
            }
            // Also try the original type path
            if let Some(info) = cache.get(type_path) {
                return Some(info.clone());
            }

            // If not found by exact match, try finding by simple name
            // e.g., "PostType" should match "crate::models::PostType"
            for (key, value) in cache.iter() {
                if key.ends_with(&format!("::{}", type_path)) || key == type_path {
                    return Some(value.clone());
                }
            }
        }

        // If not in cache, rescan and try again
        self.scan_workspace().await;

        let cache = self.type_cache.read().await;
        cache
            .get(&resolved_type)
            .cloned()
            .or_else(|| cache.get(type_path).cloned())
            .or_else(|| {
                // Try simple name match after rescan
                for (key, value) in cache.iter() {
                    if key.ends_with(&format!("::{}", type_path)) || key == type_path {
                        return Some(value.clone());
                    }
                }
                None
            })
    }

    /// Get all types from the workspace
    pub async fn get_all_types(&self) -> Vec<TypeInfo> {
        let cache = self.type_cache.read().await;
        cache.values().cloned().collect()
    }
}

fn extract_docs(attrs: &[Attribute]) -> Option<String> {
    let docs: Vec<String> = attrs
        .iter()
        .filter_map(|attr| {
            if attr.path().is_ident("doc") {
                attr.meta.require_name_value().ok().and_then(|nv| {
                    if let syn::Expr::Lit(lit) = &nv.value {
                        if let syn::Lit::Str(s) = &lit.lit {
                            return Some(s.value().trim().to_string());
                        }
                    }
                    None
                })
            } else {
                None
            }
        })
        .collect();

    if docs.is_empty() {
        None
    } else {
        Some(docs.join("\n"))
    }
}

fn has_default_derive(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|attr| {
        if attr.path().is_ident("derive") {
            // Parse the derive attribute to check for Default
            if let Ok(meta_list) = attr.meta.require_list() {
                let tokens_str = meta_list.tokens.to_string();
                // Check if "Default" appears in the derive list
                tokens_str.split(',').any(|s| s.trim() == "Default")
            } else {
                false
            }
        } else {
            false
        }
    })
}

fn type_to_string(ty: &Type) -> String {
    match ty {
        Type::Path(type_path) => quote::quote!(#type_path).to_string(),
        Type::Reference(type_ref) => {
            let inner = type_to_string(&type_ref.elem);
            if type_ref.mutability.is_some() {
                format!("&mut {}", inner)
            } else {
                format!("&{}", inner)
            }
        }
        _ => quote::quote!(#ty).to_string(),
    }
}

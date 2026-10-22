use std::collections::{HashMap, HashSet};
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
    pub has_default: bool,
}

impl FieldInfo {
    pub fn is_optional(&self) -> bool {
        self.type_name.starts_with("Option") || self.has_default
    }
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
            TypeKind::Enum(variants) => {
                // Search through all variants' fields
                variants
                    .iter()
                    .flat_map(|v| &v.fields)
                    .find(|f| f.name == field_name)
            }
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
    /// Types keyed by their crate-qualified path (e.g. `my_crate::models::User`),
    /// so RON files outside a crate (or in another workspace member) can refer to them.
    crate_types: RwLock<HashMap<String, TypeInfo>>,
    /// Type aliases keyed by their crate-qualified path.
    crate_type_aliases: RwLock<HashMap<String, String>>,
    initial_scan_complete: RwLock<bool>,
    custom_deserializers: RwLock<HashSet<String>>,
}

impl RustAnalyzer {
    pub fn new() -> Self {
        Self {
            workspace_root: RwLock::new(None),
            type_cache: RwLock::new(HashMap::new()),
            type_aliases: RwLock::new(HashMap::new()),
            crate_types: RwLock::new(HashMap::new()),
            crate_type_aliases: RwLock::new(HashMap::new()),
            initial_scan_complete: RwLock::new(false),
            custom_deserializers: RwLock::new(HashSet::new()),
        }
    }

    pub async fn set_workspace_root(&self, root: &Path) {
        *self.workspace_root.write().await = Some(root.to_path_buf());
        // Trigger initial scan
        self.scan_workspace().await;
        // Mark initial scan as complete
        *self.initial_scan_complete.write().await = true;
    }

    pub async fn scan_workspace(&self) {
        let root = {
            let guard = self.workspace_root.read().await;
            match guard.as_ref() {
                Some(r) => r.clone(),
                None => return,
            }
        };

        let mut type_cache = self.type_cache.write().await;
        let mut type_aliases = self.type_aliases.write().await;
        let mut crate_types = self.crate_types.write().await;
        let mut crate_type_aliases = self.crate_type_aliases.write().await;

        let mut custom_deserializers = HashSet::new();
        let mut crate_roots = HashMap::new();
        // Crate-qualified types and aliases, finalized once every file is scanned
        let mut qualified_types = HashMap::new();
        let mut qualified_aliases = HashMap::new();

        // Find all .rs files in the workspace
        for entry in WalkDir::new(&root)
            .follow_links(true)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().and_then(|s| s.to_str()) == Some("rs"))
        {
            if let Ok(content) = fs::read_to_string(entry.path()) {
                if let Ok(syntax_tree) = syn::parse_file(&content) {
                    let crate_root = find_crate_root(entry.path(), &root, &mut crate_roots);
                    // Module paths are relative to the owning crate's directory, so a
                    // `src` directory above the crate does not leak into them.
                    let module_prefix = crate_root
                        .as_ref()
                        .and_then(|(_, dir)| entry.path().strip_prefix(dir).ok())
                        .map(|relative| self.file_path_to_module_path(relative))
                        .unwrap_or_else(|| self.file_path_to_module_path(entry.path()));

                    let mut file_deserializers = HashSet::new();
                    collect_custom_deserializers(
                        &syntax_tree.items,
                        &module_prefix,
                        &mut file_deserializers,
                    );
                    let mut file_types = HashMap::new();
                    let mut file_aliases = HashMap::new();
                    self.extract_types_from_file(
                        &syntax_tree,
                        entry.path(),
                        &module_prefix,
                        &mut file_types,
                        &mut file_aliases,
                    );

                    // Also register `crate::...` paths under the owning crate's name,
                    // e.g. `crate::user::User` -> `proj_core::user::User`.
                    if let Some((crate_name, _)) = &crate_root {
                        for (key, info) in &file_types {
                            if let Some(qualified) = crate_qualified_path(key, crate_name) {
                                qualified_types
                                    .insert(qualified, (crate_name.clone(), info.clone()));
                            }
                        }
                        for (key, target) in &file_aliases {
                            if let Some(qualified) = crate_qualified_path(key, crate_name) {
                                qualified_aliases
                                    .insert(qualified, (crate_name.clone(), target.clone()));
                            }
                        }
                        for name in &file_deserializers {
                            if let Some(qualified) = crate_qualified_path(name, crate_name) {
                                custom_deserializers.insert(qualified);
                            }
                        }
                    }
                    custom_deserializers.extend(file_deserializers);
                    type_cache.extend(file_types);
                    type_aliases.extend(file_aliases);
                }
            }
        }

        // Resolve type references inside crate-qualified types and aliases against
        // their owning crate, so nested lookups do not fall back to the shared
        // `crate::` cache where same-named types from other members collide.
        let known: HashSet<String> = qualified_types
            .keys()
            .chain(qualified_aliases.keys())
            .cloned()
            .collect();
        for (qualified, (crate_name, mut info)) in qualified_types {
            let module = parent_path(&qualified);
            let qualify =
                |ty: &mut String| *ty = qualify_type_refs(ty, &crate_name, module, &known);
            match &mut info.kind {
                TypeKind::Struct(fields) => {
                    fields.iter_mut().for_each(|f| qualify(&mut f.type_name))
                }
                TypeKind::Enum(variants) => variants
                    .iter_mut()
                    .flat_map(|v| v.fields.iter_mut())
                    .for_each(|f| qualify(&mut f.type_name)),
            }
            info.name = qualified.clone();
            crate_types.insert(qualified, info);
        }
        for (qualified, (crate_name, target)) in qualified_aliases {
            let target = qualify_type_refs(&target, &crate_name, parent_path(&qualified), &known);
            // `type_to_string` renders paths as `a :: B`
            crate_type_aliases.insert(qualified, target.replace(" :: ", "::"));
        }

        *self.custom_deserializers.write().await = custom_deserializers;
    }

    fn extract_types_from_file(
        &self,
        syntax_tree: &syn::File,
        file_path: &Path,
        module_prefix: &str,
        type_cache: &mut HashMap<String, TypeInfo>,
        type_aliases: &mut HashMap<String, String>,
    ) {
        for item in &syntax_tree.items {
            if let Item::Struct(struct_item) = item {
                if let Some(type_info) =
                    self.extract_struct_info(struct_item, module_prefix, file_path)
                {
                    type_cache.insert(type_info.name.clone(), type_info);
                }
            } else if let Item::Enum(enum_item) = item {
                if let Some(type_info) = self.extract_enum_info(enum_item, module_prefix, file_path)
                {
                    type_cache.insert(type_info.name.clone(), type_info);
                }
            } else if let Item::Type(type_item) = item {
                self.extract_type_alias(type_item, module_prefix, type_aliases);
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

                    let field_attributes = serde_attributes::extract_serde_field_attributes(field);
                    let has_default = field_attributes.has_default;

                    FieldInfo {
                        name: field_name,
                        type_name,
                        docs: field_docs,
                        line,
                        column,
                        has_default,
                    }
                })
                .collect(),
            Fields::Unnamed(fields) => fields
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, field)| {
                    let type_name = type_to_string(&field.ty);

                    let field_attributes = serde_attributes::extract_serde_field_attributes(field);
                    let has_default = field_attributes.has_default;

                    FieldInfo {
                        name: i.to_string(),
                        type_name,
                        docs: None,
                        line: None,
                        column: None,
                        has_default,
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

                            let field_attributes =
                                serde_attributes::extract_serde_field_attributes(field);
                            let has_default = field_attributes.has_default;

                            FieldInfo {
                                name: field_name,
                                type_name,
                                docs: field_docs,
                                line,
                                column,
                                has_default,
                            }
                        })
                        .collect(),
                    Fields::Unnamed(fields) => fields
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(i, field)| {
                            let type_name = type_to_string(&field.ty);

                            let field_attributes =
                                serde_attributes::extract_serde_field_attributes(field);
                            let has_default = field_attributes.has_default;

                            FieldInfo {
                                name: i.to_string(),
                                type_name,
                                docs: None,
                                line: None,
                                column: None,
                                has_default,
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
            match aliases.get(type_path) {
                Some(target) => target.clone(),
                None => self
                    .crate_type_aliases
                    .read()
                    .await
                    .get(type_path)
                    .cloned()
                    .unwrap_or_else(|| type_path.to_string()),
            }
        };

        // Crate-qualified paths (e.g. `my_crate::models::User`)
        if let Some(info) = self
            .get_crate_qualified_type(type_path, &resolved_type)
            .await
        {
            return Some(info);
        }

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
            // After resolving alias, also try simple name match for resolved_type
            for (key, value) in cache.iter() {
                if key.ends_with(&format!("::{}", type_path))
                    || key.ends_with(&format!("::{}", resolved_type))
                    || key == type_path
                    || key == &resolved_type
                {
                    return Some(value.clone());
                }
            }
        }

        // If not in cache and initial scan hasn't completed, rescan
        let scan_complete = *self.initial_scan_complete.read().await;
        if !scan_complete {
            self.scan_workspace().await;

            if let Some(info) = self
                .get_crate_qualified_type(type_path, &resolved_type)
                .await
            {
                return Some(info);
            }

            let cache = self.type_cache.read().await;
            return cache
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
                });
        }

        // Initial scan already complete, type not found
        None
    }

    async fn get_crate_qualified_type(
        &self,
        type_path: &str,
        resolved_type: &str,
    ) -> Option<TypeInfo> {
        let crate_types = self.crate_types.read().await;
        crate_types
            .get(resolved_type)
            .or_else(|| crate_types.get(type_path))
            .cloned()
    }

    /// Custom Deserialize implementations and Serde conversions define their own
    /// wire format. Static field layout cannot validate that representation.
    pub async fn has_custom_deserializer(&self, type_name: &str) -> bool {
        let mut inner = type_name.replace(' ', "");
        let mut visited = HashSet::new();
        while visited.insert(inner.clone()) {
            let wrapper = ["Option<", "Vec<", "Box<", "Rc<", "Arc<"]
                .into_iter()
                .find(|prefix| inner.starts_with(prefix));
            if let Some(prefix) = wrapper.filter(|_| inner.ends_with('>')) {
                inner = inner[prefix.len()..inner.len() - 1].to_string();
                continue;
            }
            let aliases = self.type_aliases.read().await;
            let crate_aliases = self.crate_type_aliases.read().await;
            let target = aliases
                .get(&inner)
                .or_else(|| crate_aliases.get(&inner))
                .or_else(|| {
                    let mut matches = aliases
                        .iter()
                        .filter(|(name, _)| name.ends_with(&format!("::{inner}")));
                    let (_, target) = matches.next()?;
                    if matches.next().is_none() {
                        Some(target)
                    } else {
                        None
                    }
                });
            match target {
                Some(target) => inner = target.replace(' ', ""),
                None => break,
            }
        }
        if let Some(info) = self.get_type_info(&inner).await {
            return self.custom_deserializers.read().await.contains(&info.name);
        }
        false
    }

    /// Get all types from the workspace
    pub async fn get_all_types(&self) -> Vec<TypeInfo> {
        let cache = self.type_cache.read().await;
        cache.values().cloned().collect()
    }

    #[cfg(test)]
    pub async fn insert_type_for_test(&self, type_info: TypeInfo) {
        let mut cache = self.type_cache.write().await;
        cache.insert(type_info.name.clone(), type_info);
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

mod serde_attributes {
    use syn::Field;

    pub struct SerdeFieldAttributes {
        pub has_default: bool,
    }

    pub fn extract_serde_field_attributes(field: &Field) -> SerdeFieldAttributes {
        let mut field_attrs = SerdeFieldAttributes { has_default: false };

        let attrs = field
            .attrs
            .iter()
            .filter(|attr| attr.path().is_ident("serde"));

        for attr in attrs {
            let _ = attr.parse_nested_meta(|meta| {
                // #[serde(default)]
                if meta.path.is_ident("default") {
                    field_attrs.has_default = true;
                }
                Ok(())
            });
        }

        field_attrs
    }
}

/// Rewrite a `crate::...` path to `<crate_name>::...`.
fn crate_qualified_path(path: &str, crate_name: &str) -> Option<String> {
    path.strip_prefix("crate::")
        .map(|rest| format!("{crate_name}::{rest}"))
}

/// Find the crate a source file belongs to: the nearest `Cargo.toml` with a
/// `[package]` above it (within `root`). Returns the crate's path name (the
/// `[lib] name` if set, otherwise the package name with `-` replaced by `_`)
/// and the manifest directory. Results are cached per directory in `cache`.
fn find_crate_root(
    file_path: &Path,
    root: &Path,
    cache: &mut HashMap<PathBuf, Option<(String, PathBuf)>>,
) -> Option<(String, PathBuf)> {
    let mut visited = Vec::new();
    let mut result = None;
    let mut dir = file_path.parent();
    while let Some(current) = dir {
        if let Some(cached) = cache.get(current) {
            result = cached.clone();
            break;
        }
        visited.push(current.to_path_buf());
        let crate_name = fs::read_to_string(current.join("Cargo.toml"))
            .ok()
            .and_then(|text| text.parse::<toml::Table>().ok())
            .and_then(|manifest| {
                let package_name = manifest.get("package")?.get("name")?.as_str()?;
                let lib_name = manifest
                    .get("lib")
                    .and_then(|lib| lib.get("name"))
                    .and_then(|name| name.as_str());
                Some(lib_name.unwrap_or(package_name).replace('-', "_"))
            });
        if let Some(name) = crate_name {
            result = Some((name, current.to_path_buf()));
            break;
        }
        if current == root {
            break;
        }
        dir = current.parent();
    }
    for path in visited {
        cache.insert(path, result.clone());
    }
    result
}

/// `a::b::C` -> `a::b`
fn parent_path(path: &str) -> &str {
    path.rsplit_once("::").map_or("", |(parent, _)| parent)
}

/// Rewrite the paths in a rendered type (e.g. `Vec < self :: Child >`) that refer
/// to items of the owning crate into crate-qualified paths (`my_crate::m::Child`).
/// `module` is the crate-qualified module the type is written in; paths are only
/// rewritten when they resolve to an entry of `known`.
fn qualify_type_refs(ty: &str, crate_name: &str, module: &str, known: &HashSet<String>) -> String {
    let chars: Vec<char> = ty.chars().collect();
    let is_ident_start = |c: char| c.is_alphabetic() || c == '_';
    let skip_spaces = |mut j: usize| {
        while j < chars.len() && chars[j] == ' ' {
            j += 1;
        }
        j
    };
    let mut out = String::new();
    let mut i = 0;
    while i < chars.len() {
        // A lifetime is not a path
        if chars[i] == '\'' {
            out.push(chars[i]);
            i += 1;
            while i < chars.len() && (chars[i].is_alphanumeric() || chars[i] == '_') {
                out.push(chars[i]);
                i += 1;
            }
            continue;
        }
        if !is_ident_start(chars[i]) {
            out.push(chars[i]);
            i += 1;
            continue;
        }
        let start = i;
        let mut segments = Vec::new();
        loop {
            let segment_start = i;
            while i < chars.len() && (chars[i].is_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            segments.push(chars[segment_start..i].iter().collect::<String>());
            let j = skip_spaces(i);
            if j + 1 < chars.len() && chars[j] == ':' && chars[j + 1] == ':' {
                let k = skip_spaces(j + 2);
                if k < chars.len() && is_ident_start(chars[k]) {
                    i = k;
                    continue;
                }
            }
            break;
        }
        match resolve_in_crate(&segments, crate_name, module, known) {
            Some(qualified) => out.push_str(&qualified),
            None => out.extend(&chars[start..i]),
        }
    }
    out
}

/// Resolve a path written in `module` of `crate_name` to a crate-qualified path
/// in `known`.
fn resolve_in_crate(
    segments: &[String],
    crate_name: &str,
    module: &str,
    known: &HashSet<String>,
) -> Option<String> {
    let mut base: Vec<&str> = module.split("::").collect();
    let mut rest = segments;
    match segments.first()?.as_str() {
        "crate" => {
            base = vec![crate_name];
            rest = &segments[1..];
        }
        "self" => rest = &segments[1..],
        "super" => {
            while rest.first().is_some_and(|s| s == "super") {
                if base.len() <= 1 {
                    return None;
                }
                base.pop();
                rest = &rest[1..];
            }
        }
        _ => {}
    }
    if rest.is_empty() {
        return None;
    }
    let candidate = base
        .into_iter()
        .map(str::to_string)
        .chain(rest.iter().cloned())
        .collect::<Vec<_>>()
        .join("::");
    known.contains(&candidate).then_some(candidate)
}

fn collect_custom_deserializers(items: &[Item], prefix: &str, result: &mut HashSet<String>) {
    let qualify = |name: &str| {
        if prefix.is_empty() {
            name.to_string()
        } else {
            format!("{prefix}::{name}")
        }
    };
    for item in items {
        match item {
            Item::Impl(item)
                if item.trait_.as_ref().is_some_and(|(_, path, _)| {
                    path.segments
                        .last()
                        .is_some_and(|segment| segment.ident == "Deserialize")
                }) =>
            {
                if let Type::Path(ty) = item.self_ty.as_ref() {
                    let name = ty
                        .path
                        .segments
                        .iter()
                        .map(|s| s.ident.to_string())
                        .collect::<Vec<_>>()
                        .join("::");
                    result.insert(if name.starts_with("crate::") {
                        name
                    } else {
                        qualify(&name)
                    });
                }
            }
            Item::Struct(item) if has_serde_conversion(&item.attrs) => {
                result.insert(qualify(&item.ident.to_string()));
            }
            Item::Enum(item) if has_serde_conversion(&item.attrs) => {
                result.insert(qualify(&item.ident.to_string()));
            }
            Item::Mod(item) => {
                if let Some((_, items)) = &item.content {
                    collect_custom_deserializers(items, &qualify(&item.ident.to_string()), result);
                }
            }
            _ => {}
        }
    }
}

fn has_serde_conversion(attrs: &[Attribute]) -> bool {
    attrs
        .iter()
        .filter(|attr| attr.path().is_ident("serde"))
        .any(|attr| {
            let mut custom = false;
            let _ = attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("transparent")
                    || meta.path.is_ident("from")
                    || meta.path.is_ident("try_from")
                {
                    custom = true;
                }
                // Consume unrelated attribute values so later options are visited.
                if meta.input.peek(syn::Token![=]) {
                    let _: syn::Expr = meta.value()?.parse()?;
                } else if meta.input.peek(syn::token::Paren) {
                    let content;
                    syn::parenthesized!(content in meta.input);
                    let _: proc_macro2::TokenStream = content.parse()?;
                }
                Ok(())
            });
            custom
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn resolves_crate_qualified_paths_in_workspace() {
        let root =
            std::env::temp_dir().join(format!("ron-workspace-crates-{}", std::process::id()));
        let write = |path: &str, content: &str| {
            let path = root.join(path);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, content).unwrap();
        };
        write("Cargo.toml", "[workspace]\nmembers = [\"crates/*\"]\n");
        write(
            "crates/proj_core/Cargo.toml",
            "[package]\nname = \"proj-core\"\nversion = \"0.1.0\"\n",
        );
        write("crates/proj_core/src/lib.rs", "pub mod user;\n");
        write(
            "crates/proj_core/src/user.rs",
            "pub struct User { pub id: u32, pub name: String }\npub type UserAlias = crate::user::User;\n",
        );
        write(
            "crates/other/Cargo.toml",
            "[package]\nname = \"other\"\nversion = \"0.1.0\"\n",
        );
        write("crates/other/src/lib.rs", "pub mod user;\n");
        write(
            "crates/other/src/user.rs",
            "pub struct User { pub email: String }\n",
        );

        let analyzer = RustAnalyzer::new();
        analyzer.set_workspace_root(&root).await;

        let user = analyzer
            .get_type_info("proj_core::user::User")
            .await
            .expect("proj_core::user::User should resolve");
        assert!(user.find_field("name").is_some());

        let other = analyzer
            .get_type_info("other::user::User")
            .await
            .expect("other::user::User should resolve");
        assert!(other.find_field("email").is_some());

        let alias = analyzer
            .get_type_info("proj_core::user::UserAlias")
            .await
            .expect("crate-qualified alias should resolve");
        assert!(alias.find_field("id").is_some());

        // Existing `crate::` paths keep working.
        assert!(analyzer.get_type_info("crate::user::User").await.is_some());

        fs::remove_dir_all(root).unwrap();
    }

    fn write_tree(root: &Path, files: &[(&str, &str)]) {
        for (path, content) in files {
            let path = root.join(path);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, content).unwrap();
        }
    }

    #[tokio::test]
    async fn crate_module_paths_ignore_src_above_the_crate() {
        let base = std::env::temp_dir().join(format!("ron-src-ancestor-{}", std::process::id()));
        // The workspace itself lives under a directory named `src`.
        let root = base.join("src").join("ws");
        write_tree(
            &root,
            &[
                ("Cargo.toml", "[workspace]\nmembers = [\"crates/*\"]\n"),
                (
                    "crates/app/Cargo.toml",
                    "[package]\nname = \"app\"\nversion = \"0.1.0\"\n",
                ),
                ("crates/app/src/lib.rs", "pub mod user;\n"),
                ("crates/app/src/user.rs", "pub struct User { pub id: u32 }\n"),
                (
                    "crates/schema/Cargo.toml",
                    "[package]\nname = \"schema-package\"\nversion = \"0.1.0\"\n\n[lib]\nname = \"schema\"\n",
                ),
                ("crates/schema/src/lib.rs", "pub struct Config { pub level: u8 }\n"),
            ],
        );

        let analyzer = RustAnalyzer::new();
        analyzer.set_workspace_root(&root).await;

        let user = analyzer
            .get_type_info("app::user::User")
            .await
            .expect("app::user::User should resolve");
        assert!(user.find_field("id").is_some());
        assert!(analyzer.get_type_info("crate::user::User").await.is_some());

        // `[lib] name` is the path used in Rust code.
        let config = analyzer
            .get_type_info("schema::Config")
            .await
            .expect("schema::Config should resolve via the lib target name");
        assert!(config.find_field("level").is_some());

        fs::remove_dir_all(base).unwrap();
    }

    #[tokio::test]
    async fn crate_qualified_types_keep_their_crate_for_nested_lookups() {
        let root = std::env::temp_dir().join(format!("ron-crate-identity-{}", std::process::id()));
        let user_rs = |child_field: &str, extra: &str| {
            format!(
                "pub struct User {{ pub child: Child, pub children: Vec<self::Child> }}\n\
                 pub struct Child {{ pub {child_field}: u32 }}\n\
                 pub type Alias = User;\n\
                 pub type SelfAlias = self::User;\n\
                 pub mod nested {{ pub type SuperAlias = super::User; }}\n{extra}"
            )
        };
        let a_user = user_rs("a_only", "");
        let b_user = user_rs(
            "b_only",
            "impl<'de> serde::Deserialize<'de> for User { fn deserialize<D>(_: D) -> Result<Self, D::Error> where D: serde::Deserializer<'de> { todo!() } }\n",
        );
        write_tree(
            &root,
            &[
                ("Cargo.toml", "[workspace]\nmembers = [\"a\", \"b\"]\n"),
                (
                    "a/Cargo.toml",
                    "[package]\nname = \"a\"\nversion = \"0.1.0\"\n",
                ),
                ("a/src/lib.rs", "pub mod user;\n"),
                ("a/src/user.rs", &a_user),
                (
                    "b/Cargo.toml",
                    "[package]\nname = \"b\"\nversion = \"0.1.0\"\n",
                ),
                ("b/src/lib.rs", "pub mod user;\n"),
                ("b/src/user.rs", &b_user),
            ],
        );

        let analyzer = RustAnalyzer::new();
        analyzer.set_workspace_root(&root).await;

        for krate in ["a", "b"] {
            let own = format!("{krate}_only");
            let user = analyzer
                .get_type_info(&format!("{krate}::user::User"))
                .await
                .unwrap();
            assert_eq!(user.name, format!("{krate}::user::User"));

            // Nested field types resolve within the owning crate.
            let child_type = &user.find_field("child").unwrap().type_name;
            let child = analyzer.get_type_info(child_type).await.unwrap();
            assert!(child.find_field(&own).is_some(), "{krate}: {child_type}");
            let children_type = user
                .find_field("children")
                .unwrap()
                .type_name
                .replace(' ', "");
            assert_eq!(children_type, format!("Vec<{krate}::user::Child>"));

            // Relative aliases resolve within the owning crate and module.
            for alias in ["Alias", "SelfAlias", "nested::SuperAlias"] {
                let info = analyzer
                    .get_type_info(&format!("{krate}::user::{alias}"))
                    .await
                    .unwrap_or_else(|| panic!("{krate}::user::{alias} should resolve"));
                assert_eq!(info.name, format!("{krate}::user::User"), "{alias}");
            }
        }

        // Only `b`'s User has a custom Deserialize impl.
        assert!(analyzer.has_custom_deserializer("b::user::User").await);
        assert!(analyzer.has_custom_deserializer("b::user::Alias").await);
        assert!(!analyzer.has_custom_deserializer("a::user::User").await);
        assert!(!analyzer.has_custom_deserializer("a::user::Alias").await);

        fs::remove_dir_all(root).unwrap();
    }
}

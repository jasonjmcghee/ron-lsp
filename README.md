# ron-lsp

[![Crates.io](https://img.shields.io/crates/v/ron-lsp.svg)](https://crates.io/crates/ron-lsp)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jasonjmcghee/ron-lsp#license)
[![Visual Studio Marketplace Badge](https://img.shields.io/badge/Visual%20Studio%20Marketplace-0.1.0-blue.svg)](https://marketplace.visualstudio.com/items?itemName=JasonMcGhee.ron-lsp)
[![JetBrains Plugin Version](https://img.shields.io/jetbrains/plugin/v/28862-ron-lsp)](https://plugins.jetbrains.com/plugin/28862-ron-lsp)

_Type validation for `.ron` files (in or out of the ide)_

An LSP for [RON](https://github.com/ron-rs/ron) files that provides autocomplete, diagnostics, go to definition, code actions, and hover support based on Rust type annotations. It can also be used to check in bulk via CLI, optionally with a path. `ron-lsp check [<path>]`

<img width="727" height="214" alt="Screenshot 2025-10-24 at 9 59 22 AM" src="https://github.com/user-attachments/assets/e1c436ff-2f58-474e-b97f-14215107067d" />

## Getting started

Install:

```bash
cargo install ron-lsp
```

Add comment annotations to the top of `.ron` files like `/* @[crate::models::User] */`.

And then use the cli (from a rust project working directory):

```bash
ron-lsp check
```

It'll output something like this if there are warnings / errors:

<img width="503" height="488" alt="Screenshot 2025-10-24 at 10 02 35 AM" src="https://github.com/user-attachments/assets/7914fef5-8fff-4216-bc75-e7437ee5c333" />

You can optionally pass a file or folder. (e.g. `ron-lsp check crates/sub-crate` - it will recursively check all `.ron` files from that point.)

It will use the nearest `Cargo.toml` starting from the resolved `.ron` file.

## Usage

### Type Annotation Format

At the top of your RON file, add a block comment with the type annotation:

```ron
/* @[crate::models::User] */

User(
    id: 1,
    name: "Alice",
    email: "alice@example.com",
    age: 30,
)
```

The LSP will:
1. Parse the `@[crate::models::User]` annotation
2. Find the `User` struct in your Rust project
3. Extract field names, types, and documentation
4. Provide autocomplete and validation
5. Support `Default` trait for optional field omission
6. Provide code actions for inserting either required or missing fields, when applicable

### Example

**src/models/user.rs:**
```rust
pub struct User {
    pub id: u32,
    pub name: String,
    pub email: String,
    pub age: u32,
    pub bio: Option<String>,
}
```

**data/users.ron:**
```ron
/* @[crate::models::User] */

User(
    id: 1,
    name: "Alice",
    email: "alice@example.com",
    age: 30,
    bio: Some("Software developer"),
)
```

### Example with Defaults

```rust
#[derive(Default, Serialize, Deserialize)]
pub struct Config {
    pub host: String,
    pub port: u16,
    pub max_connections: u32,
    pub debug: bool,
    pub api_key: Option<String>,
    pub allowed_origins: Vec<String>,
}
```

```ron
/* @[crate::models::Config] */
Config(
    // This Config struct has #[derive(Default)]
    // So we can omit fields and they will use their default values
    // The LSP should NOT show warnings for missing fields
    port: 8080,
    debug: true,
)
```

<img width="707" height="436" alt="Screenshot 2025-10-24 at 5 52 13 PM" src="https://github.com/user-attachments/assets/f638947d-0408-4a7d-a209-de8e5d56c15d" />

## ron.toml configuration

You can configure default type mappings for RON files using an optional ron.toml file at your project root
(next to Cargo.toml).

This lets you omit the [Type Annotation](#type-annotation-format) in RON files whose paths match a glob pattern
which is useful if you have multiple files with the same type (e.g., bevy assets).

Example:

```toml
[[types]]
# Matches all files named `post.ron`
glob = "**/post.ron"
# Fully qualified type name
type = "crate::models::Post"

[[types]]
# Matches all files ron files in the `config` directory
glob = "**/config/*.ron"
type = "crate::models::Config"

[[types]]
# Matches all files ending with .user.ron
glob = "**/*.user.ron"
type = "crate::models::User"
```

### Precedence

1. Type annotation in RON file
2. Type declaration in ron.toml, declarations are tested in the order of the file


## Editor Integration

Expand a section below for editor-specific instructions.

<details>
<summary><h3>VSCode</h3></summary>

Make sure you already did `cargo install ron-lsp`.

Either:

- [Install `ron-lsp` from the VS Code marketplace](https://marketplace.visualstudio.com/items?itemName=JasonMcGhee.ron-lsp)

OR

- Grab `.vsix` from [releases](https://github.com/jasonjmcghee/ron-lsp/releases)

OR

- `cd vscode-extensions` and package by running `npm install` and `vsce package`

Then, either ensure 'ron-lsp' is in your PATH or update `.vscode/settings.json`:

```json
{
  ...
  "ronLsp.serverPath": "/path/to/ron-lsp"
}
```
</details>

[![Visual Studio Marketplace Badge](https://img.shields.io/badge/Visual%20Studio%20Marketplace-0.1.0-blue.svg)](https://marketplace.visualstudio.com/items?itemName=JasonMcGhee.ron-lsp)


<details>
<summary><h3>JetBrains</h3></summary>

Make sure you already did `cargo install ron-lsp`.

Either:

- [Install `ron-lsp` from the JetBrains marketplace](https://plugins.jetbrains.com/plugin/28862-ron-lsp)

OR

- Grab `.zip` from [releases](https://github.com/jasonjmcghee/ron-lsp/releases)

OR :

```bash
cd jetbrains-plugin
./gradlew buildPlugin
```

And "Install Plugin from Disk" and choose the zip.

Then, either ensure 'ron-lsp' is in your PATH or update "Server path" in `Settings > Tools > RON LSP`.
</details>

[![JetBrains Plugin Version](https://img.shields.io/jetbrains/plugin/v/28862-ron-lsp)](https://plugins.jetbrains.com/plugin/28862-ron-lsp)


<details>
<summary><h3>Neovim</h3></summary>

Make sure you already did `cargo install ron-lsp`.

Ensure you already have [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig).

Note: If you don't want `ron-lsp` in your path, replace with the absolute path.

Add [ron.lua](./nvim-plugin/ron.lua) to `~/.config/nvim/lua/plugins/ron.lua`.

</details>

<details>
<summary><h3>Other editors (e.g. Zed / Helix)</h3></summary>

See their docs on how to add an LSP, you should be able to follow them for this plugin.

</details>

## Contributing

Contributions welcome! This is a foundational implementation that can be extended with more features.

## License

MIT (except altered Rust logo svg, see below)

## Attribution

The logo used in the Jetbrains plugin is a colorized version of the Rust logo which is [CC-BY 4.0](https://creativecommons.org/licenses/by/4.0/). See Rust logo [specific trademark policy](https://rustfoundation.org/policy/rust-trademark-policy/#art) for additional details.

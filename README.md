# RON LSP - Type validation for `.ron` files (in or out of the ide)

An LSP for [RON](https://github.com/ron-rs/ron) files that provides autocomplete, diagnostics, go to definition, code actions, and hover support based on Rust type annotations.

<img width="727" height="214" alt="Screenshot 2025-10-24 at 9 59 22 AM" src="https://github.com/user-attachments/assets/e1c436ff-2f58-474e-b97f-14215107067d" />

It can also be used to check in bulk via CLI, optionally with a path. `ron-lsp check [<path>]`

<img width="503" height="488" alt="Screenshot 2025-10-24 at 10 02 35 AM" src="https://github.com/user-attachments/assets/7914fef5-8fff-4216-bc75-e7437ee5c333" />

## Note

There's a lot of functionality that could be added to improve this, like more code actions, better auto complete, less hacky type detection and resolution... the list goes on.

Feel free to open a PR or an issue.

## Getting started

Build:

```bash
cargo build --release
```

The binary will be at `target/release/ron-lsp`, move it wherever you like.

To minimize configuration, you might want to add it to your PATH.

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

## CLI

Use via `ron-lsp check`

You can optionally pass a file or folder. (e.g. `ron-lsp check crates/sub-crate` - it will recursively check all `.ron` files from that point.)

It will use the nearest Cargo.toml starting from the resolved `.ron` file.

```bash
./target/release/ron-lsp check
```

<img width="503" height="488" alt="Screenshot 2025-10-24 at 10 02 35 AM" src="https://github.com/user-attachments/assets/7914fef5-8fff-4216-bc75-e7437ee5c333" />

## Editor Integration

### VSCode

Either:

- Grab `.vsix` from releases

OR

- `cd vscode-extensions` and package by running `npm install` and `vsce package`

Then, either ensure 'ron-lsp' is in your PATH or update `.vscode/settings.json`:

```json
{
  ...
  "ronLsp.serverPath": "/path/to/ron-lsp"
}
```

### Jetbrains


Either:

- Grab `.zip` from releases

OR :

```bash
cd jetbrains-plugin
./gradlew buildPlugin
```

And "Install Plugin from Disk" and choose the zip.

Then, either ensure 'ron-lsp' is in your PATH or update "Server path" in `Settings > Tools > RON LSP`.

### Neovim (with nvim-lspconfig)

Note: If you don't want `ron-lsp` in your path, replace with the absolute path.

Add `~/.config/nvim/lua/plugins/ron.lua`:

```lua
return {
  {
    "neovim/nvim-lspconfig",
    opts = function(_, opts)
      local configs = require("lspconfig.configs")

      if not configs.ron_lsp then
        configs.ron_lsp = {
          default_config = {
            cmd = { vim.fn.expand("ron-lsp") },
            filetypes = { "ron" },
            root_dir = function(fname)
              local util = require("lspconfig.util")
              return util.root_pattern("Cargo.toml", ".git")(fname) or vim.loop.cwd()
            end,
            settings = {},
          },
        }
      end

      opts.servers = opts.servers or {}
      opts.servers.ron_lsp = {
        -- Add any specific on_attach, capabilities, or handlers here if needed
      }

      return opts
    end,
  },
}
```

### Zed / Helix / Other editors

See their docs on how to add an LSP, it should be the same.

## Contributing

Contributions welcome! This is a foundational implementation that can be extended with more features.

## License

MIT (except altered Rust logo svg, see below)

## Attribution

The logo used in the Jetbrains plugin is a colorized version of the Rust logo which is [CC-BY 4.0](https://creativecommons.org/licenses/by/4.0/). See Rust logo [specific trademark policy](https://rustfoundation.org/policy/rust-trademark-policy/#art) for additional details.

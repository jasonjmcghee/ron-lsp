# RON LSP - Type validation for `.ron` files (in or out of the ide)

An LSP for [RON](https://github.com/ron-rs/ron) files that provides autocomplete and diagnostics support based on Rust type annotations.

<img width="727" height="214" alt="Screenshot 2025-10-24 at 9 59 22 AM" src="https://github.com/user-attachments/assets/e1c436ff-2f58-474e-b97f-14215107067d" />

It can also be used to check in bulk via CLI, optionally with a path. `ron-lsp check [<path>]`

<img width="521" height="487" alt="Screenshot 2025-10-24 at 9 53 03 AM" src="https://github.com/user-attachments/assets/9d5f2f44-a962-4c43-bf8c-32d10e4c02f3" />


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

## CLI

Use via `ron-lsp check`

You can optionally pass a file or folder. (e.g. `ron-lsp check crates/sub-crate` - it will recursively check all `.ron` files from that point.)

It will use the nearest Cargo.toml starting from the resolved `.ron` file.

```bash
./target/release/ron-lsp check
Checking RON files in: "/Users/jason/workspace/ron-lsp"
Found 17 types in workspace
Error: Unknown field 'foo'
    ╭─[./example/data/post.ron:11:5]
    │
 11 │     foo: 1,
    │     ─┬─
    │      ╰─── Unknown field 'foo'
────╯
Error: Type mismatch: expected User, got integer
   ╭─[./example/data/post.ron:7:13]
   │
 7 │     author: 1,
   │             ┬
   │             ╰── Type mismatch: expected User, got integer
───╯
Error: Type mismatch: expected u32, got float
   ╭─[./example/data/post.ron:8:12]
   │
 8 │     likes: 42.0,
   │            ──┬─
   │              ╰─── Type mismatch: expected u32, got float
───╯
Warning: Missing fields: published
   ╭─[./example/data/post.ron:3:1]
   │
 3 │ Post(
   │ ──┬─
   │   ╰─── Missing fields: published
───╯
```

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

MIT

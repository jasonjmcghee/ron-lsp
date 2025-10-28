# RON LSP - VSCode Extension

Language Server Protocol integration for RON (Rust Object Notation) files in Visual Studio Code.

## Features

- **Diagnostics** for syntax errors, type mismatches, and missing fields
- **Type validation** based on Rust struct/enum definitions
- **Code actions** to insert missing/required fields
- **Support for Default trait** (optional fields)
- **Go to definition** for types and fields
- **Code completion** for fields and enum variants
- **Hover documentation** showing field types and doc comments

## Requirements

- Visual Studio Code 1.75.0 or later
- `ron-lsp` binary installed and available in your PATH

## Installation

### Install the LSP Server

```bash
cargo install ron-lsp
```

### Install the Extension

Search for "RON LSP" in the VSCode Extensions view, or install from the [VSCode Marketplace](https://marketplace.visualstudio.com/items?itemName=YOUR_PUBLISHER_ID.ron-lsp).

## Configuration

If `ron-lsp` is not in your PATH, configure the binary location in your settings:

```json
{
  "ronLsp.serverPath": "/path/to/ron-lsp"
}
```

## Usage

1. Open a Rust project with RON files
2. Add type annotations to your RON files:
   ```ron
   /* @[crate::models::User] */

   User(
       id: 1,
       name: "Alice",
       email: "alice@example.com",
   )
   ```

3. The LSP will automatically:
   - Validate field names and types
   - Provide completions as you type
   - Show documentation on hover
   - Enable go-to-definition navigation

## Extension Settings

* `ronLsp.serverPath`: Path to the `ron-lsp` binary (defaults to `ron-lsp` in PATH)
* `ronLsp.trace.server`: Traces the communication between VSCode and the language server (for debugging)

## Troubleshooting

### Server not starting

1. Verify `ron-lsp` is installed:
   ```bash
   ron-lsp check
   ```

2. Check the server path configuration in your settings

3. Enable trace logging and check the Output panel:
   - **View → Output → RON LSP**
   - Set `"ronLsp.trace.server": "verbose"` in settings

### No completions or diagnostics

1. Make sure your RON file has a type annotation:
   ```ron
   /* @[crate::models::YourType] */
   ```

2. Verify the type exists in your Rust project

3. Check that your Rust project has a `Cargo.toml` file

## License

MIT

## Relevant Links

- [ron-lsp](https://github.com/jasonjmcghee/ron-lsp) - The LSP server repository
- [RON](https://github.com/ron-rs/ron) - Rust Object Notation

---

## Development

### Building from Source

1. Clone this repository:
   ```bash
   git clone https://github.com/jasonjmcghee/ron-lsp.git
   cd ron-lsp/vscode-extension
   ```

2. Install dependencies:
   ```bash
   npm install
   ```

3. Package the extension:
   ```bash
   npx vsce package
   ```

4. Install the generated `.vsix` file via Command Palette → **Extensions: Install from VSIX**

## Contributing

Contributions are welcome! Please open an issue or pull request on GitHub.

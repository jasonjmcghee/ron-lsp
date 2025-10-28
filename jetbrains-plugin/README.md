# RON LSP - JetBrains Plugin

Language Server Protocol integration for RON (Rust Object Notation) files in JetBrains IDEs.

## Features

- **Diagnostics** for syntax errors, type mismatches, and missing fields
- **Type validation** based on Rust struct/enum definitions
- **Code actions** to insert missing/required fields
- **Support for Default trait** (optional fields)
- **Go to definition** for types and fields
- **Code completion** for fields and enum variants
- **Hover documentation** showing field types and doc comments

## Requirements

- IntelliJ IDEA Ultimate, CLion, RustRover, or any other IntelliJ-based IDE (Ultimate version required for LSP support)
- Version 2025.2 or later
- `ron-lsp` binary installed and available in your PATH

## Installation

### Install the LSP Server
```bash
cargo install ron-lsp
```

### Install the Plugin

Search for "RON LSP" in the JetBrains Marketplace within your IDE (**Settings/Preferences → Plugins**).

## Configuration

If `ron-lsp` is not in your PATH, configure the binary location in **Settings → Tools → RON LSP → Server path**.

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

## Supported IDEs

- IntelliJ IDEA Ultimate 2025.2+
- CLion 2025.2+
- RustRover 2025.2+
- PyCharm Professional 2025.2+
- WebStorm 2025.2+
- PhpStorm 2025.2+
- GoLand 2025.2+
- Rider 2025.2+
- DataGrip 2025.2+
- DataSpell 2025.2+
- RubyMine 2025.2+

Note: Community editions do not support LSP integration.

## Troubleshooting

### Server not starting

1. Verify `ron-lsp` is installed:
```bash
   ron-lsp check
```

2. Check the LSP server path in **Settings → Tools → RON LSP**

3. Enable logging in the settings and check IDE logs:
   - **Help → Show Log in Finder/Explorer**

### No completions or diagnostics

1. Make sure your RON file has a type annotation:
```ron
   /* @[crate::models::YourType] */
```

2. Verify the type exists in your Rust project

3. Check that your Rust project has a `Cargo.toml` file

## License

MIT (except altered Rust logo svg, see below)

## Attribution

The logo used in the Jetbrains plugin is a colorized version of the Rust logo which is [CC-BY 4.0](https://creativecommons.org/licenses/by/4.0/). See Rust logo [specific trademark policy](https://rustfoundation.org/policy/rust-trademark-policy/#art) for additional details.

## Relevant Links

- [ron-lsp](https://github.com/jasonjmcghee/ron-lsp) - The LSP server repository
- [RON](https://github.com/ron-rs/ron) - Rust Object Notation

---

## Development

### Building from Source

1. Clone this repository:
```bash
   git clone https://github.com/jasonjmcghee/ron-lsp.git
   cd ron-lsp/jetbrains-plugin
```

2. Build the plugin:
```bash
   ./gradlew buildPlugin
```

3. Install the plugin:
   - Open your JetBrains IDE
   - Go to **Settings/Preferences → Plugins → ⚙️ → Install Plugin from Disk**
   - Select `build/distributions/ron-lsp-intellij-0.1.0.zip` (or current version)

## Contributing

Contributions are welcome! Please open an issue or pull request on GitHub.

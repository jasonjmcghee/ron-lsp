#[cfg(feature = "cli")]
use ariadne::{Color, Label, Report, ReportKind, Source};
#[cfg(feature = "cli")]
use std::path::Path;
use tower_lsp::lsp_types::{Diagnostic as LspDiagnostic, DiagnosticSeverity};

/// A diagnostic that is independent of the output format (LSP or miette)
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub line: u32,
    pub col_start: u32,
    pub col_end: u32,
    pub severity: Severity,
    pub message: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    #[allow(dead_code)]
    Info,
}

impl Diagnostic {
    #[allow(dead_code)]
    pub fn error(line: u32, col_start: u32, col_end: u32, message: String) -> Self {
        Self {
            line,
            col_start,
            col_end,
            severity: Severity::Error,
            message,
        }
    }

    #[allow(dead_code)]
    pub fn warning(line: u32, col_start: u32, col_end: u32, message: String) -> Self {
        Self {
            line,
            col_start,
            col_end,
            severity: Severity::Warning,
            message,
        }
    }

    /// Convert to LSP diagnostic
    #[allow(dead_code)]
    pub fn to_lsp(&self) -> LspDiagnostic {
        use tower_lsp::lsp_types::{Position, Range};

        LspDiagnostic {
            range: Range::new(
                Position::new(self.line, self.col_start),
                Position::new(self.line, self.col_end),
            ),
            severity: Some(match self.severity {
                Severity::Error => DiagnosticSeverity::ERROR,
                Severity::Warning => DiagnosticSeverity::WARNING,
                Severity::Info => DiagnosticSeverity::INFORMATION,
            }),
            message: self.message.clone(),
            ..Default::default()
        }
    }

    /// Report using ariadne (for CLI output)
    #[cfg(feature = "cli")]
    pub fn report_ariadne(&self, file_path: &Path, source: &str) {
        let report_kind = match self.severity {
            Severity::Error => ReportKind::Error,
            Severity::Warning => ReportKind::Warning,
            Severity::Info => ReportKind::Advice,
        };

        let color = match self.severity {
            Severity::Error => Color::Red,
            Severity::Warning => Color::Yellow,
            Severity::Info => Color::Blue,
        };

        // Convert line/col to byte offset
        let byte_offset = get_byte_offset(source, self.line as usize, self.col_start as usize);
        let byte_end = get_byte_offset(source, self.line as usize, self.col_end as usize);

        Report::build(report_kind, file_path.display().to_string(), byte_offset)
            .with_message(&self.message)
            .with_label(
                Label::new((file_path.display().to_string(), byte_offset..byte_end))
                    .with_message(&self.message)
                    .with_color(color),
            )
            .finish()
            .eprint((file_path.display().to_string(), Source::from(source)))
            .unwrap();
    }
}

/// Convert line/col position to byte offset in source
#[cfg(feature = "cli")]
fn get_byte_offset(source: &str, line: usize, col: usize) -> usize {
    let mut byte_offset = 0;
    for (i, line_text) in source.lines().enumerate() {
        if i == line {
            // Add the column offset
            return byte_offset + col.min(line_text.len());
        }
        // Add line length + newline
        byte_offset += line_text.len() + 1;
    }
    byte_offset
}

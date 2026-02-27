#[cfg(feature = "cli")]
use ariadne::{Color, Label, Report, ReportKind, Source};
#[cfg(feature = "cli")]
use std::path::Path;
use tower_lsp::lsp_types::{Diagnostic as LspDiagnostic, DiagnosticSeverity};

/// A diagnostic that is independent of the output format (LSP or ariadne)
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub line: u32,
    pub col_start: u32,
    pub col_end: u32,
    pub severity: Severity,
    pub message: String,
}

impl PartialEq for Diagnostic {
    fn eq(&self, other: &Self) -> bool {
        self.line == other.line && self.col_start == other.col_start
    }
}

impl Eq for Diagnostic {}

impl PartialOrd for Diagnostic {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Diagnostic {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Sort by line first, then by column
        match self.line.cmp(&other.line) {
            std::cmp::Ordering::Equal => self.col_start.cmp(&other.col_start),
            other => other,
        }
    }
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
        let mut byte_end = get_byte_offset(source, self.line as usize, self.col_end as usize);

        // Ensure the label span is valid (start <= end)
        if byte_end <= byte_offset {
            byte_end = byte_offset + 1;
        }

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

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(feature = "cli")]
    #[test]
    fn test_report_ariadne_does_not_panic_with_inverted_span() {
        // Bug fix: when col_start > col_end (e.g. from a multi-line LSP range
        // flattened to one line), ariadne panicked with "Label start is after its end".
        // The guard in report_ariadne should prevent this.
        let diag = Diagnostic {
            line: 0,
            col_start: 20,
            col_end: 5, // intentionally inverted
            severity: Severity::Error,
            message: "test inverted span".to_string(),
        };

        let source = "    some_field: SomeType(\n        nested: 1,\n    ),\n";
        let path = std::path::Path::new("test.ron");

        // This should not panic
        diag.report_ariadne(path, source);
    }

    #[cfg(feature = "cli")]
    #[test]
    fn test_report_ariadne_does_not_panic_with_zero_width_span() {
        let diag = Diagnostic {
            line: 0,
            col_start: 10,
            col_end: 10, // zero width
            severity: Severity::Error,
            message: "test zero width span".to_string(),
        };

        let source = "    some_field: value,\n";
        let path = std::path::Path::new("test.ron");

        // This should not panic
        diag.report_ariadne(path, source);
    }

    #[cfg(feature = "cli")]
    #[test]
    fn test_get_byte_offset() {
        let source = "line0\nline1\nline2\n";
        assert_eq!(get_byte_offset(source, 0, 0), 0);
        assert_eq!(get_byte_offset(source, 0, 3), 3);
        assert_eq!(get_byte_offset(source, 1, 0), 6);
        assert_eq!(get_byte_offset(source, 1, 2), 8);
        assert_eq!(get_byte_offset(source, 2, 0), 12);
    }
}

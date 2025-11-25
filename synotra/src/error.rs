use std::fmt;

/// Represents a span of text in the source code
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize, // byte offset
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
}

/// Error severity level
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorLevel {
    Error,
    Warning,
}

impl fmt::Display for ErrorLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorLevel::Error => write!(f, "[ERROR]"),
            ErrorLevel::Warning => write!(f, "[WARNING]"),
        }
    }
}

/// A compile-time error with position information
#[derive(Debug, Clone)]
pub struct CompileError {
    pub level: ErrorLevel,
    pub title: String,
    pub span: Option<Span>,
    pub explanation: Option<String>,
    pub suggestion: Option<String>,
}

impl CompileError {
    pub fn error(title: String) -> Self {
        CompileError {
            level: ErrorLevel::Error,
            title,
            span: None,
            explanation: None,
            suggestion: None,
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_explanation(mut self, explanation: String) -> Self {
        self.explanation = Some(explanation);
        self
    }

    pub fn with_suggestion(mut self, suggestion: String) -> Self {
        self.suggestion = Some(suggestion);
        self
    }
}

/// A runtime error
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub title: String,
    pub location: Option<String>, // e.g., "example.sy:15"
    pub context: Option<String>,
    pub suggestion: Option<String>,
}

impl RuntimeError {
    pub fn new(title: String) -> Self {
        RuntimeError {
            title,
            location: None,
            context: None,
            suggestion: None,
        }
    }

    pub fn with_location(mut self, location: String) -> Self {
        self.location = Some(location);
        self
    }

    pub fn with_context(mut self, context: String) -> Self {
        self.context = Some(context);
        self
    }

    pub fn with_suggestion(mut self, suggestion: String) -> Self {
        self.suggestion = Some(suggestion);
        self
    }
}

/// Formats errors for display
pub struct ErrorFormatter {
    source_code: String,
    filename: String,
}

impl ErrorFormatter {
    pub fn new(source_code: String, filename: String) -> Self {
        ErrorFormatter {
            source_code,
            filename,
        }
    }

    /// Format a compile error with source snippet
    pub fn format_compile_error(&self, err: &CompileError) -> String {
        let mut output = String::new();

        // Title line
        output.push_str(&format!("{} {}\n", err.level, err.title));

        // Location line
        if let Some(span) = &err.span {
            let (line, col) = self.get_line_col(span.start);
            output.push_str(&format!("  --> {}:{}:{}\n", self.filename, line, col));
            output.push('\n');

            // Source snippet
            let snippet = self.format_source_snippet(line, span);
            output.push_str(&snippet);
        }

        // Explanation
        if let Some(explanation) = &err.explanation {
            output.push('\n');
            output.push_str(&format!("  {}\n", explanation));
        }

        // Suggestion
        if let Some(suggestion) = &err.suggestion {
            output.push('\n');
            output.push_str(&format!("  help: {}\n", suggestion));
        }

        output
    }

    /// Format a runtime error
    pub fn format_runtime_error(err: &RuntimeError) -> String {
        let mut output = String::new();

        // Title line
        output.push_str(&format!("[RUNTIME ERROR] {}\n", err.title));

        // Location
        if let Some(location) = &err.location {
            output.push_str(&format!("  at {}\n", location));
        }

        // Context
        if let Some(context) = &err.context {
            output.push('\n');
            output.push_str(&format!("  {}\n", context));
        }

        // Suggestion
        if let Some(suggestion) = &err.suggestion {
            output.push('\n');
            output.push_str(&format!("  help: {}\n", suggestion));
        }

        output
    }

    /// Convert byte offset to (line_number, column_number)
    /// Line and column are 1-indexed
    fn get_line_col(&self, byte_offset: usize) -> (usize, usize) {
        let mut line = 1;
        let mut col = 1;

        for (i, ch) in self.source_code.chars().enumerate() {
            if i >= byte_offset {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }

        (line, col)
    }

    /// Get the source line at the given line number (1-indexed)
    fn get_source_line(&self, line_num: usize) -> Option<&str> {
        self.source_code.lines().nth(line_num - 1)
    }

    /// Format source snippet with error highlighting
    fn format_source_snippet(&self, line_num: usize, span: &Span) -> String {
        let mut output = String::new();

        if let Some(line_content) = self.get_source_line(line_num) {
            let (_, start_col) = self.get_line_col(span.start);
            let (end_line, end_col) = self.get_line_col(span.end);

            // Show line with line number
            output.push_str(&format!("{:5} | {}\n", line_num, line_content));

            // Show pointer
            if end_line == line_num {
                // Single line error
                let spaces = " ".repeat(start_col - 1);
                let carets = "^".repeat(end_col - start_col);
                output.push_str(&format!("      | {}{}\n", spaces, carets));
            } else {
                // Multi-line error (just underline from start to end of line)
                let spaces = " ".repeat(start_col - 1);
                let remaining = line_content.len() - (start_col - 1);
                let carets = "^".repeat(remaining);
                output.push_str(&format!("      | {}{}\n", spaces, carets));
            }
        }

        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_col_calculation() {
        let source = "line 1\nline 2\nline 3";
        let formatter = ErrorFormatter::new(source.to_string(), "test.sy".to_string());

        assert_eq!(formatter.get_line_col(0), (1, 1));
        assert_eq!(formatter.get_line_col(7), (2, 1)); // After first newline
        assert_eq!(formatter.get_line_col(14), (3, 1)); // After second newline
    }

    #[test]
    fn test_format_compile_error() {
        let source = "var x = unknownVar";
        let formatter = ErrorFormatter::new(source.to_string(), "test.sy".to_string());

        let err = CompileError::error("Undefined variable 'unknownVar'".to_string())
            .with_span(Span::new(8, 18))
            .with_suggestion("Did you mean 'knownVar'?".to_string());

        let formatted = formatter.format_compile_error(&err);
        assert!(formatted.contains("[ERROR]"));
        assert!(formatted.contains("unknownVar"));
        assert!(formatted.contains("help:"));
    }

    #[test]
    fn test_format_runtime_error() {
        let err = RuntimeError::new("Index out of bounds".to_string())
            .with_location("test.sy:15".to_string())
            .with_context("List has 3 items, but index 5 was requested".to_string())
            .with_suggestion("Valid range: 0..2".to_string());

        let formatted = ErrorFormatter::format_runtime_error(&err);
        assert!(formatted.contains("[RUNTIME ERROR]"));
        assert!(formatted.contains("at test.sy:15"));
        assert!(formatted.contains("Valid range"));
    }
}

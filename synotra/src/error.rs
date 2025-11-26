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

/// Types of compile errors with standardized messages
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    UndefinedVariable { name: String },
    UninitializedVariable { name: String },
    TypeMismatch { expected: String, found: String },
    DuplicateDefinition { name: String },
    InvalidOperation { op: String, ty: String },
    MissingReturn,
    Other { message: String },
}

impl ErrorKind {
    /// Get a user-friendly title for the error
    pub fn title(&self) -> String {
        match self {
            ErrorKind::UndefinedVariable { name } => {
                format!("Undefined variable '{}'", name)
            }
            ErrorKind::UninitializedVariable { name } => {
                format!("Use of uninitialized variable '{}'", name)
            }
            ErrorKind::TypeMismatch { expected, found } => {
                format!("Type mismatch: expected {}, found {}", expected, found)
            }
            ErrorKind::DuplicateDefinition { name } => {
                format!("Duplicate definition of '{}'", name)
            }
            ErrorKind::InvalidOperation { op, ty } => {
                format!("Cannot apply operator '{}' to type {}", op, ty)
            }
            ErrorKind::MissingReturn => "Missing return statement".to_string(),
            ErrorKind::Other { message } => message.clone(),
        }
    }

    /// Get explanation of why this error occurred
    pub fn explanation(&self) -> Option<String> {
        match self {
            ErrorKind::UndefinedVariable { name } => Some(format!(
                "The variable '{}' is not defined in this scope",
                name
            )),
            ErrorKind::UninitializedVariable { name } => Some(format!(
                "The variable '{}' is declared but has not been assigned a value yet",
                name
            )),
            ErrorKind::TypeMismatch { expected, found } => Some(format!(
                "Expected a value of type {}, but got {}",
                expected, found
            )),
            ErrorKind::DuplicateDefinition { name } => {
                Some(format!("'{}' is already defined in this scope", name))
            }
            ErrorKind::InvalidOperation { op, ty } => Some(format!(
                "The operator '{}' cannot be used with values of type {}",
                op, ty
            )),
            ErrorKind::MissingReturn => Some("Pure functions must return a value".to_string()),
            ErrorKind::Other { .. } => None,
        }
    }

    /// Get suggestion on how to fix the error
    pub fn suggestion(&self) -> Option<String> {
        match self {
            ErrorKind::UndefinedVariable { .. } => {
                Some("Make sure the variable is declared before using it".to_string())
            }
            ErrorKind::UninitializedVariable { .. } => {
                Some("Assign a value to the variable before using it".to_string())
            }
            ErrorKind::TypeMismatch { .. } => {
                Some("Check that the value's type matches what is expected".to_string())
            }
            ErrorKind::DuplicateDefinition { .. } => {
                Some("Use a different name or remove one of the definitions".to_string())
            }
            ErrorKind::InvalidOperation { .. } => Some(
                "Use a different operator or convert the value to a compatible type".to_string(),
            ),
            ErrorKind::MissingReturn => {
                Some("Add a return statement at the end of the function".to_string())
            }
            ErrorKind::Other { .. } => None,
        }
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

    /// Create an error from an ErrorKind with automatic title, explanation, and suggestion
    pub fn from_kind(kind: ErrorKind) -> Self {
        CompileError {
            level: ErrorLevel::Error,
            title: kind.title(),
            span: None,
            explanation: kind.explanation(),
            suggestion: kind.suggestion(),
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

    /// Find the position of an identifier in the source code (for span estimation)
    /// Returns the first occurrence after the given hint offset
    pub fn find_identifier(&self, identifier: &str, hint_offset: usize) -> Option<Span> {
        let bytes = self.source_code.as_bytes();

        // Search for identifier starting from hint
        for i in hint_offset..bytes.len() {
            if i + identifier.len() > bytes.len() {
                break;
            }

            // Check if we have a match
            if &bytes[i..i + identifier.len()] == identifier.as_bytes() {
                // Verify it's a complete identifier (not part of another word)
                let before_ok =
                    i == 0 || !bytes[i - 1].is_ascii_alphanumeric() && bytes[i - 1] != b'_';
                let after_ok = i + identifier.len() >= bytes.len()
                    || (!bytes[i + identifier.len()].is_ascii_alphanumeric()
                        && bytes[i + identifier.len()] != b'_');

                if before_ok && after_ok {
                    return Some(Span::new(i, i + identifier.len()));
                }
            }
        }

        None
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

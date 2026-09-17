use std::{fmt::{Display, Formatter}, path::PathBuf};

use color_print::cformat;

pub mod symbol;

#[macro_export]
macro_rules! error {
    (in $sink:expr; $span:expr => $($tt:tt)*) => {
        $sink.emit(error!($span => $($tt)*))
    };
    ($span:expr => $($tt:tt)*) => {
        ::diagnostic::Diagnostic::new_error(format!($($tt)*), $span)
    };
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticSink {
    emitted: Vec<Diagnostic>,
    fatal: bool,
}

impl DiagnosticSink {
    pub fn new() -> Self {
        Self { emitted: Vec::new(), fatal: false }
    }

    pub fn emit(&mut self, diagnostic: Diagnostic) {
        if matches!(diagnostic.level, DiagnosticLevel::Error) {
            self.fatal = true;
        }
        self.emitted.push(diagnostic);
    }

    pub fn emit_or<T>(&mut self, res: Result<T, Diagnostic>, def: T) -> T {
        match res {
            Ok(t) => t,
            Err(err) => {
                self.emit(err);
                def
            }
        }
    }

    pub fn fatal(&self) -> bool {
        self.fatal
    }

    pub fn into_emitted(self) -> Vec<Diagnostic> {
        self.emitted
    }

    pub fn display_errors(self, source_map: &SourceMap) -> String {
        self.emitted.into_iter()
            .map(|diagnostic| diagnostic.display(source_map))
            .fold(None, |acc: Option<String>, curr| {
                match acc {
                    Some(mut str) => {
                        str.push_str("\n\n");
                        str.push_str(&curr);
                        Some(str)
                    }
                    None => Some(curr),
                }
            })
            .unwrap_or(String::new())
    }
}

// TODO: DiagnosticMessage enum
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub message: String,
    pub span: Span,
    pub level: DiagnosticLevel,
}

impl Diagnostic {
    pub fn new_error(message: String, span: Span) -> Self {
        Diagnostic { message, span, level: DiagnosticLevel::Error }
    }

    pub fn new_warning(message: String, span: Span) -> Self {
        Diagnostic { message, span, level: DiagnosticLevel::Warning }
    }

    pub fn display(&self, source_map: &SourceMap) -> String {
        cformat!("<s>{}: {}</>\n  in <g>{}:{}:{}</>:\n\n{}",
            self.level.display_color(),
            self.message,
            source_map.get_source(self.span.source).0.display(),
            self.span.start.line,
            self.span.start.col,
            self.display_file_preview(source_map),
        )
    }

    fn display_file_preview(&self, source_map: &SourceMap) -> String {
        let start = self.span.start;
        let end = self.span.end;
        if start.line == 0 {
            return String::new();
        }

        let src = source_map.get_source(self.span.source).1;
        let mut lines = src.lines();
        let first_line = lines.nth((start.line - 1) as usize)
            .unwrap_or("");
        if start.line != end.line {
            let rest = lines.take((end.line - start.line) as usize)
                .fold(String::new(), |mut acc, curr| {
                    acc.push_str(&cformat!("\n<s>{}</> {curr}", self.level.in_color("|")));
                    acc
                });
            let first_spaces = "-".repeat((start.col) as usize);
            let end_spaces = "_".repeat((end.col) as usize);
            cformat!("  {first_line}\
                \n{}\
                {rest}\
                \n{}",
                self.level.in_color(cformat!("<s>/{first_spaces}^</>")),
                self.level.in_color(cformat!("<s>\\{end_spaces}^ {}</>", self.message)),
            )
        } else {
            let spaces = " ".repeat((start.col.saturating_sub(1)) as usize);
            let arrows = "^".repeat((end.col + 1 - start.col) as usize);

            cformat!("  {first_line}\n  {spaces}<s>{}</>", self.level.in_color(format!("{} {}", arrows, self.message)))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiagnosticLevel {
    Error,
    Warning,
}

impl Display for DiagnosticLevel {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Error => write!(f, "error"),
            Self::Warning => write!(f, "warning"),
        }
    }
}

impl DiagnosticLevel {
    pub fn display_color(&self) -> String {
        self.in_color(self)
    }

    pub fn in_color(&self, f: impl Display) -> String {
        match self {
            Self::Error => cformat!("<r>{f}</>"),
            Self::Warning => cformat!("<y>{f}</>"),
        }
    }
}

#[derive(Debug)]
pub struct SourceMap<'a> {
    sources: Vec<(PathBuf, &'a str)>,
}

impl<'a> SourceMap<'a> {
    pub fn new() -> Self {
        Self { sources: Vec::new() }
    }

    pub fn add_source(&mut self, path: PathBuf, source: &'a str) -> FileId {
        self.sources.push((path, source));
        FileId(self.sources.len() - 1)
    }

    pub fn get_source(&self, id: FileId) -> &(PathBuf, &'a str) {
        &self.sources[id.0]
    }
}

impl Default for SourceMap<'_> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FileId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    source: FileId,
    start: Location,
    end: Location,
}

impl Span {
    pub fn new(source: FileId, start: Location, end: Location) -> Self {
        Self { source, start, end }
    }

    pub fn new_width(source: FileId, line: u32, col: u32, width: u32) -> Self {
        Self { source, start: Location { line, col }, end: Location { line, col: col + width } }
    }

    pub fn new_single(source: FileId, line: u32, col: u32) -> Self {
        Self::new_width(source, line, col, 1)
    }

    pub fn with_end(self, end: Location) -> Self {
        Self { source: self.source, start: self.start, end }
    }

    pub fn containing(self, other: Span) -> Self {
        if self.source != other.source {
            panic!("cannot span across files")
        }
        Self { source: self.source, start: self.start.min(other.start), end: self.end.max(other.end) }
    }

    pub fn source(self) -> FileId {
        self.source
    }

    pub fn start(self) -> Location {
        self.start
    }

    pub fn end(self) -> Location {
        self.end
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location {
    pub line: u32,
    pub col: u32,
}


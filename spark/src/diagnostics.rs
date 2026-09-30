use diagnostic::{Diagnostic, DiagnosticError, Span, error, symbol::{Symbol, SymbolDisplay, SymbolTable}};
use parse::{FunName, Path, expr::{BinaryOperator, UnaryOperator}, lex::{Break, Continue, Ident, TokenKind}};

use crate::ty::TypeId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeMismatch<'t> {
    pub expected: TypeId<'t>,
    pub got: TypeId<'t>,
    pub span: Span,
}

impl DiagnosticError for TypeMismatch<'_> {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.span => "expected {}, but got {} instead", self.expected.display(symbol_table), self.got.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownIdent {
    pub ident: Ident,
}

impl DiagnosticError for UnknownIdent {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.ident.span() => "unknown identifier `{}`", self.ident.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownField<'t> {
    pub ident: Ident,
    pub ty: TypeId<'t>,
}

impl DiagnosticError for UnknownField<'_> {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.ident.span() => "field `{}` does not exist on {}", self.ident.display(symbol_table), self.ty.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownAssociated<'t> {
    pub ident: Ident,
    pub ty: TypeId<'t>,
}

impl DiagnosticError for UnknownAssociated<'_> {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.ident.span() => "associated item `{}` does not exist on {}", self.ident.display(symbol_table), self.ty.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MissingReceiver {
    pub fun: Ident,
}

impl DiagnosticError for MissingReceiver {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.fun.span() => "method `{}` is missing a receiver", self.fun.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpectedFunction(pub Span);

impl DiagnosticError for ExpectedFunction {
    fn into_diagnostic(self, _: &SymbolTable) -> Diagnostic {
        error!(self.0 => "expected function")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionArgMismatch {
    pub expected: usize,
    pub got: usize,
    pub span: Span,
}

impl DiagnosticError for FunctionArgMismatch {
    fn into_diagnostic(self, _: &SymbolTable) -> Diagnostic {
        error!(self.span => "function requires {} args, but got {} args instead", self.expected, self.got)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnnotationArgMismatch {
    pub expected: usize,
    pub got: usize,
    pub span: Span,
}

impl DiagnosticError for AnnotationArgMismatch {
    fn into_diagnostic(self, _: &SymbolTable) -> Diagnostic {
        error!(self.span => "annotation requires {} args, but got {} args instead", self.expected, self.got)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NotAStruct<'t> {
    pub ty: TypeId<'t>,
    pub span: Span,
}

impl DiagnosticError for NotAStruct<'_> {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.span => "{} is not a struct", self.ty.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CannotIndex<'t> {
    pub ty: TypeId<'t>,
    pub span: Span,
}

impl DiagnosticError for CannotIndex<'_> {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.span => "cannot index a {}", self.ty.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MissingField {
    pub field: Symbol,
    pub span: Span,
}

impl DiagnosticError for MissingField {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.span => "missing field `{}`", self.field.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeRequiredForEmptyArray(pub Span);

impl DiagnosticError for TypeRequiredForEmptyArray {
    fn into_diagnostic(self, _: &SymbolTable) -> Diagnostic {
        error!(self.0 => "empty arrays require an explicit type annotation")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NoBinOp<'t> {
    pub lhs: TypeId<'t>,
    pub op: BinaryOperator,
    pub rhs: TypeId<'t>,
}

impl DiagnosticError for NoBinOp<'_> {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.op.span() => "{} cannot be applied to {} and {}", self.op, self.lhs.display(symbol_table), self.rhs.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NoUnOp<'t> {
    pub op: UnaryOperator,
    pub ty: TypeId<'t>,
}

impl DiagnosticError for NoUnOp<'_> {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.op.span() => "{} cannot be applied to {}", self.op, self.ty.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BadAssignment(pub Span);

impl DiagnosticError for BadAssignment {
    fn into_diagnostic(self, _: &SymbolTable) -> Diagnostic {
        error!(self.0 => "bad left-hand-side of assignment")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DuplicateStructField(pub Ident);

impl DiagnosticError for DuplicateStructField {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "duplicate struct field `{}`", self.0.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodAsValue(pub Ident);

impl DiagnosticError for MethodAsValue {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "cannot use method `{}` as a value", self.0.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BadFunctionName(pub Path);

impl DiagnosticError for BadFunctionName {
    fn into_diagnostic(self, _: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "bad function name")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalAlreadyDeclared(pub Ident);

impl DiagnosticError for LocalAlreadyDeclared {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "local `{}` already declared", self.0.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrimitiveAlreadyDeclared(pub Ident);

impl DiagnosticError for PrimitiveAlreadyDeclared {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "primitive `{}` already declared", self.0.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeAlreadyDeclared(pub FunName);

impl DiagnosticError for NativeAlreadyDeclared {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "native `{}` already declared", self.0.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UninitVarNeedsType(pub Ident);

impl DiagnosticError for UninitVarNeedsType {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "variable `{}` needs an explicit type", self.0.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContinueOutsideLoop(pub Continue);

impl DiagnosticError for ContinueOutsideLoop {
    fn into_diagnostic(self, _: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "`{}` not allowed outside loops", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BreakOutsideLoop(pub Break);

impl DiagnosticError for BreakOutsideLoop {
    fn into_diagnostic(self, _: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "`{}` not allowed outside loops", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DuplicateFunArg(pub Ident);

impl DiagnosticError for DuplicateFunArg {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "duplicate function argument `{}`", self.0.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownAnnotation(pub Ident);

impl DiagnosticError for UnknownAnnotation {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "unknown annotation `{}`", self.0.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownStdType(pub Ident);

impl DiagnosticError for UnknownStdType {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "unknown std type `{}`", self.0.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructAlreadyDeclared(pub Ident);

impl DiagnosticError for StructAlreadyDeclared {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "struct `{}` already declared", self.0.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunAlreadyDeclared(pub FunName);

impl DiagnosticError for FunAlreadyDeclared {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "function `{}` already declared", self.0.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemNotFound(pub Ident);

impl DiagnosticError for ItemNotFound {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "item `{}` not found", self.0.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownPrimitive(pub Ident);

impl DiagnosticError for UnknownPrimitive {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "unknown primitive `{}`", self.0.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownNative(pub FunName);

impl DiagnosticError for UnknownNative {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "unknown native `{}`", self.0.display(symbol_table))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MismatchedNativeArgs(pub Span);

impl DiagnosticError for MismatchedNativeArgs {
    fn into_diagnostic(self, _symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.0 => "mismatched native args")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MismatchedNativeRet(pub Span);

impl DiagnosticError for MismatchedNativeRet {
    fn into_diagnostic(self, _symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.0 => "mismatched native return type")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuiltinAlreadyDeclared(pub Ident);

impl DiagnosticError for BuiltinAlreadyDeclared {
    fn into_diagnostic(self, symbol_table: &SymbolTable) -> Diagnostic {
        error!(self.0.span() => "builtin `{}` already declared", self.0.display(symbol_table))
    }
}


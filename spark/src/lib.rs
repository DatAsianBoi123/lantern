use arena::Arena;
use diagnostic::{DiagnosticError, DiagnosticSink, Span, symbol::{SymbolDisplay, SymbolTable}};
use parse::{FunArg, Item, ItemFun, ItemNativeFun, ItemPrimitive, ItemStruct, LanternFile, ReturnStmt, StructField, ValDeclaration, WhileStmt, expr::{self as pe, BinaryOperator, ExprArray, ExprBinary, ExprBlock, ExprField, ExprFunCall, ExprIndex, ExprMethodCall, ExprParen, ExprStruct, ExprUnary}, lex::{self, TokenKind}};

use crate::{def::{LanternFunction, LanternStruct, LanternStructField}, diagnostics::*, expr::{Expr, ExprKind, Literal, LogicalOperation}, native::{FromDefError, NativeFun}, scope::{Globals, Scope, ScopeKind}, stmt::{IfBranch, IfStmt, Stmt}, ty::{BuiltinType, LanternType, TypeContext, TypeId}};

pub mod ty;
pub mod scope;
pub mod stmt;
pub mod expr;
pub mod def;
pub mod primitive;
pub mod native;
pub mod diagnostics;

pub fn lower<'t>(file: LanternFile, arena: &'t Arena<LanternType<'t>>, symbol_table: &SymbolTable) -> (Option<Spark<'t>>, DiagnosticSink) {
    let mut lighter = Lighter::new(symbol_table, arena);
    lighter.lower_module(file);
    lighter.compile()
}

#[non_exhaustive]
#[derive(Debug)]
pub struct Spark<'t> {
    pub globals: Globals<'t>,
    pub tcx: TypeContext<'t>,
}

#[derive(Debug)]
pub struct Lighter<'a, 't> {
    globals: Globals<'t>,
    sink: DiagnosticSink,
    symbol_table: &'a SymbolTable<'a>,
    tcx: TypeContext<'t>,
}

impl<'a, 't> Lighter<'a, 't> {
    pub fn new(symbol_table: &'a SymbolTable<'a>, arena: &'t Arena<LanternType<'t>>) -> Self {
        Self {
            globals: Globals::new(),
            sink: DiagnosticSink::new(),
            symbol_table,
            tcx: TypeContext::new(arena),
        }
    }

    pub fn compile(self) -> (Option<Spark<'t>>, DiagnosticSink) {
        if self.sink.fatal() {
            (None, self.sink)
        } else {
            (Some(Spark { globals: self.globals, tcx: self.tcx }), self.sink)
        }
    }

    pub fn lower_module(&mut self, file: LanternFile) -> usize {
        let mut module_scope = Scope::new_module(&self.tcx);
        let lowered = self.lower_stmts(file.stmts, &mut module_scope);
        let entry = self.globals.funs.len();
        self.globals.funs.push(SparkFunction::Lantern {
            name: "<module>".to_string(),
            stmts: lowered,
            locals: module_scope.max_locals
        });
        entry
    }

    pub fn lower_stmts(&mut self, stmts: Vec<parse::Stmt>, scope: &mut Scope<'_, 't>) -> Vec<Stmt<'t>> {
        let mut spark_stmts = Vec::new();

        self.resolve_types(&stmts, scope);
        self.build_types_and_funs(&stmts, scope);

        for stmt in stmts {
            match stmt {
                parse::Stmt::Item(Item::Fun(ItemFun { name, args, ret, block, .. })) => {
                    let ret = ret
                        .map(|(_, ret)| self.sink.emit_or(LanternType::resolve(&ret, scope, &self.tcx), self.tcx.error()))
                        .unwrap_or(self.tcx.null());

                    let fun_def = match &name.base {
                        Some(base) if let Ok(ty) = LanternType::resolve(base, scope, &self.tcx)
                            && let Some(fun) = scope.associated(ty, name.ident.0) =>
                        {
                            fun
                        }
                        None if let Some(fun) = scope.function(name.ident.0) => fun,
                        _ => continue,
                    };

                    let mut fun_scope = scope.child_function(name.span(), ret);
                    fun_def.args.iter().zip(args).for_each(|((arg_name, ty), fun_arg)| {
                        if fun_scope.insert_variable(*arg_name, *ty).is_none() {
                            self.emit(DuplicateFunArg(fun_arg.ident));
                        }
                    });
                    let stmts = self.lower_stmts(block.stmts, &mut fun_scope);
                    self.globals.funs[fun_def.index] = SparkFunction::Lantern {
                        name: name.display(self.symbol_table),
                        stmts,
                        locals: fun_scope.max_locals,
                    };
                }
                parse::Stmt::Item(_) => {}
                parse::Stmt::IfStmt(if_stmt) => spark_stmts.push(Stmt::If(self.check_if(if_stmt, scope))),
                parse::Stmt::WhileStmt(WhileStmt { condition, block, .. }) => {
                    let cond_span = condition.span();
                    let cond = self.lower_expr(condition, scope);
                    if !cond.ty.is_error_or_eq(self.tcx.primitive(&primitive::BOOL_PRIMITIVE)) {
                        self.emit(TypeMismatch {
                            expected: self.tcx.primitive(&primitive::BOOL_PRIMITIVE),
                            got: cond.ty,
                            span: cond_span,
                        });
                    }

                    // don't check to see if the child scope diverges since it's not guaranteed the
                    // condition is met in the first place
                    let mut loop_scope = scope.child_loop();
                    let stmts = self.lower_stmts(block.stmts, &mut loop_scope);
                    scope.max_locals = scope.max_locals.max(loop_scope.max_locals);
                    spark_stmts.push(Stmt::While { cond, stmts });
                }
                parse::Stmt::ValDeclaration(ValDeclaration { ident, r#type, init: Some((_, init)), .. }) => {
                    let init_span = init.span();
                    let init = self.lower_expr(init, scope);
                    let ty = r#type
                        .map(|(_, ty)| {
                            let ty = self.sink.emit_or(LanternType::resolve(&ty, scope, &self.tcx), self.tcx.error());
                            if !init.ty.is_error_or_eq(ty) {
                                self.emit(TypeMismatch {
                                    expected: ty,
                                    got: init.ty,
                                    span: init_span,
                                });
                            }
                            ty
                        })
                        .unwrap_or(init.ty);
                    match scope.insert_variable(ident.0, ty) {
                        Some(index) => spark_stmts.push(Stmt::Val(index, Some(init))),
                        None => self.emit(LocalAlreadyDeclared(ident)),
                    }
                }
                parse::Stmt::ValDeclaration(ValDeclaration { ident, r#type, .. }) => {
                    // TODO: ensure uninitialized vars are initialized before usage
                    let ty = r#type
                        .map(|(_, ty)| self.sink.emit_or(LanternType::resolve(&ty, scope, &self.tcx), self.tcx.error()))
                        .unwrap_or_else(|| {
                            self.emit(UninitVarNeedsType(ident));
                            self.tcx.error()
                        });
                    match scope.insert_variable(ident.0, ty) {
                        Some(index) => spark_stmts.push(Stmt::Val(index, None)),
                        None => self.emit(LocalAlreadyDeclared(ident)),
                    }
                }
                parse::Stmt::Return(ReturnStmt { ret, expr, .. }) => {
                    let span = expr.as_ref()
                        .map(|expr| expr.span())
                        .unwrap_or(ret.span());
                    let expr = expr.map(|expr| self.lower_expr(expr, scope));
                    let ty = expr.as_ref().map(|expr| expr.ty).unwrap_or(self.tcx.null());
                    if !ty.is_error_or_eq(scope.expected_ret) {
                        self.emit(TypeMismatch {
                            expected: scope.expected_ret,
                            got: ty,
                            span,
                        });
                    }

                    scope.diverges = true;
                    spark_stmts.push(Stmt::Return(expr));
                }
                parse::Stmt::Continue(_, _) if scope.in_loop => spark_stmts.push(Stmt::Continue),
                parse::Stmt::Continue(cont, _) => self.emit(ContinueOutsideLoop(cont)),
                parse::Stmt::Break(_, _) if scope.in_loop => spark_stmts.push(Stmt::Break),
                parse::Stmt::Break(r#break, _) => self.emit(BreakOutsideLoop(r#break)),
                parse::Stmt::Throw(_, expr, _) => {
                    let expr_span = expr.span();
                    let expr = self.lower_expr(expr, scope);
                    if !expr.ty.is_error_or_eq(self.tcx.builtin(BuiltinType::String)) {
                        self.emit(TypeMismatch {
                            expected: self.tcx.builtin(BuiltinType::String),
                            got: expr.ty,
                            span: expr_span,
                        });
                    }
                    scope.diverges = true;
                    spark_stmts.push(Stmt::Throw(expr));
                }
                parse::Stmt::Expr(expr, _) => spark_stmts.push(Stmt::Expr(self.lower_expr(expr, scope))),
            }
        }

        match scope.kind() {
            ScopeKind::Function(_, span) if !scope.diverges => {
                if !self.tcx.null().is_error_or_eq(scope.expected_ret) {
                    self.emit(TypeMismatch {
                        expected: scope.expected_ret,
                        got: self.tcx.null(),
                        span: *span,
                    });
                }
                spark_stmts.push(Stmt::Return(None));
            }
            ScopeKind::Module => spark_stmts.push(Stmt::Return(None)),
            _ => {}
        }

        spark_stmts
    }

    pub fn lower_expr(&mut self, expr: pe::Expr, scope: &mut Scope<'_, 't>) -> Expr<'t> {
        match expr {
            pe::Expr::Literal(lex::Literal::Integer(int, span)) => {
                Expr::new(ExprKind::Literal(Literal::Int(int)), self.tcx.primitive(&primitive::INT_PRIMITIVE), span)
            }
            pe::Expr::Literal(lex::Literal::Float(float, span)) => {
                Expr::new(ExprKind::Literal(Literal::Float(float)), self.tcx.primitive(&primitive::FLOAT_PRIMITIVE), span)
            }
            pe::Expr::Literal(lex::Literal::True(span)) => {
                Expr::new(ExprKind::Literal(Literal::True), self.tcx.primitive(&primitive::BOOL_PRIMITIVE), span)
            }
            pe::Expr::Literal(lex::Literal::False(span)) => {
                Expr::new(ExprKind::Literal(Literal::False), self.tcx.primitive(&primitive::BOOL_PRIMITIVE), span)
            }
            pe::Expr::Literal(lex::Literal::String(str, span)) => {
                let global = self.globals.vars.insert_str(str.into_boxed_str());
                Expr::new(ExprKind::Global(global), self.tcx.builtin(BuiltinType::String), span)
            }
            pe::Expr::Identifier(ident) => {
                if let Some(var) = scope.variable(ident.0) {
                    Expr::new(ExprKind::Local(var.index), var.ty, ident.span())
                } else if let Some(fun) = scope.function(ident.0) {
                    Expr::new(ExprKind::Static(fun.index), fun.ty, ident.span())
                } else {
                    self.emit(UnknownIdent { ident });
                    Expr::error(&self.tcx, ident.span())
                }
            }
            pe::Expr::Field(ExprField { expr, ident }) => {
                // Struct.static_function
                if let Some(ty) = self.item_static(&expr, scope) {
                    let Some(associated) = scope.associated(ty, ident.0) else {
                        self.emit(UnknownAssociated { ident, ty });
                        return Expr::error(&self.tcx, ident.span());
                    };
                    return Expr::new(ExprKind::Static(associated.index), associated.ty, ident.span());
                }

                let base = self.lower_expr(*expr, scope);
                if *base.ty == LanternType::Error {
                    return Expr::error(&self.tcx, ident.span());
                }
                match *base.ty {
                    LanternType::Struct(ref r#struct) => {
                        if let Some(LanternStructField { offset, ty, .. }) = r#struct.find_field(ident.0) {
                            return Expr::new(ExprKind::Field(Box::new(base), offset), ty, ident.span())
                        }
                    }
                    LanternType::Array(_) if self.symbol_table.resolve(ident.0) == "len" => {
                        return Expr::new(ExprKind::Len(Box::new(base)), self.tcx.primitive(&primitive::INT_PRIMITIVE), ident.span())
                    }
                    _ => {}
                }

                if scope.associated(base.ty, ident.0).is_some() {
                    self.emit(MethodAsValue(ident));
                } else {
                    self.emit(UnknownField { ident, ty: base.ty });
                }
                Expr::error(&self.tcx, ident.span())
            }
            pe::Expr::FunCall(ExprFunCall { expr, args, closed_paren, .. }) => {
                let span = expr.span();
                let fun = self.lower_expr(*expr, scope);

                match &*fun.ty {
                    LanternType::Function { args: fun_args, ret } => {
                        let arg_exprs = self.check_fun(args, fun_args, span, scope);
                        let ret = *ret;
                        Expr::new(ExprKind::Call(Box::new(fun), arg_exprs), ret, closed_paren.span())
                    }
                    LanternType::Error => {
                        // still typecheck the args
                        for arg in args {
                            self.lower_expr(arg, scope);
                        }
                        Expr::error(&self.tcx, closed_paren.span())
                    }
                    _ => {
                        self.emit(ExpectedFunction(span));
                        Expr::error(&self.tcx, closed_paren.span())
                    }
                }
            }
            pe::Expr::MethodCall(ExprMethodCall { expr, ident, args, closed_paren, .. }) => {
                let (fun, recv) = match self.item_static(&expr, scope) {
                    Some(ty) => {
                        let Some(associated) = scope.associated(ty, ident.0) else {
                            self.emit(UnknownAssociated { ident, ty });
                            return Expr::error(&self.tcx, ident.span());
                        };
                        (associated, None)
                    }
                    None => {
                        let recv = self.lower_expr(*expr, scope);
                        if *recv.ty == LanternType::Error {
                            // still typecheck the args
                            for arg in args {
                                self.lower_expr(arg, scope);
                            }
                            return Expr::error(&self.tcx, closed_paren.span());
                        }

                        let Some(method) = scope.associated(recv.ty, ident.0) else {
                            // TODO: diagnostic hint for (obj.field)()
                            self.emit(UnknownAssociated { ident, ty: recv.ty });
                            return Expr::error(&self.tcx, closed_paren.span());
                        };

                        if !method.has_receiver(recv.ty) {
                            self.emit(MissingReceiver { fun: ident });
                            // still typecheck the args
                            for arg in args {
                                self.lower_expr(arg, scope);
                            }
                            return Expr::error(&self.tcx, closed_paren.span());
                        }

                        (method, Some(recv))
                    }
                };

                let fun_index = fun.index;
                let fun_ret = fun.ret;
                if let Some(recv) = recv {
                    // skip the receiver arg
                    let method_args = fun.args.iter()
                        .skip(1)
                        .map(|(_, ty)| *ty)
                        .collect::<Vec<_>>();
                    let arg_exprs = self.check_fun(args, &method_args, ident.span(), scope);
                    Expr::new(ExprKind::CallMethod(Box::new(recv), fun_index, arg_exprs), fun_ret, closed_paren.span())
                } else {
                    let fun_ty = fun.ty;
                    let fun_args = fun.args.iter()
                        .map(|(_, ty)| *ty)
                        .collect::<Vec<_>>();
                    let arg_exprs = self.check_fun(args, &fun_args, ident.span(), scope);
                    let fun_expr = Expr::new(ExprKind::Static(fun_index), fun_ty, ident.span());
                    Expr::new(ExprKind::Call(Box::new(fun_expr), arg_exprs), fun_ret, closed_paren.span())
                }
            }
            pe::Expr::Struct(ExprStruct { ident, fields, closed_brace, .. }) => {
                let Some(item) = scope.item(ident.0) else {
                    self.emit(UnknownIdent { ident });
                    return Expr::error(&self.tcx, closed_brace.span());
                };
                let LanternType::Struct(ref r#struct) = *item else {
                    self.emit(NotAStruct { ty: item, span: ident.span() });
                    return Expr::error(&self.tcx, closed_brace.span());
                };

                let mut init_fields = Vec::new();
                let mut field_exprs = Vec::new();
                for field in fields {
                    match r#struct.find_field(field.ident.0) {
                        Some(struct_field) => {
                            let span = field.expr.span();
                            let field_expr = self.lower_expr(field.expr, scope);
                            if !field_expr.ty.is_error_or_eq(struct_field.ty) {
                                self.emit(TypeMismatch {
                                    expected: struct_field.ty,
                                    got: field_expr.ty,
                                    span,
                                });
                            }
                            if init_fields.contains(&field.ident.0) {
                                self.emit(DuplicateStructField(field.ident));
                            } else {
                                init_fields.push(field.ident.0);
                                field_exprs.push((field_expr, struct_field.offset));
                            }
                        }
                        None => self.emit(UnknownField { ident: field.ident, ty: item }),
                    }
                }

                let struct_fields = r#struct.data().fields();
                if init_fields.len() != struct_fields.len() {
                    struct_fields.iter()
                        .filter(|field| !init_fields.contains(&field.name))
                        .for_each(|field| self.emit(MissingField { field: field.name, span: ident.span() }));
                }

                Expr::new(ExprKind::Struct(r#struct.id, field_exprs), item, closed_brace.span())
            }
            pe::Expr::Paren(ExprParen { expr, .. }) => self.lower_expr(*expr, scope),
            pe::Expr::Block(block) => {
                let span = block.span();
                let (stmts, diverges) = self.check_block(block, scope);
                if diverges {
                    scope.diverges = true;
                }
                Expr::new(ExprKind::Block(stmts), self.tcx.null(), span)
            }
            pe::Expr::Array(ExprArray { open_bracket, elements, closed_bracket, ty }) => {
                let mut ty = ty.map(|ty| self.sink.emit_or(LanternType::resolve(&ty, scope, &self.tcx), self.tcx.error()));

                let mut element_exprs = Vec::new();
                for element in elements {
                    let span = element.span();
                    let element_expr = self.lower_expr(element, scope);
                    let ty = *ty.get_or_insert(element_expr.ty);
                    if !element_expr.ty.is_error_or_eq(ty) {
                        self.emit(TypeMismatch {
                            expected: ty,
                            got: element_expr.ty,
                            span,
                        });
                    }
                    element_exprs.push(element_expr);
                }

                match ty {
                    Some(ty) if *ty == LanternType::Error => Expr::error(&self.tcx, closed_bracket.span()),
                    Some(ty) => Expr::new(ExprKind::Array(ty, element_exprs), self.tcx.intern(LanternType::Array(ty)), closed_bracket.span()),
                    None => {
                        self.emit(TypeRequiredForEmptyArray(open_bracket.span().containing(closed_bracket.span())));
                        Expr::error(&self.tcx, closed_bracket.span())
                    }
                }
            }
            pe::Expr::Index(ExprIndex { expr, index, closed_bracket, .. }) => {
                let expr_span = expr.span();
                let base = self.lower_expr(*expr, scope);

                let inner = match *base.ty {
                    LanternType::Array(inner) => inner,
                    LanternType::Error => self.tcx.error(),
                    _ => {
                        self.emit(CannotIndex { ty: base.ty, span: expr_span });
                        self.tcx.error()
                    }
                };

                let index_span = index.span();
                let index = self.lower_expr(*index, scope);
                if !index.ty.is_error_or_eq(self.tcx.primitive(&primitive::INT_PRIMITIVE)) {
                    self.emit(TypeMismatch {
                        expected: self.tcx.primitive(&primitive::INT_PRIMITIVE),
                        got: index.ty,
                        span: index_span,
                    });
                }

                Expr::new(ExprKind::Index(Box::new(base), Box::new(index)), inner, closed_bracket.span())
            }
            pe::Expr::Binary(ExprBinary { lhs, op, rhs }) => {
                let lhs_span = lhs.span();
                let rhs_span = rhs.span();
                let lhs = self.lower_expr(*lhs, scope);
                let rhs = self.lower_expr(*rhs, scope);

                // special cases
                match op {
                    BinaryOperator::And(_) | BinaryOperator::Or(_) => {
                        if !lhs.ty.is_primitive_type(&primitive::BOOL_PRIMITIVE) || !rhs.ty.is_primitive_type(&primitive::BOOL_PRIMITIVE) {
                            self.emit(NoBinOp {
                                lhs: lhs.ty,
                                op,
                                rhs: rhs.ty,
                            });
                        }

                        let logical = match op {
                            BinaryOperator::And(_) => LogicalOperation::And,
                            BinaryOperator::Or(_) => LogicalOperation::Or,
                            _ => unreachable!(),
                        };
                        return Expr::new(
                            ExprKind::Logical(Box::new(lhs), logical, Box::new(rhs)),
                            self.tcx.primitive(&primitive::BOOL_PRIMITIVE),
                            rhs_span
                        );
                    }
                    BinaryOperator::Assign(_) => {
                        if !lhs.kind.is_place() {
                            self.emit(BadAssignment(lhs_span));
                        }
                        if !lhs.ty.is_error_or_eq(rhs.ty) {
                            self.emit(TypeMismatch {
                                expected: lhs.ty,
                                got: rhs.ty,
                                span: rhs_span,
                            });
                        }
                        return Expr::new(ExprKind::Assign(Box::new(lhs), Box::new(rhs)), self.tcx.null(), rhs_span);
                    }
                    BinaryOperator::AddAssign(_)
                    | BinaryOperator::SubAssign(_)
                    | BinaryOperator::MultAssign(_)
                    | BinaryOperator::DivAssign(_)
                    | BinaryOperator::ModAssign(_) => {
                        if !lhs.kind.is_place() {
                            self.emit(BadAssignment(lhs_span));
                        }
                        match (&*lhs.ty, &*rhs.ty) {
                            (LanternType::Primitive(lhs_primitive), LanternType::Primitive(_))
                                if let Some(op) = lhs_primitive.ops.get_bin_op(op) =>
                            {
                                return Expr::new(ExprKind::BinaryAssign(Box::new(lhs), op, Box::new(rhs)), self.tcx.null(), rhs_span);
                            }
                            _ => {
                                self.emit(NoBinOp {
                                    lhs: lhs.ty,
                                    op,
                                    rhs: rhs.ty,
                                });
                                return Expr::error(&self.tcx, rhs_span);
                            }
                        }
                    }
                    _ => {}
                }

                if *lhs.ty == LanternType::Error || *rhs.ty == LanternType::Error {
                    return Expr::error(&self.tcx, rhs_span);
                }
                if lhs.ty != rhs.ty {
                    self.emit(TypeMismatch {
                        expected: lhs.ty,
                        got: rhs.ty,
                        span: op.span(),
                    });
                    return Expr::error(&self.tcx, rhs_span);
                }
                match (&*lhs.ty, &*rhs.ty) {
                    (LanternType::Primitive(lhs_primitive), LanternType::Primitive(_))
                        if op.is_comparison() && let Some(op) = lhs_primitive.ops.get_bin_op(op) =>
                    {
                        Expr::new(ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)), self.tcx.primitive(&primitive::BOOL_PRIMITIVE), rhs_span)
                    }
                    (LanternType::Primitive(lhs_primitive), LanternType::Primitive(_))
                        if let Some(op) = lhs_primitive.ops.get_bin_op(op) =>
                    {
                        let ty = lhs.ty;
                        Expr::new(ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)), ty, rhs_span)
                    }
                    _ => {
                        self.emit(NoBinOp {
                            lhs: lhs.ty,
                            op,
                            rhs: rhs.ty,
                        });
                        Expr::error(&self.tcx, rhs_span)
                    }
                }
            }
            pe::Expr::Unary(ExprUnary { op, expr }) => {
                let span = expr.span();
                let base = self.lower_expr(*expr, scope);
                match &*base.ty {
                    LanternType::Primitive(primitive) if let Some(op) = primitive.ops.get_un_op(op) => {
                        let ty = base.ty;
                        Expr::new(ExprKind::Unary(op, Box::new(base)), ty, span)
                    }
                    _ => {
                        self.emit(NoUnOp {
                            ty: base.ty,
                            op,
                        });
                        Expr::error(&self.tcx, span)
                    }
                }
            }
        }
    }

    fn resolve_types(&mut self, stmts: &[parse::Stmt], scope: &mut Scope<'_, 't>) {
        stmts.iter()
            .filter_map(|stmt| match stmt {
                parse::Stmt::Item(item) => Some(item),
                _ => None,
            })
            .for_each(|item| match item {
                Item::Using(_) => todo!(),
                Item::Struct(ItemStruct { annotations, ident, .. }) => {
                    let r#struct = LanternStruct::new(ident.0, self.globals.types.len());

                    let ty = self.tcx.intern(LanternType::Struct(r#struct));
                    for annotation in &annotations.annotations {
                        if self.symbol_table.resolve(annotation.ident.0) == "std" {
                            if let Some(args) = &annotation.args && args.args.len() == 1 {
                                let arg = &args.args[0];
                                let successful_link = match self.symbol_table.resolve(arg.0) {
                                    "string" => self.tcx.link_builtin(BuiltinType::String, ty),
                                    _ => {
                                        self.emit(UnknownStdType(*arg));
                                        true
                                    }
                                };
                                if !successful_link {
                                    self.emit(BuiltinAlreadyDeclared(*ident));
                                }
                            } else {
                                self.emit(AnnotationArgMismatch {
                                    expected: 1,
                                    got: annotation.args.as_ref().map_or_default(|args| args.args.len()),
                                    span: ident.span(),
                                });
                            }
                        } else {
                            self.emit(UnknownAnnotation(annotation.ident));
                        }
                    }

                    if scope.insert_item(ident.0, ty).is_none() {
                        // BUG: duplicate items get diagnostics on all except the first, while the
                        // actual struct is built on the last
                        self.emit(StructAlreadyDeclared(*ident));
                        return;
                    }

                    let LanternType::Struct(r#struct) = ty.as_ty() else { unreachable!() };
                    self.globals.types.push(&r#struct.data);
                }
                Item::Primitive(ItemPrimitive { ident, .. }) => {
                    if let Some(primitive) = primitive::get_primitive(self.symbol_table.resolve(ident.0)) {
                        if scope.insert_item(ident.0, self.tcx.primitive(primitive)).is_none() {
                            self.emit(PrimitiveAlreadyDeclared(*ident));
                        }
                    } else {
                        self.emit(UnknownPrimitive(*ident));
                    }
                }
                _ => {},
            });
    }

    fn build_types_and_funs(&mut self, stmts: &[parse::Stmt], scope: &mut Scope<'_, 't>) {
        stmts.iter()
            .filter_map(|stmt| match stmt {
                parse::Stmt::Item(item) => Some(item),
                _ => None,
            })
            .for_each(|item| match item {
                    Item::Fun(ItemFun { name, args, ret, .. }) => {
                        let args = args.iter()
                            .map(|FunArg { ident, r#type, .. }| {
                                (ident.0, self.sink.emit_or(LanternType::resolve(r#type, scope, &self.tcx), self.tcx.error()))
                            })
                            .collect();

                        let ret = ret.as_ref()
                            .map(|(_, ty)| self.sink.emit_or(LanternType::resolve(ty, scope, &self.tcx), self.tcx.error()))
                            .unwrap_or(self.tcx.null());

                        let fun = LanternFunction::new(self.globals.funs.len(), args, ret, &self.tcx);
                        match &name.base {
                            Some(ty) => {
                                match LanternType::resolve(ty, scope, &self.tcx) {
                                    Ok(ty) => {
                                        if scope.insert_associated(ty, name.ident.0, fun).is_none() {
                                            self.emit(FunAlreadyDeclared(name.clone()));
                                            return;
                                        }
                                    }
                                    Err(err) => {
                                        self.sink.emit(err);
                                        return;
                                    }
                                }
                            }
                            None => {
                                if scope.insert_function(name.ident.0, fun).is_none() {
                                    self.emit(FunAlreadyDeclared(name.clone()));
                                    return;
                                }
                            }
                        }
                        // this gets overridden when the function is generated
                        self.globals.funs.push(SparkFunction::Lantern {
                            name: String::new(),
                            stmts: Vec::new(),
                            locals: 0,
                        });
                    }
                    Item::NativeFun(ItemNativeFun { name, open_paren, args, closed_paren, ret, semi, .. }) => {
                        let base = name.base.as_ref()
                            .map(|ty| self.sink.emit_or(LanternType::resolve(ty, scope, &self.tcx), self.tcx.error()));

                        let mut arg_types = Vec::new();
                        let mut has_err = false;
                        for arg in args {
                            match LanternType::resolve(&arg.r#type, scope, &self.tcx) {
                                Ok(ty) => arg_types.push(ty),
                                Err(err) => {
                                    self.sink.emit(err);
                                    has_err = true;
                                }
                            }
                        }

                        let ret_ty = ret.as_ref()
                            .map(|(_, ty)| self.sink.emit_or(LanternType::resolve(ty, scope, &self.tcx), self.tcx.error()))
                            .unwrap_or(self.tcx.null());

                        if has_err || *ret_ty == LanternType::Error || base.is_some_and(|ty| *ty == LanternType::Error) {
                            return;
                        }

                        let fun_name = name.ident.0;
                        let args = args.iter().zip(arg_types.iter())
                            .map(|(arg, ty)| (arg.ident.0, *ty))
                            .collect();
                        let fun = LanternFunction::new(self.globals.funs.len(), args, ret_ty, &self.tcx);

                        let native = match NativeFun::from_def(base, self.symbol_table.resolve(fun_name), &arg_types, ret_ty, &self.tcx) {
                            Ok(native) => Some(native),
                            Err(FromDefError::NotFound) => {
                                self.emit(UnknownNative(name.clone()));
                                None
                            }
                            Err(FromDefError::MismatchedArgs) => {
                                self.emit(MismatchedNativeArgs(open_paren.span().containing(closed_paren.span())));
                                None
                            }
                            Err(FromDefError::MismatchedRet) => {
                                let span = ret.as_ref().map_or(semi.span(), |(_, ty)| ty.span());
                                self.emit(MismatchedNativeRet(span));
                                None
                            }
                        };

                        let exists = match base {
                            Some(base) => scope.insert_associated(base, fun_name, fun),
                            None => scope.insert_function(fun_name, fun),
                        };
                        if exists.is_none() {
                            self.emit(NativeAlreadyDeclared(name.clone()));
                        } else {
                            if let Some(native) = native {
                                self.globals.funs.push(SparkFunction::Native {
                                    name: name.display(self.symbol_table),
                                    native,
                                });
                            } else {
                                // "dummy" function
                                self.globals.funs.push(SparkFunction::Lantern {
                                    name: name.display(self.symbol_table),
                                    stmts: Vec::new(),
                                    locals: 0,
                                });
                            }
                        }
                    }
                    Item::Struct(ItemStruct { ident, fields, .. }) => {
                        let fields = fields.iter()
                            .map(|StructField { ident, r#type, .. }| {
                                // type may not have fields initialized, but structs have constant
                                // size/alignment and primitives are hardcoded
                                (ident.0, self.sink.emit_or(LanternType::resolve(r#type, scope, &self.tcx), self.tcx.error()))
                            })
                            .collect();

                        let item = scope.item(ident.0).expect("types were resolved");
                        let LanternType::Struct(ref r#struct) = *item else { panic!("resolved type not a struct") };
                        // self.globals.types holds a reference to the OnceCell
                        r#struct.init(fields);
                    }
                    _ => {}
            });
    }

    fn check_if(&mut self, if_stmt: parse::IfStmt, scope: &mut Scope<'_, 't>) -> IfStmt<'t> {
        let (if_stmt, diverges) = self.check_if_stmt(if_stmt, scope);
        if diverges {
            scope.diverges = true;
        }
        if_stmt
    }

    fn check_if_stmt(&mut self, if_stmt: parse::IfStmt, scope: &mut Scope<'_, 't>) -> (IfStmt<'t>, bool) {
        let parse::IfStmt { condition, block, branch, .. } = if_stmt;

        let condition_span = condition.span();
        let cond = self.lower_expr(condition, scope);
        if !cond.ty.is_error_or_eq(self.tcx.primitive(&primitive::BOOL_PRIMITIVE)) {
            self.emit(TypeMismatch {
                expected: self.tcx.primitive(&primitive::BOOL_PRIMITIVE),
                got: cond.ty,
                span: condition_span,
            });
        }

        let (stmts, block_diverges) = self.check_block(block, scope);

        let (branch, rest_diverges) = match branch {
            Some((_, branch)) => match *branch {
                parse::IfBranch::ElseIf(if_stmt) => {
                    let (if_stmt, diverges) = self.check_if_stmt(*if_stmt, scope);
                    (Some(IfBranch::ElseIf(Box::new(if_stmt))), diverges)
                }
                parse::IfBranch::Else(block) => {
                    let (stmts, diverges) = self.check_block(block, scope);
                    (Some(IfBranch::Else(stmts)), diverges)
                }
            },
            // without an `else` the chain can always fall through
            None => (None, false),
        };

        (IfStmt { cond, stmts, branch }, block_diverges && rest_diverges)
    }

    fn check_block(&mut self, block: ExprBlock, scope: &mut Scope<'_, 't>) -> (Vec<Stmt<'t>>, bool) {
        let mut block_scope = scope.child_block();
        let stmts = self.lower_stmts(block.stmts, &mut block_scope);
        let diverges = block_scope.diverges;
        scope.max_locals = scope.max_locals.max(block_scope.max_locals);
        (stmts, diverges)
    }

    fn check_fun(&mut self, args: Vec<pe::Expr>, fun_args: &[TypeId<'t>], span: Span, scope: &mut Scope<'_, 't>) -> Vec<Expr<'t>> {
        if fun_args.len() != args.len() {
            self.emit(FunctionArgMismatch {
                expected: fun_args.len(),
                got: args.len(),
                span,
            });
        }

        let mut fun_args_iter = fun_args.iter();
        let mut arg_exprs = Vec::new();
        for arg in args {
            let span = arg.span();
            let arg_expr = self.lower_expr(arg, scope);
            if let Some(fun_arg) = fun_args_iter.next() && !arg_expr.ty.is_error_or_eq(*fun_arg) {
                self.emit(TypeMismatch {
                    expected: *fun_arg,
                    got: arg_expr.ty,
                    span,
                });
            }
            arg_exprs.push(arg_expr);
        }
        arg_exprs
    }

    fn item_static(&self, expr: &pe::Expr, scope: &Scope<'_, 't>) -> Option<TypeId<'t>> {
        match expr {
            pe::Expr::Identifier(ident) if let Some(item) = scope.item(ident.0) && scope.variable(ident.0).is_none() => Some(item),
            _ => None,
        }
    }

    fn emit(&mut self, diag: impl DiagnosticError) {
        self.sink.emit(diag.into_diagnostic(self.symbol_table));
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SparkFunction<'t> {
    Lantern {
        name: String,
        stmts: Vec<Stmt<'t>>,
        locals: usize,
    },
    Native {
        name: String,
        native: NativeFun,
    },
}


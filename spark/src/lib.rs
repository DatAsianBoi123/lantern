use std::{collections::HashMap, fs::File, io::Read};

use arena::Arena;
use diagnostic::{DiagnosticError, DiagnosticSink, FileId, SourceMap, Span, error, symbol::{SymbolDisplay, SymbolTable}, warning};
use parse::{FunArg, FunSig, Item, ItemFun, ItemNativeFun, ItemNativeType, ItemStruct, ItemUsing, LanternFile, MatchPattern, MatchStmt, PassStmt, ReturnStmt, Stmt as PStmt, StructField, UsingItems, ValDeclaration, WhileStmt, expr::{BinaryOperator, Expr as PExpr, ExprArray, ExprBinary, ExprField, ExprFunCall, ExprIndex, ExprMethodCall, ExprParen, ExprStruct, ExprUnary}, lex::{self, TokenKind}};

use crate::{def::{LanternFunction, LanternStruct, LanternStructField}, diagnostics::*, expr::{Expr, ExprKind, Literal, LogicalOperation}, native::{BOOL_NATIVE, FLOAT_NATIVE, FromDefError, INT_NATIVE, NativeFun, STR_NATIVE}, scope::{Globals, ModuleExports, PassBehavior, Scope, ScopeBehavior, ScopeKind}, stmt::{IfBranch, IfStmt, Stmt}, ty::{LanternType, TypeContext, TypeId}};

pub mod ty;
pub mod scope;
pub mod stmt;
pub mod expr;
pub mod def;
pub mod native;
pub mod diagnostics;

pub fn lower<'a, 't>(
    file: LanternFile,
    arena: &'t Arena<LanternType<'t>>,
    symbol_table: &'a mut SymbolTable,
    source_map: &'a mut SourceMap,
) -> (Option<Spark<'t>>, DiagnosticSink) {
    let mut lighter = Lighter::new(symbol_table, source_map, arena);
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
pub struct Lighter<'a, 's, 't> {
    globals: Globals<'t>,
    sink: DiagnosticSink,
    symbol_table: &'s mut SymbolTable,
    source_map: &'a mut SourceMap,
    tcx: TypeContext<'t>,
    exports: HashMap<FileId, ModuleExports<'t>>,
}

impl<'a, 's, 't> Lighter<'a, 's, 't> {
    pub fn new(symbol_table: &'s mut SymbolTable, source_map: &'a mut SourceMap, arena: &'t Arena<LanternType<'t>>) -> Self {
        Self {
            globals: Globals::new(),
            sink: DiagnosticSink::new(),
            symbol_table,
            source_map,
            tcx: TypeContext::new(arena),
            exports: HashMap::new(),
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
        let (lowered, behavior) = self.lower_stmts(file.stmts, Scope::new_module(&self.tcx));
        let entry = self.globals.funs.len();
        self.globals.funs.push(SparkFunction::Lantern {
            name: "<module>".to_string(),
            stmts: lowered,
            locals: behavior.locals_used,
        });
        entry
    }

    #[must_use]
    pub fn lower_stmts(&mut self, mut stmts: Vec<PStmt>, mut scope: Scope<'_, 't>) -> (Vec<Stmt<'t>>, ScopeBehavior<'t>) {
        let mut spark_stmts = Vec::new();

        // TODO: optimize looping through all statements every pass
        self.resolve_types(&mut stmts, &mut scope);
        self.resolve_usings(&mut stmts, &mut scope);
        self.build_types_and_funs(&stmts, &mut scope);

        for stmt in stmts {
            match stmt {
                PStmt::Item(Item::Fun(ItemFun { sig: FunSig { name, args, .. }, block, .. })) => {
                    let fun_def = match &name.base {
                        Some(base) if let Ok(ty) = LanternType::resolve(base, &scope, &self.tcx)
                            && let Some(fun) = scope.associated(ty, name.ident.0) =>
                        {
                            fun
                        }
                        None if let Some(fun) = scope.function(name.ident.0) => fun,
                        _ => continue,
                    };

                    let mut fun_scope = scope.child_function(block.closed_brace.span(), fun_def.ret);
                    fun_def.args.iter().zip(args).for_each(|((arg_name, ty), fun_arg)| {
                        if fun_scope.insert_variable(*arg_name, *ty).is_none() {
                            self.emit(DuplicateFunArg(fun_arg.ident));
                        }
                    });
                    let (stmts, behavior) = self.lower_stmts(block.stmts, fun_scope);
                    self.globals.funs[fun_def.index] = SparkFunction::Lantern {
                        name: name.display(self.symbol_table),
                        stmts,
                        locals: behavior.locals_used,
                    };
                }
                PStmt::Item(_) => {}
                PStmt::IfStmt(if_stmt) => spark_stmts.push(Stmt::If(self.check_if(if_stmt, &mut scope))),
                PStmt::MatchStmt(MatchStmt { expr, arms, .. }) => {
                    let span = expr.span();
                    let expr = self.lower_expr(expr, &mut scope);
                    let inner = match *expr.ty {
                        LanternType::Optional(inner) => inner,
                        LanternType::Error => self.tcx.error(),
                        _ => {
                            self.emit(CannotMatch { ty: expr.ty, span });
                            self.tcx.error()
                        }
                    };

                    let mut some_arm = None;
                    let mut none_arm = None;
                    let mut overall_behavior = None;

                    for arm in arms {
                        match arm.pat {
                            MatchPattern::Ident(ident) => {
                                if some_arm.is_some() {
                                    self.emit(UnreachableArm(arm.span()));
                                }

                                let mut block_scope = scope.child_block();
                                let some_local = block_scope.insert_variable(ident.0, inner).expect("scope initially has no locals");
                                let (stmts, behavior) = self.lower_stmts(arm.block.stmts, block_scope);
                                scope.inherit_locals(behavior);
                                overall_behavior = Some(match overall_behavior {
                                    Some(overall) => behavior.combine_branch(overall),
                                    None => behavior
                                });

                                some_arm.get_or_insert((stmts, some_local));
                            }
                            MatchPattern::None(_) => {
                                if none_arm.is_some() {
                                    self.emit(UnreachableArm(arm.span()));
                                }

                                let (stmts, behavior) = self.lower_stmts(arm.block.stmts, scope.child_block());
                                scope.inherit_locals(behavior);
                                overall_behavior = Some(match overall_behavior {
                                    Some(overall) => overall.combine_branch(behavior),
                                    None => behavior
                                });

                                none_arm.get_or_insert(stmts);
                            }
                        }
                    }

                    if let Some(behavior) = overall_behavior {
                        scope.inherit(behavior);
                    }

                    if let (Some((some_arm, some_local)), Some(none_arm)) = (some_arm, none_arm) {
                        spark_stmts.push(Stmt::Match { expr, some_local, some_arm, none_arm });
                    } else {
                        self.emit(NonExhaustiveMatch(span));
                    }
                }
                PStmt::WhileStmt(WhileStmt { condition, block, .. }) => {
                    let cond_span = condition.span();
                    let cond = self.lower_expr(condition, &mut scope);
                    if !cond.ty.is_applicable(self.tcx.primitive(&BOOL_NATIVE)) {
                        self.emit(TypeMismatch {
                            expected: self.tcx.primitive(&BOOL_NATIVE),
                            got: cond.ty,
                            span: cond_span,
                        });
                    }

                    // don't check to see if the child scope diverges since it's not guaranteed the
                    // condition is met in the first place
                    let (stmts, behavior) = self.lower_stmts(block.stmts, scope.child_loop());
                    scope.inherit_locals(behavior);
                    spark_stmts.push(Stmt::While { cond, stmts });
                }
                PStmt::ValDeclaration(ValDeclaration { ident, r#type, init: Some((_, init)), .. }) => {
                    let init_span = init.span();
                    let init = self.lower_expr(init, &mut scope);
                    let ty = r#type
                        .map(|(_, ty)| {
                            let ty = self.sink.emit_or(LanternType::resolve(&ty, &scope, &self.tcx), self.tcx.error());
                            if !init.ty.is_applicable(ty) {
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
                PStmt::ValDeclaration(ValDeclaration { ident, r#type, .. }) => {
                    // TODO: ensure uninitialized vars are initialized before usage
                    self.sink.emit(warning!(ident.span() => "unitialized variables are not fully supported, use at your own risk"));
                    let ty = r#type
                        .map(|(_, ty)| self.sink.emit_or(LanternType::resolve(&ty, &scope, &self.tcx), self.tcx.error()))
                        .unwrap_or_else(|| {
                            self.emit(UninitVarNeedsType(ident));
                            self.tcx.error()
                        });
                    match scope.insert_variable(ident.0, ty) {
                        Some(index) => spark_stmts.push(Stmt::Val(index, None)),
                        None => self.emit(LocalAlreadyDeclared(ident)),
                    }
                }
                PStmt::Return(ReturnStmt { ret, expr, .. }) => {
                    let span = expr.as_ref()
                        .map(|expr| expr.span())
                        .unwrap_or(ret.span());
                    let expr = expr
                        .map(|expr| self.lower_expr(expr, &mut scope))
                        .unwrap_or(Expr::none(&self.tcx, ret.span()));

                    if !expr.ty.is_applicable(scope.expected_ret) {
                        self.emit(TypeMismatch {
                            expected: scope.expected_ret,
                            got: expr.ty,
                            span,
                        });
                    }

                    scope.diverges = true;
                    spark_stmts.push(Stmt::Return(Some(expr)));
                }
                PStmt::Pass(PassStmt { pass, expr, .. }) => {
                    let span = expr.as_ref()
                        .map(|expr| expr.span())
                        .unwrap_or(pass.span());
                    let expr = expr
                        .map(|expr| self.lower_expr(expr, &mut scope))
                        .unwrap_or(Expr::new(ExprKind::Literal(Literal::None), self.tcx.none(), pass.span()));

                    if let ScopeKind::Block { in_expr, pass, .. } = scope.kind_mut() && *in_expr {
                        if let Some(ty) = pass.ty() && !expr.ty.is_applicable(ty) {
                            self.emit(TypeMismatch {
                                expected: ty,
                                got: expr.ty,
                                span,
                            });
                        }
                        *pass = pass.upgrade(expr.ty);
                        spark_stmts.push(Stmt::Pass(expr));
                    } else {
                        self.emit(PassOutsideBlock(pass));
                    }
                }
                PStmt::Continue(_, _) if scope.in_loop() => spark_stmts.push(Stmt::Continue),
                PStmt::Continue(cont, _) => self.emit(ContinueOutsideLoop(cont)),
                PStmt::Break(_, _) if scope.in_loop() => spark_stmts.push(Stmt::Break),
                PStmt::Break(r#break, _) => self.emit(BreakOutsideLoop(r#break)),
                PStmt::Throw(_, expr, _) => {
                    let expr_span = expr.span();
                    let expr = self.lower_expr(expr, &mut scope);
                    if !expr.ty.is_applicable(self.tcx.primitive(&STR_NATIVE)) {
                        self.emit(TypeMismatch {
                            expected: self.tcx.primitive(&STR_NATIVE),
                            got: expr.ty,
                            span: expr_span,
                        });
                    }
                    scope.diverges = true;
                    spark_stmts.push(Stmt::Throw(expr));
                }
                PStmt::Expr(expr, _) => spark_stmts.push(Stmt::Expr(self.lower_expr(expr, &mut scope))),
            }
        }

        match scope.kind() {
            ScopeKind::Function(_, span) if !scope.diverges => {
                if !self.tcx.none().is_applicable(scope.expected_ret) {
                    self.emit(TypeMismatch {
                        expected: scope.expected_ret,
                        got: self.tcx.none(),
                        span: *span,
                    });
                }
                spark_stmts.push(Stmt::Return(None));
            }
            ScopeKind::Module => spark_stmts.push(Stmt::Return(None)),
            _ => {}
        }

        (spark_stmts, scope.into_behavior())
    }

    pub fn lower_expr(&mut self, expr: PExpr, scope: &mut Scope<'_, 't>) -> Expr<'t> {
        match expr {
            PExpr::Literal(lex::Literal::None(span)) => {
                Expr::none(&self.tcx, span)
            }
            PExpr::Literal(lex::Literal::Integer(int, span)) => {
                Expr::new(ExprKind::Literal(Literal::Int(int)), self.tcx.primitive(&INT_NATIVE), span)
            }
            PExpr::Literal(lex::Literal::Float(float, span)) => {
                Expr::new(ExprKind::Literal(Literal::Float(float)), self.tcx.primitive(&FLOAT_NATIVE), span)
            }
            PExpr::Literal(lex::Literal::True(span)) => {
                Expr::new(ExprKind::Literal(Literal::True), self.tcx.primitive(&BOOL_NATIVE), span)
            }
            PExpr::Literal(lex::Literal::False(span)) => {
                Expr::new(ExprKind::Literal(Literal::False), self.tcx.primitive(&BOOL_NATIVE), span)
            }
            PExpr::Literal(lex::Literal::String(str, span)) => {
                let global = self.globals.vars.insert_str(str.into_boxed_str());
                Expr::new(ExprKind::Global(global), self.tcx.primitive(&STR_NATIVE), span)
            }
            PExpr::Identifier(ident) => {
                if let Some(var) = scope.variable(ident.0) {
                    Expr::new(ExprKind::Local(var.index), var.ty, ident.span())
                } else if let Some(fun) = scope.function(ident.0) {
                    Expr::new(ExprKind::Static(fun.index), fun.ty, ident.span())
                } else {
                    self.emit(UnknownIdent { ident });
                    Expr::error(&self.tcx, ident.span())
                }
            }
            PExpr::Field(ExprField { expr, ident }) => {
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
                            // TODO: check if the field is accessible
                            return Expr::new(ExprKind::Field(Box::new(base), offset), ty, ident.span())
                        }
                    }
                    LanternType::Array(_) if self.symbol_table.resolve(ident.0) == "len" => {
                        return Expr::new(ExprKind::Len(Box::new(base)), self.tcx.primitive(&INT_NATIVE), ident.span())
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
            PExpr::FunCall(ExprFunCall { expr, args, closed_paren, .. }) => {
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
            PExpr::MethodCall(ExprMethodCall { expr, ident, args, closed_paren, .. }) => {
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
            PExpr::Struct(ExprStruct { ident, fields, closed_brace, .. }) => {
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
                            if !field_expr.ty.is_applicable(struct_field.ty) {
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
            PExpr::Paren(ExprParen { expr, .. }) => self.lower_expr(*expr, scope),
            PExpr::Block(block) => {
                let span = block.span();
                let (stmts, behavior) = self.lower_stmts(block.stmts, scope.child_block_expr());
                if let PassBehavior::KnownType(ty) = behavior.pass && !self.tcx.none().is_applicable(ty) {
                    self.emit(TypeMismatch {
                        expected: ty,
                        got: self.tcx.none(),
                        span: block.closed_brace.span(),
                    })
                }
                scope.inherit(behavior.strip_pass());
                Expr::new(ExprKind::Block(stmts), behavior.pass.ty().unwrap_or(self.tcx.none()), span)
            }
            PExpr::Array(ExprArray { open_bracket, elements, closed_bracket, ty }) => {
                let mut ty = ty.map(|ty| self.sink.emit_or(LanternType::resolve(&ty, scope, &self.tcx), self.tcx.error()));

                let mut element_exprs = Vec::new();
                for element in elements {
                    let span = element.span();
                    let element_expr = self.lower_expr(element, scope);
                    let ty = *ty.get_or_insert(element_expr.ty);
                    if !element_expr.ty.is_applicable(ty) {
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
                    Some(ty) if ty.needs_tag() => {
                        error!(in self.sink; open_bracket.span().containing(closed_bracket.span()) => "optional primitives are currently not supported as array elements");
                        Expr::error(&self.tcx, closed_bracket.span())
                    }
                    Some(ty) => Expr::new(ExprKind::Array(ty, element_exprs), self.tcx.intern(LanternType::Array(ty)), closed_bracket.span()),
                    None => {
                        self.emit(TypeRequiredForEmptyArray(open_bracket.span().containing(closed_bracket.span())));
                        Expr::error(&self.tcx, closed_bracket.span())
                    }
                }
            }
            PExpr::Index(ExprIndex { expr, index, closed_bracket, .. }) => {
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
                if !index.ty.is_applicable(self.tcx.primitive(&INT_NATIVE)) {
                    self.emit(TypeMismatch {
                        expected: self.tcx.primitive(&INT_NATIVE),
                        got: index.ty,
                        span: index_span,
                    });
                }

                Expr::new(ExprKind::Index(Box::new(base), Box::new(index)), inner, closed_bracket.span())
            }
            PExpr::Binary(ExprBinary { lhs, op, rhs }) => {
                let lhs_span = lhs.span();
                let rhs_span = rhs.span();
                let lhs = self.lower_expr(*lhs, scope);
                let rhs = self.lower_expr(*rhs, scope);

                // special cases
                match op {
                    BinaryOperator::And(_) | BinaryOperator::Or(_) => {
                        if !lhs.ty.is_primitive_type(&BOOL_NATIVE) || !rhs.ty.is_primitive_type(&BOOL_NATIVE) {
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
                            self.tcx.primitive(&BOOL_NATIVE),
                            rhs_span
                        );
                    }
                    BinaryOperator::Assign(_) => {
                        if !lhs.kind.is_place() {
                            self.emit(BadAssignment(lhs_span));
                        }
                        if !rhs.ty.is_applicable(lhs.ty) {
                            self.emit(TypeMismatch {
                                expected: lhs.ty,
                                got: rhs.ty,
                                span: rhs_span,
                            });
                        }
                        return Expr::new(ExprKind::Assign(Box::new(lhs), Box::new(rhs)), self.tcx.none(), rhs_span);
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
                            (LanternType::Native(lhs_primitive), LanternType::Native(_))
                                if let Some(op) = lhs_primitive.ops.get_bin_op(op) =>
                            {
                                return Expr::new(ExprKind::BinaryAssign(Box::new(lhs), op, Box::new(rhs)), self.tcx.none(), rhs_span);
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
                        span: rhs_span,
                    });
                    return Expr::error(&self.tcx, rhs_span);
                }
                match (&*lhs.ty, &*rhs.ty) {
                    (LanternType::Native(lhs_primitive), LanternType::Native(_))
                        if op.is_comparison() && let Some(op) = lhs_primitive.ops.get_bin_op(op) =>
                    {
                        Expr::new(ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)), self.tcx.primitive(&BOOL_NATIVE), rhs_span)
                    }
                    (LanternType::Native(lhs_primitive), LanternType::Native(_))
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
            PExpr::Unary(ExprUnary { op, expr }) => {
                let span = expr.span();
                let base = self.lower_expr(*expr, scope);
                match &*base.ty {
                    LanternType::Native(primitive) if let Some(op) = primitive.ops.get_un_op(op) => {
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

    fn resolve_types(&mut self, stmts: &mut Vec<PStmt>, scope: &mut Scope<'_, 't>) -> ModuleExports<'t> {
        let mut exports = ModuleExports::new();
        // use retain to delete duplicate items
        stmts.retain(|stmt| {
            match stmt {
                PStmt::Item(Item::Struct(ItemStruct { vis, annotations, ident, .. })) => {
                    let r#struct = LanternStruct::new(ident.0, self.globals.types.len());

                    for annotation in &annotations.annotations {
                        if self.symbol_table.resolve(annotation.ident.0) == "std" {
                            if let Some(args) = &annotation.args && args.args.len() == 1 {
                                self.emit(UnknownStdType(args.args[0]));
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

                    if scope.is_duplicate_item(ident.0) {
                        self.emit(StructAlreadyDeclared(*ident));
                        return false;
                    }

                    let ty = self.tcx.intern(LanternType::Struct(r#struct));
                    scope.insert_item(ident.0, ty);
                    if vis.is_public() {
                        exports.types.insert(ident.0, ty);
                    }

                    let LanternType::Struct(r#struct) = ty.as_ty() else { unreachable!() };
                    self.globals.types.push(&r#struct.data);
                    true
                }
                PStmt::Item(Item::NativeType(ItemNativeType { vis, ident, .. })) => {
                    if let Some(primitive) = native::get_native(self.symbol_table.resolve(ident.0)) {
                        let ty = self.tcx.primitive(primitive);
                        if scope.insert_item(ident.0, ty).is_none() {
                            self.emit(PrimitiveAlreadyDeclared(*ident));
                            return false;
                        }
                        if vis.is_public() {
                            exports.types.insert(ident.0, ty);
                        }
                        true
                    } else {
                        self.emit(UnknownPrimitive(*ident));
                        false
                    }
                }
                _ => true,
            }
        });

        exports
    }

    fn resolve_usings(&mut self, stmts: &mut Vec<PStmt>, scope: &mut Scope<'_, 't>) {
        for stmt in stmts {
            if let PStmt::Item(Item::Using(ItemUsing { path, items, .. })) = stmt {
                if !matches!(items, UsingItems::Glob) {
                    error!(in self.sink; path.span() => "non-glob imports are currently not supported");
                    continue;
                }
                let path_span = path.span();
                let mut path_buf = self.source_map.root().to_path_buf();
                for item in &path.items {
                    path_buf.push(self.symbol_table.resolve(item.0));
                }
                path_buf.set_extension("la");
                let file_id = match self.source_map.find_id(&path_buf) {
                    Some(file_id) => file_id,
                    None => {
                        let Ok(mut file) = File::open(&path_buf) else {
                            self.emit(ModuleNotFound(path_buf, path_span));
                            continue;
                        };
                        let mut contents = String::new();
                        if file.read_to_string(&mut contents).is_err() {
                            self.emit(InvalidTextError(path_buf, path_span));
                            continue;
                        }
                        self.source_map.add_source(path_buf, contents)
                    }
                };
                if !self.exports.contains_key(&file_id) {
                    let mut lantern_file = match parse::parse(file_id, &self.source_map.get_source(file_id).1, self.symbol_table) {
                        Ok(lantern_file) => lantern_file,
                        Err(diagnostic) => {
                            self.sink.emit(diagnostic);
                            return;
                        }
                    };
                    let mut module_scope = Scope::new_module(&self.tcx);
                    let exports = self.resolve_types(&mut lantern_file.stmts, &mut module_scope);
                    self.exports.insert(file_id, exports);
                    self.resolve_usings(&mut lantern_file.stmts, &mut module_scope);
                    self.build_types_and_funs(&lantern_file.stmts, &mut module_scope);
                    // TODO: lower function bodies
                }
                scope.import_all(self.exports.get(&file_id).unwrap());
            }
        }
    }

    fn build_types_and_funs(&mut self, stmts: &[PStmt], scope: &mut Scope<'_, 't>) {
        stmts.iter()
            .filter_map(|stmt| match stmt {
                PStmt::Item(item) => Some(item),
                _ => None,
            })
            .for_each(|item| match item {
                    Item::Fun(ItemFun { sig: FunSig { name, args, ret, .. }, .. }) => {
                        let args = args.iter()
                            .map(|FunArg { ident, r#type, .. }| {
                                (ident.0, self.sink.emit_or(LanternType::resolve(r#type, scope, &self.tcx), self.tcx.error()))
                            })
                            .collect();

                        let ret = ret.as_ref()
                            .map(|(_, ty)| self.sink.emit_or(LanternType::resolve(ty, scope, &self.tcx), self.tcx.error()))
                            .unwrap_or(self.tcx.none());

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
                    Item::NativeFun(ItemNativeFun { sig: FunSig { name, open_paren, args, closed_paren, ret }, semi, .. }) => {
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
                            .unwrap_or(self.tcx.none());

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
                            .map(|StructField { vis, ident, r#type, .. }| {
                                // type may not have fields initialized, but structs have constant
                                // size/alignment and primitives are hardcoded
                                let ty = self.sink.emit_or(LanternType::resolve(r#type, scope, &self.tcx), self.tcx.error());
                                if ty.needs_tag() {
                                    error!(in self.sink; r#type.span() => "optional primitives are currently not supported as struct fields");
                                }
                                (*vis, ident.0, ty)
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
        let (if_stmt, behavior) = self.check_if_stmt(if_stmt, scope);
        scope.inherit(behavior);
        if_stmt
    }

    fn check_if_stmt(
        &mut self,
        parse::IfStmt { condition, block, branch, ..}: parse::IfStmt,
        scope: &mut Scope<'_, 't>,
    ) -> (IfStmt<'t>, ScopeBehavior<'t>) {
        let condition_span = condition.span();
        let cond = self.lower_expr(condition, scope);
        if !cond.ty.is_applicable(self.tcx.primitive(&BOOL_NATIVE)) {
            self.emit(TypeMismatch {
                expected: self.tcx.primitive(&BOOL_NATIVE),
                got: cond.ty,
                span: condition_span,
            });
        }

        let (stmts, block_behavior) = self.lower_stmts(block.stmts, scope.child_block());
        scope.inherit_locals(block_behavior);

        let (branch, rest_behavior) = match branch {
            Some((_, branch)) => match *branch {
                parse::IfBranch::ElseIf(if_stmt) => {
                    let (if_stmt, behavior) = self.check_if_stmt(*if_stmt, scope);
                    (Some(IfBranch::ElseIf(Box::new(if_stmt))), behavior)
                }
                parse::IfBranch::Else(block) => {
                    let (stmts, behavior) = self.lower_stmts(block.stmts, scope.child_block());
                    (Some(IfBranch::Else(stmts)), behavior)
                }
            }
            // without an `else` the chain can always fall through
            None => (None, block_behavior.guaranteed_converges()),
        };

        (IfStmt { cond, stmts, branch }, block_behavior.combine_branch(rest_behavior))
    }

    fn check_fun(&mut self, args: Vec<PExpr>, fun_args: &[TypeId<'t>], span: Span, scope: &mut Scope<'_, 't>) -> Vec<Expr<'t>> {
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
            if let Some(fun_arg) = fun_args_iter.next() && !arg_expr.ty.is_applicable(*fun_arg) {
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

    fn item_static(&self, expr: &PExpr, scope: &Scope<'_, 't>) -> Option<TypeId<'t>> {
        match expr {
            PExpr::Identifier(ident) if let Some(item) = scope.item(ident.0) && scope.variable(ident.0).is_none() => Some(item),
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


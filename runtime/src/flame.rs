use diagnostic::Span;
use instruction::InstructionSet;
use spark::{Spark, SparkFunction, expr::{ExprKind as SExpr, LogicalOperation}, scope::GlobalVariables, stmt::Stmt as SStmt, ty::BuiltinType};

use crate::{Slot, VM, error::{RuntimeError, StacktraceLocation}, flame::frame::{LineMap, LoopScope, StackFrame}, heap::{HeapArray, HeapObject, TypeInfo}, inst};

pub type NativeFn = fn(&mut VM) -> Result<Slot, RuntimeError>;

pub mod instruction;
pub mod frame;
pub mod native;

pub fn ignite(spark: Spark<'_>, globals: &mut Globals) -> [usize; BuiltinType::SIZE] {
    let mut r#gen = FlameGen::new(globals);

    for fun in spark.globals.funs {
        r#gen.compile_spark_fun(fun);
    }

    for lantern_struct in spark.globals.types {
        r#gen.globals.types.push(lantern_struct.get().expect("all type data is initialized").clone().into());
    }

    r#gen.globals.vars = spark.globals.vars;

    spark.tcx.into_builtins(r#gen.type_offset)
}

#[derive(Debug)]
pub struct FlameGen<'a> {
    pub frame: StackFrame,
    pub globals: &'a mut Globals,
    pub type_offset: usize,
}

impl<'a> FlameGen<'a> {
    pub fn new(globals: &'a mut Globals) -> Self {
        Self {
            frame: StackFrame::new_module(),
            type_offset: globals.types.len(),
            globals,
        }
    }

    pub fn using_frame<F: FnOnce(&mut Self)>(&mut self, mut frame: StackFrame, fun: F) -> GeneratedFunction {
        std::mem::swap(&mut self.frame, &mut frame);
        fun(self);
        std::mem::swap(&mut self.frame, &mut frame);
        frame.into_gen()
    }

    pub fn compile_spark_fun(&mut self, fun: SparkFunction) {
        let generated = match fun {
            SparkFunction::Lantern { name, stmts, locals } => {
                self.using_frame(StackFrame::new(name.to_string(), locals), |this| this.compile_spark_stmts(&stmts))
            }
            SparkFunction::Native { name, native } => {
                GeneratedFunction::new(name.into_boxed_str(), FunctionKind::Native(native::get_native_spark_fn(native)))
            }
        };
        self.globals.funs.push(generated);
    }

    pub fn compile_spark_stmts(&mut self, stmts: &[SStmt]) {
        for stmt in stmts {
            match stmt {
                SStmt::If(if_stmt) => {
                    let mut end_indices = Vec::new();
                    self.compile_if_stmt(if_stmt, &mut end_indices);
                    for index in end_indices {
                        self.frame.instructions[index] = inst!(GOTO self.frame.instructions.len());
                    }
                },
                SStmt::While { cond, stmts } => {
                    let head = self.frame.instructions.len();
                    self.frame.loop_context.scopes.push(LoopScope::new(head));

                    self.compile_spark_expr(cond);

                    let goto_index = self.frame.instructions.len();
                    inst!(self.frame.instructions; POP_GOTO_IF_FALSE 0);

                    self.compile_spark_stmts(stmts);
                    inst!(self.frame.instructions; GOTO head);

                    let end = self.frame.instructions.len();
                    self.frame.instructions[goto_index] = inst!(POP_GOTO_IF_FALSE end);
                    let while_scope = self.frame.loop_context.scopes.pop().expect("in loop");
                    for break_index in while_scope.breaks {
                        self.frame.instructions[break_index] = inst!(GOTO end);
                    }
                }
                // TODO: span data
                SStmt::Val(id, init) => {
                    if let Some(init) = init {
                        self.compile_spark_expr(init);
                    } else {
                        inst!(self.frame.instructions; PUSHU 0);
                    }
                    inst!(self.frame.instructions; STORE_LOCAL *id);
                }
                SStmt::Return(expr) => {
                    if let Some(expr) = expr {
                        self.compile_spark_expr(expr);
                    } else {
                        inst!(self.frame.instructions; PUSHU 0);
                    }
                    inst!(self.frame.instructions; RET);
                }
                // TODO: span data
                SStmt::Continue => {
                    let loop_scope = self.frame.loop_context.scopes.last().expect("`continue` in a loop");
                    inst!(self.frame.instructions; GOTO loop_scope.head);
                }
                // TODO: span data
                SStmt::Break => {
                    let loop_scope = self.frame.loop_context.scopes.last_mut().expect("`break` in a loop");
                    loop_scope.breaks.push(self.frame.instructions.len());
                    inst!(self.frame.instructions; GOTO 0);
                }
                SStmt::Throw(expr) => {
                    self.compile_spark_expr(expr);
                    inst!(self.frame.instructions; THRW);
                }
                SStmt::Expr(expr) => {
                    self.compile_spark_expr(expr);
                    inst!(self.frame.instructions; POP);
                }
            }
        }
    }

    pub fn compile_if_stmt(&mut self, if_stmt: &spark::stmt::IfStmt, end_indices: &mut Vec<usize>) {
        self.compile_spark_expr(&if_stmt.cond);
        let goto_index = self.frame.instructions.len();
        inst!(self.frame.instructions; POP_GOTO_IF_FALSE 0);

        self.compile_spark_stmts(&if_stmt.stmts);
        end_indices.push(self.frame.instructions.len());
        inst!(self.frame.instructions; GOTO 0);

        self.frame.instructions[goto_index] = inst!(POP_GOTO_IF_FALSE self.frame.instructions.len());

        match &if_stmt.branch {
            Some(spark::stmt::IfBranch::ElseIf(if_stmt)) => self.compile_if_stmt(if_stmt, end_indices),
            Some(spark::stmt::IfBranch::Else(stmts)) => self.compile_spark_stmts(stmts),
            None => {}
        }
    }

    pub fn compile_spark_expr(&mut self, expr: &spark::expr::Expr) {
        match &expr.kind {
            SExpr::Literal(spark::expr::Literal::Int(int)) => inst!(with self.frame => expr.span; PUSHI *int),
            SExpr::Literal(spark::expr::Literal::Float(float)) => inst!(with self.frame => expr.span; PUSHF *float),
            SExpr::Literal(spark::expr::Literal::True) => inst!(with self.frame => expr.span; PUSHU crate::bool_to_slot(true)),
            SExpr::Literal(spark::expr::Literal::False) => inst!(with self.frame => expr.span; PUSHU crate::bool_to_slot(false)),
            SExpr::Static(id) => inst!(with self.frame => expr.span; PUSHU *id),
            SExpr::Global(id) => inst!(with self.frame => expr.span; LOAD_GLOBAL *id),
            SExpr::Local(id) => inst!(with self.frame => expr.span; LOAD_LOCAL *id),
            SExpr::Block(_) => todo!(),
            SExpr::Field(obj, offset) => {
                self.compile_spark_expr(obj);
                let size = if expr.ty.is_primitive() { expr.ty.size() } else { 0 };
                inst! { with self.frame => expr.span;
                    [PUSHU HeapObject::field_offset() + *offset]
                    [READ size]
                }
            }
            SExpr::Len(array) => {
                self.compile_spark_expr(array);
                inst! { with self.frame => expr.span;
                    [PUSHU HeapArray::len_offset()]
                    [READ size_of::<i64>()]
                }
            }
            SExpr::Index(array, index) => {
                self.compile_spark_expr(array);
                self.compile_spark_expr(index);
                inst!(with self.frame => expr.span; INDEX)
            }
            SExpr::Binary(lhs, op, rhs) => {
                self.compile_spark_expr(lhs);
                self.compile_spark_expr(rhs);
                inst!(with self.frame => expr.span);
                self.frame.instructions.push((*op).into());
            }
            SExpr::BinaryAssign(lhs, op, rhs) => {
                let place = self.compile_place(lhs);
                for _ in 0..place.operands() {
                    inst!(self.frame.instructions; DUP place.operands() - 1);
                }
                self.read_place(place, lhs.span);
                self.compile_spark_expr(rhs);
                self.frame.instructions.push((*op).into());
                self.write_place(place, expr.span);
            }
            SExpr::Unary(op, value) => {
                self.compile_spark_expr(value);
                inst!(with self.frame => expr.span);
                self.frame.instructions.push((*op).into());
            }
            SExpr::Logical(lhs, logical, rhs) => {
                self.compile_spark_expr(lhs);
                let goto_index = self.frame.instructions.len();
                inst!(self.frame.instructions; POP);

                inst!(self.frame.instructions; POP);
                self.compile_spark_expr(rhs);

                match logical {
                    LogicalOperation::And => self.frame.instructions[goto_index] = inst!(GOTO_IF_FALSE self.frame.instructions.len()),
                    LogicalOperation::Or => self.frame.instructions[goto_index] = inst!(GOTO_IF_TRUE self.frame.instructions.len()),
                }
            }
            SExpr::Assign(place, value) => {
                let place = self.compile_place(place);
                self.compile_spark_expr(value);
                self.write_place(place, expr.span);
            }
            SExpr::Call(fun, args) => {
                self.compile_spark_expr(fun);
                for arg in args {
                    self.compile_spark_expr(arg);
                }
                inst!(with self.frame => expr.span; INV args.len());
            }
            SExpr::CallMethod(recv, id, args) => {
                self.compile_spark_expr(recv);
                inst!(with self.frame => expr.span; PUSHU *id);
                for arg in args {
                    self.compile_spark_expr(arg);
                }
                inst!(with self.frame => expr.span; INV_MET args.len());
            }
            SExpr::Struct(id, fields) => {
                // TODO: better span information here
                inst!(self.frame.instructions; ALLOC_OBJ (*id + self.type_offset));
                for (field, offset) in fields {
                    inst!(with self.frame => field.span; PUSHU (HeapObject::field_offset() + *offset));
                    self.compile_spark_expr(field);
                    inst!(with self.frame => field.span; WRITE field.ty.size());
                }
            }
            SExpr::Array(ty, elements) => {
                for element in elements {
                    self.compile_spark_expr(element);
                }
                let id = if ty.is_ref() { VM::REF_ARR_TYPE_INDEX } else { VM::PRIMITIVE_ARR_TYPE_INDEX };
                inst!(with self.frame => expr.span; ALLOC_ARR id, elements.len());
            }
            SExpr::Error => panic!("error expression encountered"),
        }
    }

    pub fn compile_place<'p>(&mut self, place: &spark::expr::Expr<'p>) -> Place<'p> {
        match &place.kind {
            SExpr::Local(id) => Place::Local(*id),
            SExpr::Field(base, offset) => {
                self.compile_spark_expr(base);
                inst!(with self.frame => base.span; PUSHU (HeapObject::field_offset() + *offset));
                Place::Field(place.ty)
            }
            SExpr::Index(array, index) => {
                self.compile_spark_expr(array);
                self.compile_spark_expr(index);
                Place::Index
            }
            _ => panic!("attempting to write to a value"),
        }
    }

    pub fn read_place(&mut self, place: Place, span: Span) {
        match place {
            Place::Local(id) => inst!(with self.frame => span; LOAD_LOCAL id),
            Place::Field(ty) => inst!(with self.frame => span; READ if ty.is_primitive() { ty.size() } else { 0 }),
            Place::Index => inst!(with self.frame => span; INDEX),
        }
    }

    pub fn write_place(&mut self, place: Place, span: Span) {
        match place {
            Place::Local(id) => inst!(with self.frame => span; STORE_LOCAL id),
            Place::Field(ty) => inst!(with self.frame => span; WRITE ty.size()),
            Place::Index => inst!(with self.frame => span; WRITE_INDEX),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Place<'t> {
    Local(usize),
    Field(spark::ty::TypeId<'t>),
    Index,
}

impl Place<'_> {
    pub fn operands(&self) -> usize {
        match self {
            Self::Local(_) => 0,
            Self::Field(_) | Self::Index => 2,
        }
    }
}

#[derive(Debug, Clone)]
pub struct GeneratedFunction {
    pub line_table: Vec<LineMap>,
    pub name: Box<str>,
    pub kind: FunctionKind,
}

impl GeneratedFunction {
    pub fn new(name: Box<str>, kind: FunctionKind) -> Self {
        Self { line_table: Vec::new(), name, kind }
    }

    pub fn line_for(&self, inst_ptr: usize) -> StacktraceLocation {
        if matches!(self.kind, FunctionKind::Native(_)) {
            StacktraceLocation::Native
        } else {
            // TODO: make sure line table has at least one entry
            if self.line_table.is_empty() { return StacktraceLocation::Line(0); };
            match self.line_table.binary_search_by_key(&inst_ptr, |map| map.ip) {
                Ok(i) => StacktraceLocation::Line(self.line_table[i].line),
                Err(i) => StacktraceLocation::Line(self.line_table[i.saturating_sub(1)].line),
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionKind {
    Instructions(InstructionSet, usize),
    Native(NativeFn),
}

#[derive(Debug, Clone)]
pub struct Globals {
    pub funs: Vec<GeneratedFunction>,
    pub types: Vec<TypeInfo>,
    pub vars: GlobalVariables,
}


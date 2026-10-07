use diagnostic::Span;
use instruction::{Instruction, InstructionSet};
use spark::{Spark, SparkFunction, expr::{Expr, ExprKind, Literal, LogicalOperation}, scope::GlobalVariables, stmt::{IfBranch, IfStmt, Stmt}, ty::{BuiltinType, TypeId}};

use crate::{Slot, VM, error::{RuntimeError, StacktraceLocation}, heap::{HeapArray, HeapObject, TypeInfo}, inst};

pub type NativeFn = fn(&mut VM) -> Result<Slot, RuntimeError>;

pub mod instruction;
pub mod native;

pub fn ignite(spark: Spark, globals: &mut Globals) -> [usize; BuiltinType::SIZE] {
    let type_offset = globals.types.len();

    for fun in spark.globals.funs {
        globals.funs.push(FlameGen::compile_fun(fun, type_offset));
    }

    for lantern_struct in spark.globals.types {
        globals.types.push(lantern_struct.get().expect("all type data is initialized").clone().into());
    }

    globals.vars = spark.globals.vars;

    spark.tcx.into_builtins(type_offset)
}

#[derive(Debug)]
struct FlameGen {
    instructions: InstructionSet,
    line_table: Vec<LineMap>,
    loops: Vec<LoopScope>,
    type_offset: usize,
}

impl FlameGen {
    fn compile_fun(fun: SparkFunction, type_offset: usize) -> GeneratedFunction {
        match fun {
            SparkFunction::Lantern { name, stmts, locals } => {
                let mut r#gen = FlameGen {
                    instructions: InstructionSet::new(),
                    line_table: Vec::new(),
                    loops: Vec::new(),
                    type_offset,
                };
                r#gen.compile_stmts(&stmts);
                GeneratedFunction {
                    line_table: r#gen.line_table,
                    name: name.into_boxed_str(),
                    kind: FunctionKind::Instructions(r#gen.instructions, locals),
                }
            }
            SparkFunction::Native { name, native } => GeneratedFunction {
                line_table: Vec::new(),
                name: name.into_boxed_str(),
                kind: FunctionKind::Native(native::get_native_spark_fn(native)),
            },
        }
    }

    fn ip(&self) -> usize {
        self.instructions.len()
    }

    #[must_use]
    fn placeholder(&mut self) -> usize {
        let index = self.ip();
        self.instructions.push(Instruction::Error);
        index
    }

    fn patch(&mut self, index: usize, inst: Instruction) {
        self.instructions[index] = inst;
    }

    fn compile_stmts(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            match stmt {
                Stmt::If(if_stmt) => {
                    let mut end_indices = Vec::new();
                    self.compile_if_stmt(if_stmt, &mut end_indices);
                    for index in end_indices {
                        self.patch(index, inst!(GOTO self.ip()));
                    }
                }
                Stmt::Match { expr, some_local, some_arm, none_arm } => {
                    self.compile_expr(expr);
                    inst!(self.instructions; IS_NULL);
                    let none_arm_goto = self.placeholder();

                    inst!(self.instructions; STORE_LOCAL *some_local);
                    inst!(self.instructions; POP);
                    self.compile_stmts(some_arm);
                    let exit = self.placeholder();

                    self.patch(none_arm_goto, inst!(POP_GOTO_IF_TRUE self.ip()));
                    inst!(self.instructions; POP);
                    self.compile_stmts(none_arm);

                    self.patch(exit, inst!(GOTO self.ip()));
                }
                Stmt::While { cond, stmts } => {
                    let head = self.ip();
                    self.loops.push(LoopScope::new(head));

                    self.compile_expr(cond);
                    let exit = self.placeholder();

                    self.compile_stmts(stmts);
                    inst!(self.instructions; GOTO head);

                    let end = self.ip();
                    self.patch(exit, inst!(POP_GOTO_IF_FALSE end));
                    for break_index in self.loops.pop().expect("in loop").breaks {
                        self.patch(break_index, inst!(GOTO end));
                    }
                }
                // TODO: span data
                Stmt::Val(id, init) => {
                    if let Some(init) = init {
                        self.compile_expr(init);
                    } else {
                        inst!(self.instructions; PUSHNULL);
                    }
                    inst! { self.instructions;
                        [STORE_LOCAL *id]
                        [POP]
                    };
                }
                Stmt::Return(expr) => {
                    if let Some(expr) = expr {
                        self.compile_expr(expr);
                    } else {
                        inst!(self.instructions; PUSHNULL);
                    }
                    inst!(self.instructions; RET);
                }
                // TODO: span data
                Stmt::Continue => {
                    let head = self.loops.last().expect("`continue` in a loop").head;
                    inst!(self.instructions; GOTO head);
                }
                // TODO: span data
                Stmt::Break => {
                    let index = self.placeholder();
                    self.loops.last_mut().expect("`break` in a loop").breaks.push(index);
                }
                Stmt::Throw(expr) => {
                    self.compile_expr(expr);
                    inst!(self.instructions; THRW);
                }
                Stmt::Expr(expr) => {
                    self.compile_expr(expr);
                    inst!(self.instructions; POP);
                }
            }
        }
    }

    fn compile_if_stmt(&mut self, if_stmt: &IfStmt, end_indices: &mut Vec<usize>) {
        self.compile_expr(&if_stmt.cond);
        let next_branch = self.placeholder();

        self.compile_stmts(&if_stmt.stmts);
        end_indices.push(self.placeholder());

        self.patch(next_branch, inst!(POP_GOTO_IF_FALSE self.ip()));

        match &if_stmt.branch {
            Some(IfBranch::ElseIf(if_stmt)) => self.compile_if_stmt(if_stmt, end_indices),
            Some(IfBranch::Else(stmts)) => self.compile_stmts(stmts),
            None => {}
        }
    }

    fn compile_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Literal(Literal::None) => inst!(with self => expr.span; PUSHNULL),
            ExprKind::Literal(Literal::Int(int)) => inst!(with self => expr.span; PUSHI *int),
            ExprKind::Literal(Literal::Float(float)) => inst!(with self => expr.span; PUSHF *float),
            ExprKind::Literal(Literal::True) => inst!(with self => expr.span; PUSHU crate::bool_to_slot(true)),
            ExprKind::Literal(Literal::False) => inst!(with self => expr.span; PUSHU crate::bool_to_slot(false)),
            ExprKind::Static(id) => inst!(with self => expr.span; PUSHU *id),
            ExprKind::Global(id) => inst!(with self => expr.span; LOAD_GLOBAL *id),
            ExprKind::Local(id) => inst!(with self => expr.span; LOAD_LOCAL *id),
            ExprKind::Block(stmts) => self.compile_stmts(stmts),
            ExprKind::Field(obj, offset) => {
                self.compile_expr(obj);
                inst! { with self => expr.span;
                    [PUSHU HeapObject::field_offset() + *offset]
                    [READ read_len(expr.ty)]
                }
            }
            ExprKind::Len(array) => {
                self.compile_expr(array);
                inst! { with self => expr.span;
                    [PUSHU HeapArray::len_offset()]
                    [READ size_of::<i64>()]
                }
            }
            ExprKind::Index(array, index) => {
                self.compile_expr(array);
                self.compile_expr(index);
                inst!(with self => expr.span; INDEX)
            }
            ExprKind::Binary(lhs, op, rhs) => {
                self.compile_expr(lhs);
                self.compile_expr(rhs);
                inst!(with self => expr.span);
                self.instructions.push((*op).into());
            }
            ExprKind::BinaryAssign(lhs, op, rhs) => {
                let place = self.compile_place(lhs);
                for _ in 0..place.operands() {
                    inst!(self.instructions; DUP place.operands() - 1);
                }
                self.read_place(place, lhs.span);
                self.compile_expr(rhs);
                self.instructions.push((*op).into());
                self.write_place(place, expr.span);
            }
            ExprKind::Unary(op, value) => {
                self.compile_expr(value);
                inst!(with self => expr.span);
                self.instructions.push((*op).into());
            }
            ExprKind::Logical(lhs, logical, rhs) => {
                self.compile_expr(lhs);
                let short_circuit = self.placeholder();

                inst!(self.instructions; POP);
                self.compile_expr(rhs);

                let end = self.ip();
                match logical {
                    LogicalOperation::And => self.patch(short_circuit, inst!(GOTO_IF_FALSE end)),
                    LogicalOperation::Or => self.patch(short_circuit, inst!(GOTO_IF_TRUE end)),
                }
            }
            ExprKind::Assign(place, value) => {
                let place = self.compile_place(place);
                self.compile_expr(value);
                self.write_place(place, expr.span);
            }
            ExprKind::Call(fun, args) => {
                self.compile_expr(fun);
                for arg in args {
                    self.compile_expr(arg);
                }
                inst!(with self => expr.span; INV args.len());
            }
            ExprKind::CallMethod(recv, id, args) => {
                self.compile_expr(recv);
                inst!(with self => expr.span; PUSHU *id);
                for arg in args {
                    self.compile_expr(arg);
                }
                inst!(with self => expr.span; INV_MET args.len());
            }
            ExprKind::Struct(id, fields) => {
                // TODO: better span information here
                inst!(self.instructions; ALLOC_OBJ (*id + self.type_offset));
                for (field, offset) in fields {
                    inst!(with self => field.span; PUSHU (HeapObject::field_offset() + *offset));
                    self.compile_expr(field);
                    inst!(with self => field.span; WRITE field.ty.size());
                }
            }
            ExprKind::Array(ty, elements) => {
                for element in elements {
                    self.compile_expr(element);
                }
                let id = if ty.is_ref() { VM::REF_ARR_TYPE_INDEX } else { VM::PRIMITIVE_ARR_TYPE_INDEX };
                inst!(with self => expr.span; ALLOC_ARR id, elements.len());
            }
            ExprKind::Error => panic!("error expression encountered"),
        }
    }

    #[must_use]
    fn compile_place<'p>(&mut self, place: &Expr<'p>) -> Place<'p> {
        match &place.kind {
            ExprKind::Local(id) => Place::Local(*id),
            ExprKind::Field(base, offset) => {
                self.compile_expr(base);
                inst!(with self => base.span; PUSHU (HeapObject::field_offset() + *offset));
                Place::Field(place.ty)
            }
            ExprKind::Index(array, index) => {
                self.compile_expr(array);
                self.compile_expr(index);
                Place::Index
            }
            _ => panic!("attempting to write to a value"),
        }
    }

    fn read_place(&mut self, place: Place, span: Span) {
        match place {
            Place::Local(id) => inst!(with self => span; LOAD_LOCAL id),
            Place::Field(ty) => inst!(with self => span; READ read_len(ty)),
            Place::Index => inst!(with self => span; INDEX),
        }
    }

    fn write_place(&mut self, place: Place, span: Span) {
        match place {
            Place::Local(id) => inst!(with self => span; STORE_LOCAL id),
            Place::Field(ty) => inst!(with self => span; WRITE ty.size()),
            Place::Index => inst!(with self => span; WRITE_INDEX),
        }
    }
}

fn read_len(ty: TypeId) -> usize {
    if ty.is_primitive() { ty.size() } else { 0 }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Place<'t> {
    Local(usize),
    Field(TypeId<'t>),
    Index,
}

impl Place<'_> {
    fn operands(&self) -> usize {
        match self {
            Self::Local(_) => 0,
            Self::Field(_) | Self::Index => 2,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct LoopScope {
    head: usize,
    breaks: Vec<usize>,
}

impl LoopScope {
    fn new(head: usize) -> Self {
        Self { head, breaks: Vec::new() }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineMap {
    pub ip: usize,
    pub line: u32,
}

impl LineMap {
    pub fn new(ip: usize, line: u32) -> Self {
        Self { ip, line }
    }
}

#[derive(Debug, Clone)]
pub struct GeneratedFunction {
    pub line_table: Vec<LineMap>,
    pub name: Box<str>,
    pub kind: FunctionKind,
}

impl GeneratedFunction {
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


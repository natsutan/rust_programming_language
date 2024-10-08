use std::fmt::Display;
// https://github.com/msakuta/ruscal/tree/master/examples
use std::error::Error;
use std::io::{BufReader, BufWriter, Read, Write};
use nom::{branch::alt, bytes::complete::tag, character::complete::{
    alpha1, alphanumeric1, char, multispace0, multispace1,
}, combinator::{map_res, opt, recognize}, multi::{fold_many0, many0}, number::complete::recognize_float, sequence::{delimited, pair, terminated}, Finish, Parser, IResult,
error::ParseError};
use nom::sequence::preceded;
use rustack::{parse_args, RunMode};
use rustack::Args;

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    LoadLiteral,
    Store,
    Copy,
    Dup,
    Add,
    Sub,
    Mul,
    Div,
    Call,
    Jmp,
    Jf,
    Lt,
    Pop
}

macro_rules! imple_op_from {
    ($($op:ident), *) => {
        impl From<u8> for OpCode {
            #[allow(non_upper_case_globals)]
            fn from(o: u8) -> Self {
                $(const $op: u8 = OpCode::$op as u8;)*

                match o {
                    $($op => Self::$op,)*
                    _ => panic!("Opecode \"{:02X}\" unrecgnized!", o),

                }
            }
        }
    };

}
imple_op_from!(
	LoadLiteral, 
	Store, 
	Copy, 
	Add, 
	Sub, 
	Mul, 
	Div, 
	Call, 
	Jmp, 
	Jf, 
	Lt, 
	Pop);
#[derive(Debug, Clone, Copy)]
#[repr(C)]
struct Instruction {
    op: OpCode,
    arg0: u8,
}
impl Instruction {
    fn new(op: OpCode, arg0: u8) -> Self{
        Self {op, arg0}
    }

    fn serialize (&self, writer: &mut impl Write)-> Result<(), std::io::Error> {
        writer.write_all(&[self.op as u8, self.arg0])?;
        Ok(())
    }

    fn deserialize(reader: &mut impl Read) -> Result<Self, std::io::Error> {
        let mut buf = [0u8; 2];
        reader.read_exact(&mut buf)?;
        Ok(Self::new(buf[0].into(), buf[1]))
    }

}

fn serialize_size(sz: usize, writer: &mut impl Write) -> std::io::Result<()> {
    writer.write_all(&(sz as u32).to_le_bytes())
}

fn deserialize_size(reader: &mut impl Read) -> std::io::Result<usize> {
    let mut buf = [0u8; std::mem::size_of::<u32>()];
    reader.read_exact(&mut buf)?;
    Ok(u32::from_le_bytes(buf) as usize)

}


fn serialize_str(s: &str, writer: &mut impl Write) -> std::io::Result<()> {
    serialize_size(s.len(), writer)?;
    writer.write_all(s.as_bytes())?;

    Ok(())
}

fn deserialize_str(reader: &mut impl Read) -> std::io::Result<String> {
    let mut buf = vec![0u8; deserialize_size(reader)?];
    reader.read_exact(&mut buf)?;
    let s = String::from_utf8(buf).unwrap();

    Ok(s)
}



#[repr(u8)]
enum ValueKind {
    F64,
    Str,
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    F64(f64),
    Str(String),
}

impl Default for Value {
    fn default() -> Self {
         Self::F64(0.)
    }
}

impl Display for Value {
    fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::F64(value) => write!(f, "{value}"),
            Self::Str(value) => write!(f, "{value:?}"),
        }
    }
}
impl Value {
    fn kind(&self) -> ValueKind {
        match self {
            Self::F64(_) => ValueKind::F64,
            Self::Str(_) => ValueKind::Str,
        }
    }

    fn serialize(&self, writer: &mut impl Write) -> std::io::Result<()>{
        let kind = self.kind() as u8;
        writer.write_all(&[kind])?;
        match self  {
            Self::F64(value) => {
                writer.write_all(&value.to_le_bytes())?;
            }
            Self::Str(value) => {
                serialize_str(value, writer)?;
            }

        }

        Ok(())
    }

    #[allow(non_upper_case_globals)]
    fn deserialize(reader: &mut impl Read) -> std::io::Result<Self> {
        const F64: u8 = ValueKind::F64 as u8;
        const Str: u8 = ValueKind::Str as u8;

        let mut kind_buf = [0u8; 1];
        reader.read_exact(&mut kind_buf)?;
        match kind_buf[0] {
            F64 => {
                let mut buf = [0u8; std::mem::size_of::<f64>()];
                reader.read_exact(&mut buf)?;
                Ok(Value::F64(f64::from_le_bytes(buf)))
            }
            Str => Ok(Value::Str(deserialize_str(reader)?)),
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Value Kind {} does not match to any known value", kind_buf[0]),
            )),
        }
    }

    fn coerce_f64(&self) -> f64 {
        match self {
            Self::F64(value) => *value,
            _ =>  panic!("Coerecion failed {:?} can not be coerced to f64", self)
        }
    }

}


struct ByteCode {
    literals: Vec<Value>,
    instructions: Vec<Instruction>,
}

impl ByteCode {
    fn new() -> Self {
        Self {
            literals: vec![],
            instructions: vec![]
        }
    }

    fn read_literals(&mut self, reader :&mut impl Read) -> std::io::Result<()> {
        let num_literarls = deserialize_size(reader)?;
        for _ in 0..num_literarls {

            self.literals.push(Value::deserialize(reader)?);
        }
        Ok(())
    }
    fn read_instructions(&mut self, reader: &mut impl Read) -> std::io::Result<()> {
        let num_instructions = deserialize_size(reader)?;
        for _ in 0..num_instructions {
            let inst = Instruction::deserialize(reader)?;
            self.instructions.push(inst);
        }
        Ok(())
    }


    fn interpret(&self) -> Option<Value> {
        let mut stack   = vec![];
        let mut ip = 0;

        while ip < self.instructions.len() {
            let instruction = &self.instructions[ip];
            println!("interpret[{ip}] {instruction:?} stack {stack:?}");
            match instruction.op {
                OpCode::LoadLiteral => {
                    stack.push(self.literals[instruction.arg0 as usize].clone());
                } ,
                OpCode::Store => {
                    let idx = stack.len() - instruction.arg0 as usize - 1;
                    stack[idx] = stack.pop().expect("Store needs an argument");
                }
                OpCode::Copy => {
                    stack.push(stack[stack.len() - instruction.arg0 as usize - 1].clone());
                }

                OpCode::Dup => {
                    let top = stack.last().unwrap().clone();
                    stack.extend((0..instruction.arg0).map(|_| top.clone()));
                }


                OpCode::Add => self.interpret_bin_op(&mut stack, |lhs, rhs| lhs + rhs),

                OpCode::Sub => {
                    self.interpret_bin_op(&mut stack, |lhs, rhs| lhs - rhs)
                },
                OpCode::Mul => {
                    self.interpret_bin_op(&mut stack, |lhs, rhs| lhs * rhs)
                },
                OpCode::Div => {
                    self.interpret_bin_op(&mut stack, |lhs, rhs| lhs / rhs)
                },


                OpCode::Call => {
                    let args = &stack[stack.len() - instruction.arg0 as usize..];
                    let fname = &stack[stack.len() - instruction.arg0 as usize - 1];
                    let Value::Str(fname) = fname else {
                        panic!("Function name shall be a string: {fname:?}");
                    };
                    let res = match fname as &str {
                        "sqrt" => unary_fn(f64::sqrt)(args),
                        "sin" => unary_fn(f64::sin)(args),
                        "cos" => unary_fn(f64::cos)(args),
                        "tan" => unary_fn(f64::tan)(args),
                        "asin" => unary_fn(f64::asin)(args),
                        "acos" => unary_fn(f64::acos)(args),
                        "atan" => unary_fn(f64::atan)(args),
                        "atan2" => binary_fn(f64::atan2)(args),
                        "pow" => binary_fn(f64::powf)(args),
                        "exp" => unary_fn(f64::exp)(args),
                        "log" => binary_fn(f64::log)(args),
                        "log10" => unary_fn(f64::log10)(args),
                        "print" => print_fn(args),
                        _ => panic!("Unknown function name {fname:?}"),
                    };
                    stack.resize(stack.len() - instruction.arg0 as usize - 1, Value::F64(0.));
                    stack.push(res);
                },
                OpCode::Jmp => {
                    ip = instruction.arg0 as usize;
                    continue;
                },
                OpCode::Jf => {
                    let cond = stack.pop().expect("Jf needs an argument");
                    if cond.coerce_f64() == 0. {
                        ip = instruction.arg0 as usize;
                        continue;
                    }
                },
                OpCode::Lt => {
                    self.interpret_bin_op(&mut stack, |lhs, rhs| {
                        (lhs < rhs) as i32 as f64
                    })

                },
                OpCode::Pop => {
                    stack.resize(
                        stack.len() - instruction.arg0 as usize, Value::default(),
                        );
                }
            }
            ip += 1;
        }
        stack.pop()
    }

    fn interpret_bin_op(&self, stack: &mut Vec<Value>, op: impl FnOnce(f64, f64)-> f64) {
        let rhs = stack.pop().expect("Stack underflow").coerce_f64();
        let lhs = stack.pop().expect("Stack underflow").coerce_f64();
        stack.push(Value::F64(op(lhs, rhs)));
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct StkIdx(usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct InstPtr(usize);


#[derive(Debug, Clone, Default)]
enum Target {
    #[default]
    Temp,
    Literal(usize),
    Local(String)
}




struct LoopFrame {
    start: StkIdx,
    break_ips: Vec<InstPtr>,
    continue_ips: Vec<(InstPtr, usize)>,
}

impl LoopFrame {
    fn new(start: StkIdx) -> Self {
        Self {
            start,
            break_ips: vec![],
            continue_ips: vec![],
        }
    }
}

#[derive(Debug)]
struct LoopStackUnderflowError;

impl Display for LoopStackUnderflowError {
    fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "A break statement outside loop")
    }
}

impl Error for LoopStackUnderflowError {}


struct Compiler {
    literals: Vec<Value>,
    instructions: Vec<Instruction>,
    target_stack: Vec<Target>,
    loop_stack: Vec<LoopFrame>
}


#[derive(Debug, PartialEq, Clone)]
enum Statement<'src> {
    Expression(Expression<'src>),
    VarDef(&'src str, Expression<'src>),
    VarAssign(&'src str, Expression<'src>),
    For {
        loop_var: &'src str,
        start: Expression<'src>,
        end: Expression<'src>,
        stmts: Statements<'src>,
    },
    Break,
    Continue,
}

type Statements<'a> = Vec<Statement<'a>>;

impl Compiler {
    fn new() -> Self {
        Self {
            literals: vec![],
            instructions: vec![],
            target_stack: vec![],
            loop_stack: vec![],
        }
    }

    fn add_literal(&mut self, value: Value) -> u8 {
        let existing = self
            .literals
            .iter()
            .enumerate()
            .find(|(_, val)| **val == value);
        if let Some((i, _)) = existing {
            i as u8
        } else {
            let ret = self.literals.len();
            self.literals.push(value);
            ret as u8
        }
    }

    fn add_inst (&mut self, op: OpCode, arg0: u8) -> InstPtr {
        let inst = self.instructions.len();
        self.instructions.push(Instruction {op, arg0});
        InstPtr(inst)
    }

    fn add_copy_inst(&mut self, stack_idx: StkIdx) -> InstPtr {
     let inst = self.add_inst(
      OpCode::Copy,
      (self.target_stack.len() - stack_idx.0 - 1) as u8,
    );
        self.target_stack.push(Target::Temp);
        inst
    }

    fn add_load_literal_inst(&mut self, lit: u8) -> InstPtr {
        let inst = self.add_inst(OpCode::LoadLiteral, lit);
        self.target_stack.push(Target::Literal(lit as usize));
        inst
    }

    fn add_store_inst(&mut self, stack_idx: StkIdx) -> InstPtr {
        let inst = self.add_inst(
            OpCode::Store,
            (self.target_stack.len() - stack_idx.0 - 1) as u8,
        );
        self.target_stack.pop();
        inst
    }

    fn add_jf_inst(&mut self) -> InstPtr {
        let inst = self.add_inst(OpCode::Jf, 0);
        self.target_stack.pop();
        inst
    }

    fn fixup_jmp(&mut self, ip: InstPtr) {
        self.instructions[ip.0].arg0 = 
        	self.instructions.len() as u8;
    }

    fn add_pop_until_inst(&mut self, stack_idx: StkIdx) -> Option<InstPtr> {
        if self.target_stack.len() <= stack_idx.0 {
            return None;
        }
        let inst = self.add_inst(
            OpCode::Pop, 
            (self.target_stack.len() - stack_idx.0 - 1) as u8,
        );
        self.target_stack.resize(stack_idx.0 + 1, Target::Temp);

        Some(inst)
    }


    fn write_literals (&self, writer: &mut impl Write) -> std::io::Result<()> {
        serialize_size(self.literals.len(), writer)?;
        for value in &self.literals {
           value.serialize(writer)?;
        }
        Ok(())
    }

    fn write_insts(&self, writer: &mut impl Write) -> std::io::Result<()> {
        serialize_size(self.instructions.len(), writer)?;
        for instruction in &self.instructions {
            instruction.serialize(writer).unwrap();
        }
        Ok(())
    }

    fn compile_expr(&mut self, ex: &Expression) -> Result<StkIdx, Box<dyn Error>> {
        Ok(match ex {
            Expression::NumLiteral(num) => {
                let id = self.add_literal(Value::F64(*num));

                self.add_load_literal_inst(id);
                self.stack_top()
            }

            Expression::Ident(ident) => {
                let var = self.target_stack.iter().enumerate().find(
                    |(_i, tgt)| {
                        if let Target::Local(id) = tgt {
                            id == ident
                        } else {
                            false
                        }
                    },
                );
                if let Some(var) = var {
                    return Ok(StkIdx(var.0));
                } else {
                    println!("stack: {:?}", self.target_stack);
                    return Err(
                        format!("Variable not found: {ident:?}").into(),
                    )
                }
            }
            Expression::Add(lhs, rhs) => {
                self.bin_op(OpCode::Add, lhs, rhs)?
            }
            Expression::Sub(lhs, rhs) => {
                self.bin_op(OpCode::Sub, lhs, rhs)?
            }
            Expression::Mul(lhs, rhs) => {
                self.bin_op(OpCode::Mul, lhs, rhs)?
            }
            Expression::Div(lhs, rhs) => {
                self.bin_op(OpCode::Div, lhs, rhs)?
            }
            Expression::Gt(lhs, rhs) => {
                self.bin_op(OpCode::Lt, rhs, lhs)?
            }
            Expression::Lt(lhs, rhs) => {
                self.bin_op(OpCode::Lt, lhs, rhs)?
            }
            Expression::FnInvoke(name, args) => {
                let stack_before_args = self.target_stack.len();
                let name = self.add_literal(Value::Str(name.to_string()));
                let args = args.iter().map(|arg| self.compile_expr(arg)).collect::<Result<Vec<_>, _>>()?;
                let stack_before_call = self.target_stack.len();
                self.add_load_literal_inst(name);
                for arg in &args {
                    self.add_copy_inst(*arg);
                }
                self.add_inst(OpCode::Call, args.len() as u8);
                self.target_stack.resize(stack_before_call + 1, Target::Temp);
                self.coerce_stack(StkIdx(stack_before_args));

                self.stack_top()
            }
            Expression::If(cond, true_branch, false_branch) => {
                use OpCode::*;
                let cond = self.compile_expr(cond)?;
                self.add_copy_inst(cond);
                let jf_inst = self.add_jf_inst();
                let stack_size_before = self.target_stack.len();
                self.compile_stmts_or_zero(true_branch)?;
                self.coerce_stack(StkIdx(stack_size_before + 1));
                let jmp_inst = self.add_inst(Jmp, 0);
                self.fixup_jmp(jf_inst);
                self
                    .target_stack
                    .resize(stack_size_before, Target::Temp);
                if let Some(false_branch) = false_branch.as_ref() {
                    self.compile_stmts_or_zero(&false_branch)?;
                }
                self.coerce_stack(StkIdx(stack_size_before + 1));
                self.fixup_jmp(jmp_inst);
                self.stack_top()
            }
        })
    }

    fn bin_op(&mut self, op:OpCode, lhs: &Expression, rhs: &Expression) -> Result<StkIdx, Box<dyn Error>> {
        let lhs = self.compile_expr(lhs)?;
        let rhs = self.compile_expr(rhs)?;
        self.add_copy_inst(lhs);
        self.add_copy_inst(rhs);
        self.add_inst(op, 0);

        self.target_stack.pop();
        self.target_stack.pop();
        self.target_stack.push(Target::Temp);

        Ok(self.stack_top())
    }

   fn coerce_stack(&mut self, target: StkIdx) {
        if target.0 < self.target_stack.len() - 1 {
            self.add_store_inst(target);
            self.add_pop_until_inst(target);
        } else if self.target_stack.len() - 1 < target.0 {
            for _ in self.target_stack.len() -1 .. target.0 {
                self.add_copy_inst(self.stack_top());
            }
        }
    }

    fn compile_stmts(
        &mut self, 
        stmts: &Statements,
    ) -> Result<Option<StkIdx>, Box<dyn Error>> {
        let mut last_result = None;
        for stmt in stmts {
            match stmt {
                Statement::Expression(ex) => {
                    last_result = Some(self.compile_expr(ex)?);
                }
                Statement::VarDef(vname, ex) => {
                    let mut ex = self.compile_expr(ex)?;
                    if matches!(self.target_stack[ex.0], Target::Local(_)) {
                        self.add_copy_inst(ex);
                        ex = self.stack_top();
                    }
                    self.target_stack[ex.0] = Target::Local(vname.to_string());
                }
                Statement::VarAssign(vname, ex) => {
                    let stk_ex = self.compile_expr(ex)?;
                    let (stk_local, _) = self.target_stack.iter_mut().enumerate()
                        .find(|(_, tgt) | {
                            if let Target::Local(tgt ) = tgt {
                                tgt == vname
                            } else {
                                false
                            }
                        })
                        .ok_or_else(|| {
                            format!("Variable name not found: {vname}")
                        })?;
                    self.add_copy_inst(stk_ex);
                    self.add_store_inst(StkIdx(stk_local));
                }
                Statement::For {loop_var, start, end, stmts} => {
                    let stk_start = self.compile_expr(start)?;
                    let stk_end = self.compile_expr(end)?;
                    //dprintln!("start: {stk_start:?} end: {stk_end:?}");
                    self.add_copy_inst(stk_start);
                    let stk_loop_var = self.stack_top();
                    self.target_stack[stk_loop_var.0] =
                        Target::Local(loop_var.to_string());
                    //dprintln!("after start: {:?}", self.target_stack);
                    let inst_check_exit = self.instructions.len();
                    self.add_copy_inst(stk_loop_var);
                    self.add_copy_inst(stk_end);
                    //dprintln!("before cmp: {:?}", self.target_stack);
                    self.add_binop_inst(OpCode::Lt);
                    let jf_inst = self.add_jf_inst();
                    //dprintln!("start in loop: {:?}", self.target_stack);
                    self.loop_stack.push(LoopFrame::new(stk_loop_var));
                    self.compile_stmts(stmts)?;
                    self.fixup_continues()?;
                    let one = self.add_literal(Value::F64(1.));
                    //dprintln!("end in loop: {:?}", self.target_stack);
                    self.add_copy_inst(stk_loop_var);
                    self.add_load_literal_inst(one);
                    self.add_inst(OpCode::Add, 0);
                    self.target_stack.pop();
                    self.add_store_inst(stk_loop_var);
                    self.add_pop_until_inst(stk_loop_var);
                    self.add_inst(OpCode::Jmp, inst_check_exit as u8);
                    self.fixup_jmp(jf_inst);
                    self.fixup_breakes()?;
                }
                Statement::Break => {
                    let start = self
                    .loop_stack.last().map(|loop_frame| loop_frame.start).ok_or(LoopStackUnderflowError)?;
                    self.add_pop_until_inst(start);

                    let loop_frame = self.loop_stack.last_mut().ok_or(LoopStackUnderflowError)?;
                    let break_ip = self.instructions.len();
                    loop_frame.break_ips.push(InstPtr(break_ip));
                    self.add_inst(OpCode::Jmp, 0);
                }
                Statement::Continue => {
                    let start = self.loop_stack.last().map(|frame| frame.start).ok_or(LoopStackUnderflowError)?;
                    self.add_pop_until_inst(start);

                    let loop_frame = self.loop_stack.last_mut().ok_or(LoopStackUnderflowError)?;
                    let continue_ip = self.instructions.len();
                    loop_frame.continue_ips.push((
                        InstPtr(continue_ip),
                        self.target_stack.len(),
                        ));
                    self.add_inst(OpCode::Dup,0);
                    self.add_inst(OpCode::Jmp, 0);
                }
            }
        }
        Ok(last_result)
    }

    fn compile_stmts_or_zero(
        &mut self,
        stmts: &Statements,
    ) -> Result<StkIdx, Box<dyn Error>> {
        Ok(self.compile_stmts(stmts)?.unwrap_or_else(|| {
            let id = self.add_literal(Value::F64(0.));
            self.add_load_literal_inst(id);
            self.stack_top()
        }))
    }

    fn add_binop_inst(&mut self, op: OpCode) -> InstPtr {
        self.target_stack.pop();
        self.add_inst(op, 0)
    }

    fn fixup_breakes (&mut self) -> Result<(), Box<dyn Error>> {
        let loop_frame = self.loop_stack.pop().ok_or(LoopStackUnderflowError)?;
        let break_jmp_addr = self.instructions.len();
        for ip in loop_frame.break_ips {
            self.instructions[ip.0].arg0 = break_jmp_addr as u8;
        }
        Ok(())
    }

    fn fixup_continues(&mut self) -> Result<(), Box<dyn Error>> {
        let loop_frame = self.loop_stack.last().ok_or(LoopStackUnderflowError)?;
        let continue_jmp_addr = self.instructions.len();
        for (ip, stk ) in &loop_frame.continue_ips {
            self.instructions[ip.0].arg0 = (self.target_stack.len() - stk) as u8;
            self.instructions[ip.0 + 1].arg0 = continue_jmp_addr as u8;
        }
        Ok(())
    }


    fn disasm(&self, writer: &mut impl Write) -> std::io::Result<()> {
        use OpCode::*;
        writeln!(writer, "Literals [{}]", self.literals.len())?;
        for (i, con) in self.literals.iter().enumerate() {
            writeln!(writer, "[{i}] {}", *con)?;
        }

        for (i, inst) in self.instructions.iter().enumerate() {
            match inst.op {
                LoadLiteral => writeln!(
                    writer,
                    "  [{i}] {:?} {} ({:?})",
                    inst.op, inst.arg0, self.literals[inst.arg0 as usize]
                )?,
                Copy | Call | Jmp | Jf | Pop | Store  => writeln!(
                    writer,
                    "  [{i}] {:?}. {}", inst.op, inst.arg0
                )?,
                _ => writeln!(writer, "  [{i}] {:?}", inst.op)?,
            }
        }

        Ok(())
    }

    fn stack_top(&self) -> StkIdx {
        StkIdx(self.target_stack.len() - 1)
    }

 

}

#[derive(Debug, PartialEq, Clone)]
enum Expression<'src> {
    Ident(&'src str),
    NumLiteral(f64),
    FnInvoke(&'src str, Vec<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Gt(Box<Expression<'src>>, Box<Expression<'src>>),
    Lt(Box<Expression<'src>>, Box<Expression<'src>>),
    If(Box<Expression<'src>>, Box<Statements<'src>>, Option<Box<Statements<'src>>>),

}

fn write_program(source: &str, writer: &mut impl Write, out_file: &str, disasm: bool) -> Result<(), Box<dyn std::error::Error>> {
    let mut compiler = Compiler::new();
    let stmts = statements_finish(source).map_err(|e| {
        std::io::Error::new(std::io::ErrorKind::Other, e.to_string())
    })?;

    compiler.compile_stmts_or_zero(&stmts)?;

    if disasm {
        compiler.disasm(&mut std::io::stdout())?;
    }

    compiler.write_literals(writer).unwrap();
    compiler.write_insts(writer).unwrap();
    println!(
        "Written {} literal and {} instruction to {out_file:?}",
        compiler.literals.len(),
        compiler.instructions.len()
    );


    Ok(())
}

fn print_fn(args: &[Value]) -> Value {
    for arg in args {
        print!("{:?} ", arg);
    }
    println!("");
    Value::F64(0.)
}


fn read_program(reader: &mut impl Read) -> std::io::Result<ByteCode> {
    let mut bytecode = ByteCode::new();
    bytecode.read_literals(reader)?;
    bytecode.read_instructions(reader)?;
    Ok(bytecode)


}

fn compile(
    writer: &mut impl Write,
    args: &Args,
    out_file: &str,
) -> Result<(), Box<dyn Error>> {
    let src = args.source.as_ref().ok_or_else(|| {
        Box::new(std::io::Error::new(
            std::io::ErrorKind::Other,
            "Please specify source file to compile after -c"
                .to_string(),
        ))
    })?;
    let source = std::fs::read_to_string(src)?;
    write_program(&source, writer, out_file, args.disasm)
}

fn main() ->  Result<(), Box<dyn std::error::Error>>{

    let Some(args) = parse_args(true) else { return Ok(())};
    match args.run_mode {
        RunMode::Compile => {

            let writer = std::fs::File::create(&args.output)?;
            let mut writer = BufWriter::new(writer);
            compile(&mut writer, &args, &args.output)?;

        }
        RunMode::Run(code_file) => {
            let reader = std::fs::File::open(&code_file)?;
            let mut reader = BufReader::new(reader);
            let byte_code = read_program(&mut reader)?;
            byte_code.interpret();
        }
        RunMode::CompileAndRun => {
            let mut buf = vec![];
            compile(&mut std::io::Cursor::new(&mut buf), &args, "<Memory>")?;
            let bytecode = read_program(&mut std::io::Cursor::new(&mut buf))?;
            bytecode.interpret();

        }
        _ => println!("please specify -c or -r as an argument"),
    }

    Ok(())
}

fn term(i: &str) -> IResult<&str, Expression> {
    let (i, init) = factor(i)?;

    fold_many0(
        pair(space_delimited(alt((char('*'), char('/')))), factor),
        move || init.clone(),
        | acc, (op, val): (char, Expression) | {
            match op {
                '*' => Expression::Mul(Box::new(acc), Box::new(val)),
                '/' => Expression::Div(Box::new(acc), Box::new(val)),
                _ => panic!("Multiplicative expression should have '*' or '/' operator"),
            }
        }
    )(i)

}

fn var_def(i: &str) -> IResult<&str, Statement> {
    let (i, _) =
        delimited(multispace0, tag("var"), multispace1)(i)?;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(char('='))(i)?;
    let (i, expr) = space_delimited(expr)(i)?;
    Ok((i, Statement::VarDef(name, expr)))
}

fn var_assign(i: &str) -> IResult<&str, Statement> {
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(char('='))(i)?;
    let (i, expr) = space_delimited(expr)(i)?;
    Ok((i, Statement::VarAssign(name, expr)))
}

fn for_statement(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("for"))(i)?;
    let (i, loop_var) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(tag("in"))(i)?;
    let (i, start) = space_delimited(expr)(i)?;
    let (i, _) = space_delimited(tag("to"))(i)?;
    let (i, end) = space_delimited(expr)(i)?;
    let (i, stmts) =
        delimited(open_brace, statements, close_brace)(i)?;
    Ok((
        i,
        Statement::For {
            loop_var,
            start,
            end,
            stmts,
        },
    ))
}

fn break_stmt(input: &str) -> IResult<&str, Statement> {
    let (r, _) = space_delimited(tag("break"))(input)?;
    Ok((r, Statement::Break))
}

fn continue_statement(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("continue"))(i)?;
    Ok((i, Statement::Continue))
}

fn general_statement<'a>(
    last: bool,
) -> impl Fn(&'a str) -> IResult<&'a str, Statement> {
    let terminator = move |i| -> IResult<&str, ()> {
        let mut semicolon = pair(tag(";"), multispace0);
        if last {
            Ok((opt(semicolon)(i)?.0, ()))
        } else {
            Ok((semicolon(i)?.0, ()))
        }
    };
    move |input: &str| {
        alt((
            terminated(var_def, terminator),
            terminated(var_assign, terminator),
            for_statement,
            terminated(break_stmt, terminator),
            terminated(continue_statement, terminator),
            terminated(expr_statement, terminator),
        ))(input)
    }
}
fn expr_statement(i: &str) -> IResult<&str, Statement> {
    let (i, res) = expr(i)?;
    Ok((i, Statement::Expression(res)))
}

pub(crate) fn last_statement(
    input: &str,
) -> IResult<&str, Statement> {
    general_statement(true)(input)
}

pub(crate) fn statement(input: &str) -> IResult<&str, Statement> {
    general_statement(false)(input)
}

fn statements(i: &str) ->  IResult<&str, Statements> {
    let (r, mut v) = many0(statement)(i)?;
    let (r, last) = opt(last_statement)(r)?;
    let (r, _)  = opt(multispace0)(r)?;
    if let Some(last) = last {
        v.push(last);
    }
    Ok((r, v))
}

fn statements_finish(i: &str) -> Result<Statements, nom::error::Error<&str>>{
    let (_, res) = statements(i).finish()?;
    Ok(res)

}

fn factor(i: &str) -> IResult<&str, Expression> {
    alt((number, func_call, ident, parens))(i)
}

fn func_call(i: &str) -> IResult<&str, Expression> {
    let (r, ident) = space_delimited(identifier)(i)?;
    let (r, args) = space_delimited(delimited(
        tag("("),
        many0(delimited(
            multispace0,
            expr,
            space_delimited(opt(tag(","))),
        )),
        tag(")"),
    ))(r)?;
    Ok((r, Expression::FnInvoke(ident, args)))
}



fn ident(input: &str) -> IResult<&str, Expression> {
    let (r, res) = delimited(multispace0, identifier, multispace0)(input)?;
    Ok((r, Expression::Ident(res)))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_"))))
    ))(input)
}

fn number(input: &str) -> IResult<&str, Expression> {
    let (r,v) = delimited(multispace0, recognize_float, multispace0)(input)?;
    Ok((
        r,
        Expression::NumLiteral(v.parse().map_err(|_| {
            nom::Err::Error(nom::error::Error {
                input,
                code: nom::error::ErrorKind::Digit,
            })
        })?),
    ))
}

fn parens(i: &str) -> IResult<&str, Expression> {
    delimited(
        multispace0,
        delimited(tag("("), expr, tag(")")),
        multispace0
    )(i)
}

fn expr(i: &str) -> IResult<&str, Expression> {
    alt((if_expr, cond_expr, num_expr))(i)
}

fn unary_fn(f: fn(f64)-> f64) -> impl  Fn(&[Value]) -> Value {
    move |args| {
        let arg = args.first().expect("function missing argument");
        let ret = f(arg.coerce_f64());
        Value::F64(ret)
    }
}

fn binary_fn(f: fn(f64, f64) -> f64, ) -> impl Fn(&[Value]) -> Value {
    move |args| {
        let mut args = args.into_iter();
        let lhs = args.next().expect("function missing the first argument").coerce_f64();
        let rhs = args.next().expect("function missing the second argument").coerce_f64();
        Value::F64(f(lhs, rhs))
    }
}

fn num_expr(i: &str) -> IResult<&str, Expression> {
    let (i, init) = term(i)?;

    fold_many0(
        pair(space_delimited(alt((char('+'), char('-')))), term),
        move || init.clone(),
        |acc, (op, val) : (char, Expression) | match op {
            '+' => Expression::Add(Box::new(acc), Box::new(val)),
            '-' => Expression::Sub(Box::new(acc), Box::new(val)),
            _ => {
                panic!("Additive expression should have + or - operator")
            }
        }

    )(i)

}

fn cond_expr(i: &str) -> IResult<&str, Expression> {
    let (i, first) = num_expr(i)?;
    let (i, cond) = space_delimited(alt((char('<'), char('>'))))(i)?;
    let (i, second) = num_expr(i)?;
    Ok((
        i,
        match cond {
            '<' => Expression::Lt(Box::new(first), Box::new(second)),
            '>' => Expression::Gt(Box::new(first), Box::new(second)),
            _ => unreachable!(),
         }
        ))
}

fn if_expr(i: &str) -> IResult<&str, Expression> {
    let (i, _) = space_delimited(tag("if"))(i)?;
    let (i, cond) = expr(i)?;
    let (i, t_case) =
        delimited(open_brace, statements, close_brace)(i)?;
    let (i, f_case) = opt(preceded(
        space_delimited(tag("else")),
        alt((
            delimited(open_brace, statements, close_brace),
            map_res(
                if_expr,
                |v| -> Result<Vec<Statement>, nom::error::Error<&str>> {
                    Ok(vec![Statement::Expression(v)])
                },
            ),
        )),
    ))(i)?;

    Ok((
        i,
        Expression::If(
            Box::new(cond),
            Box::new(t_case),
            f_case.map(Box::new),
        ),
    ))
}

fn open_brace(i: &str) -> IResult<&str, ()> {
    let (i, _) = space_delimited(char('{'))(i)?;
    Ok((i, ()))
}

fn close_brace(i: &str) -> IResult<&str, ()> {
    let (i, _) = space_delimited(char('}'))(i)?;
    Ok((i, ()))
}

fn space_delimited<'src, O, E>(
    f: impl Parser<&'src str, O, E>,
) -> impl FnMut(&'src str) -> IResult<&'src str, O, E>
where
    E: ParseError<&'src str>,
{
    delimited(multispace0, f, multispace0)
}

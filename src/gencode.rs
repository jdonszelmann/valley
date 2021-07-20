use crate::ast::{
    Assignment, Atom, Block, BlockExpr, Expression, FuncCall, FuncDef, LetAssignment, Line, Name,
    Primary, Program,
};
use crate::gencode::Instruction::Label;
use crate::stack_layout::StackLayout;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum GenCodeError {}

#[derive(Clone, PartialEq, Debug)]
pub enum Instruction<'s> {
    Label(Name<'s>),
    StackPointerToX,
    XToStackPointer,

    XToA,
    AToX,

    PushA,
    PopA,

    IncrementX,
    DecrementX,

    AddImmediateWithCarry(u16),
    SubImmediateWithCarry(u16),

    ReturnFromSubroutine,
}

impl<'s> Instruction<'s> {
    fn to_str(&self) -> String {
        match self {
            Self::Label(x) => format!("{}:", x.value),
            Self::StackPointerToX => "STX",
            Self::XToStackPointer => "SXT",
            Self::XToA => "TXA",
            Self::AToX => "TAX",
            Self::PushA => "PHA",
            Self::PopA => "PLA",
            Self::IncrementX => "INX",
            Self::DecrementX => "DEX",
            Self::AddImmediateWithCarry(x) => format!("ADC #{}", x),
            Self::SubImmediateWithCarry(x) => format!("SBC #{}", x),
            Self::ReturnFromSubroutine => "RTS",
        }
    }
}

pub struct GeneratedProgram<'s> {
    pub functions: Vec<FunctionBlock<'s>>,
}

impl<'s> GeneratedProgram<'s> {
    fn new() -> Self {
        Self { functions: vec![] }
    }
}

pub struct FunctionBlock<'s> {
    name: Name<'s>,
    instructions: Vec<Instruction<'s>>,
    stack_layout: StackLayout<'s>,
}

impl<'s> FunctionBlock<'s> {
    pub fn new(name: Name<'s>) -> Self {
        let mut stack_layout = StackLayout::new();

        Self {
            name: name.clone(),
            instructions: vec![Label(name)],
            stack_layout,
        }
    }

    pub fn instructions<'a>(&'a self) -> impl Iterator<Item = &'_ Instruction> + 'a {
        self.instructions.iter()
    }

    pub fn add_instruction(&mut self, instr: Instruction<'s>) -> Result<(), GenCodeError> {
        self.instructions.push(instr);
        Ok(())
    }
}

fn gencode(p: Program) -> Result<GeneratedProgram, GenCodeError> {
    let mut g = GeneratedProgram::new();
    gencode_program(p, &mut g)?;

    Ok(g)
}

pub fn gencode_funcdef<'s>(
    p: FuncDef<'s>,
    g: &'_ mut FunctionBlock<'s>,
) -> Result<(), GenCodeError> {
    let d = p.get_declared_bytes(&mut g.stack_layout);

    // Create stack frame
    if d == 0 {
    } else if d == 1 {
        g.add_instruction(Instruction::PushA)?;
    } else if d <= 3 {
        // 4 + 2 * n
        g.add_instruction(Instruction::StackPointerToX)?; // 2
        for _ in 0..d {
            g.add_instruction(Instruction::DecrementX)?; // 2 * d
        }
        g.add_instruction(Instruction::XToStackPointer)?; // 2
    } else {
        // 10
        g.add_instruction(Instruction::StackPointerToX)?; // 2
        g.add_instruction(Instruction::XToA)?; // 2
        g.add_instruction(Instruction::SubImmediateWithCarry(d as u16))?; // 2
        g.add_instruction(Instruction::AToX)?; // 2
        g.add_instruction(Instruction::XToStackPointer)?; // 2
    }

    // Gen code
    gencode_block(p.block, g)?;

    // Clean up stack frame
    if d == 0 {
    } else if d == 1 {
        g.add_instruction(Instruction::PopA)?;
    } else if d <= 3 {
        // 4 + 2 * n
        g.add_instruction(Instruction::StackPointerToX)?; // 2
        for _ in 0..d {
            g.add_instruction(Instruction::IncrementX)?; // 2 * d
        }
        g.add_instruction(Instruction::XToStackPointer)?; // 2
    } else {
        // 10
        g.add_instruction(Instruction::StackPointerToX)?; // 2
        g.add_instruction(Instruction::XToA)?; // 2
        g.add_instruction(Instruction::AddImmediateWithCarry(d as u16))?; // 2
        g.add_instruction(Instruction::AToX)?; // 2
        g.add_instruction(Instruction::XToStackPointer)?; // 2
    }

    g.add_instruction(Instruction::ReturnFromSubroutine)?;

    Ok(())
}

pub fn gencode_program<'s>(
    p: Program<'s>,
    g: &'_ mut GeneratedProgram<'s>,
) -> Result<(), GenCodeError> {
    for f in p.functions {
        let mut fnb = FunctionBlock::new(f.name.clone());
        gencode_funcdef(f, &mut fnb)?;
        g.functions.push(fnb)
    }

    Ok(())
}

fn gencode_line<'s>(p: Line<'s>, g: &'_ mut FunctionBlock<'s>) -> Result<(), GenCodeError> {
    match p {
        Line::Assignment(a) => gencode_assignment(a, g),
        Line::Expression(e) => gencode_expr(e, g),
        Line::LetAssignment(LetAssignment { to, expr, .. }) => {
            gencode_assignment(Assignment { to, expr }, g)
        }
    }
}

fn gencode_assignment<'s>(
    p: Assignment<'s>,
    g: &'_ mut FunctionBlock<'s>,
) -> Result<(), GenCodeError> {
    let Assignment { to, expr } = p;
    gencode_expr(expr, g)?;
    // g.add_instruction(Instruction::Store(to))?;

    Ok(())
}

fn gencode_expr<'s>(p: Expression<'s>, g: &'_ mut FunctionBlock<'s>) -> Result<(), GenCodeError> {
    match p {
        Expression::Add(a, b) => {
            gencode_expr(*a, g)?;
            gencode_expr(*b, g)?;
            // g.add_instruction(Instruction::Add)?;
        }
        Expression::Subtract(a, b) => {
            gencode_expr(*b, g)?;
            gencode_expr(*a, g)?;
            // g.add_instruction(Instruction::Subtract)?;
        }
        Expression::Mult(a, b) => {
            gencode_expr(*a, g)?;
            gencode_expr(*b, g)?;
            // g.add_instruction(Instruction::Mult)?;
        }
        Expression::Divide(a, b) => {
            gencode_expr(*a, g)?;
            gencode_expr(*b, g)?;
            // g.add_instruction(Instruction::Divide)?;
        }
        Expression::Modulo(a, b) => {
            gencode_expr(*a, g)?;
            gencode_expr(*b, g)?;
            // g.add_instruction(Instruction::Modulo)?;
        }
        Expression::Exponent(a, b) => {
            gencode_expr(*a, g)?;
            gencode_expr(*b, g)?;
            // g.add_instruction(Instruction::Exponent)?;
        }
        Expression::Decrement(a) => {
            gencode_expr(*a, g)?;
            // g.add_instruction(Instruction::Decrement)?;
        }
        Expression::Increment(a) => {
            gencode_expr(*a, g)?;
            // g.add_instruction(Instruction::Increment)?;
        }
        Expression::FuncCall(f) => {
            gencode_funccall(f, g)?;
        }
    }

    Ok(())
}

fn gencode_funccall<'s>(p: FuncCall<'s>, g: &'_ mut FunctionBlock<'s>) -> Result<(), GenCodeError> {
    let FuncCall { callee, params: _ } = p;

    gencode_primary(callee, g)?;

    Ok(())
}

fn gencode_primary<'s>(p: Primary<'s>, g: &'_ mut FunctionBlock<'s>) -> Result<(), GenCodeError> {
    match p {
        Primary::Atom(a) => gencode_atom(a, g),
        Primary::Expression(a) => gencode_expr(*a, g),
    }
}

fn gencode_atom<'s>(p: Atom<'s>, g: &'_ mut FunctionBlock<'s>) -> Result<(), GenCodeError> {
    match p {
        Atom::String(_) => todo!(),
        Atom::Integer(i) => {
            // g.add_instruction(Instruction::ConstInt(i)),
            Ok(())
        }
        Atom::Name(n) => {
            // g.add_instruction(Instruction::Load(n))
            Ok(())
        }
        Atom::Block(b) => gencode_blockexpr(b, g),
    }
}

pub fn gencode_block<'s>(p: Block<'s>, g: &'_ mut FunctionBlock<'s>) -> Result<(), GenCodeError> {
    for i in p.lines {
        gencode_line(i, g)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        Assignment, Atom, Block, Expression, FuncCall, FuncDef, Line, Name, Primary, Program,
    };
    use crate::gencode::Instruction::*;
    use crate::gencode::{gencode_program, FunctionBlock, GeneratedProgram};

    macro_rules! parse_test {
        ($name: ident : $input: literal => $($instr: expr),* $(,)?) => {
            #[test]
            fn $name() {
                let mut g = GeneratedProgram::new();
                let p = Program::from_string($input).unwrap();

                gencode_program(p, &mut g).unwrap();

                assert_eq!(
                    g.instructions().cloned().collect::<Vec<_>>(),
                    vec![
                        $(
                            $instr
                        ),*
                    ]
                );
            }
        };
    }

    macro_rules! stack_size_test {
        ($name: ident : $input: literal, $stack_size: literal) => {
            #[test]
            fn $name() {
                let mut g = GeneratedProgram::new();
                let p = Program::from_string($input).unwrap();

                gencode_program(p, &mut g).unwrap();

                assert_eq!(g.functions[0].stack_layout.size(), $stack_size,);
            }
        };
    }

    stack_size_test!(simple: "fn main() {}", 0);
    stack_size_test!(byte: "fn main() {let a: u8 = 3;}", 1);
    stack_size_test!(double_byte: "fn main() {let a: u16 = 3;}", 2);
    stack_size_test!(two_double_byte: "fn main() {let a: u16 = 3; let b: u16 = 4}", 4);
    stack_size_test!(pointer: "fn main() {let a: *u8 = 3;}", 2);
    stack_size_test!(param: "fn main(a: u8) {}", 0);
}

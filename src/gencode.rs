use crate::ast::{Program, Name, Line, Assignment, Expression, FuncCall, Primary, Atom, BlockExpr, Dir, Block};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum GenCodeError {

}

#[derive(Clone, PartialEq, Debug)]
pub enum Instruction<'s> {
    Pop,
    Add,
    Subtract,
    Divide,
    Mult,
    Exponent,
    Modulo,
    IntegerDivide,
    Negate,

    Store(Name<'s>),
    Load(Name<'s>),
    LoadOrFunctionParam(Name<'s>),

    ConstInt(i64),
    ConstFloat(f64),

    Call{num_params_on_stack: usize},

    DirBlock{ code: CodeObject<'s>}
}

pub struct CodeGenerator<'s> {
    instructions: Vec<Instruction<'s>>
}

impl<'s> CodeGenerator<'s> {
    pub fn new() -> Self {
        Self {
            instructions: vec![]
        }
    }

    pub fn instructions<'a>(&'a self) -> impl Iterator<Item = &'_ Instruction> + 'a {
        self.instructions.iter()
    }

    pub fn add_instruction(&mut self, instr: Instruction<'s>) -> Result<(), GenCodeError> {
        self.instructions.push(instr);
        Ok(())
    }

    pub fn code_object(self) -> CodeObject<'s> {
        CodeObject {
            instructions: self.instructions
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CodeObject<'s> {
    pub instructions: Vec<Instruction<'s>>
}

impl<'s> AsRef<CodeObject<'s>> for CodeObject<'s> {
    fn as_ref(&self) -> &CodeObject<'s> {
        self
    }
}


fn gencode(p: Program) -> Result<CodeObject, GenCodeError> {
    let mut g = CodeGenerator::new();
    gencode_program(p, &mut g)?;

    Ok(g.code_object())
}

pub fn gencode_program<'s>(p: Program<'s>, g: &'_ mut CodeGenerator<'s>) -> Result<(), GenCodeError> {
    for i in p.lines {
        gencode_line(i, g)?;
    }

    Ok(())
}

fn gencode_line<'s>(p: Line<'s>, g: &'_ mut CodeGenerator<'s>) -> Result<(), GenCodeError> {
    match p {
        Line::Assignment(a) => gencode_assignment(a, g),
        Line::Expression(e) => {
            gencode_expr(e, g)?;
            // when a line contains just an expression, its side effects are
            // applied, but the result is discarded (and thus needs to be popped)
            g.add_instruction(Instruction::Pop)
        },
    }
}

fn gencode_assignment<'s>(p: Assignment<'s>, g: &'_ mut CodeGenerator<'s>) -> Result<(), GenCodeError> {
    let Assignment{to, expr, } = p;
    gencode_expr(expr, g)?;
    g.add_instruction(Instruction::Store(to))?;

    Ok(())
}

fn gencode_expr<'s>(p: Expression<'s>, g: &'_ mut CodeGenerator<'s>) -> Result<(), GenCodeError> {
    match p {
        Expression::Add(a, b) => {
            gencode_expr(*a, g)?;
            gencode_expr(*b, g)?;
            g.add_instruction(Instruction::Add)?;
        }
        Expression::Subtract(a, b) => {
            gencode_expr(*b, g)?;
            gencode_expr(*a, g)?;
            g.add_instruction(Instruction::Subtract)?;
        }
        Expression::Mult(a, b) => {
            gencode_expr(*a, g)?;
            gencode_expr(*b, g)?;
            g.add_instruction(Instruction::Mult)?;
        }
        Expression::Divide(a, b) => {
            gencode_expr(*a, g)?;
            gencode_expr(*b, g)?;
            g.add_instruction(Instruction::Divide)?;
        }
        Expression::IntegerDivide(a, b) => {
            gencode_expr(*a, g)?;
            gencode_expr(*b, g)?;
            g.add_instruction(Instruction::IntegerDivide)?;
        }
        Expression::Modulo(a, b) => {
            gencode_expr(*a, g)?;
            gencode_expr(*b, g)?;
            g.add_instruction(Instruction::Modulo)?;
        }
        Expression::Exponent(a, b) => {
            gencode_expr(*a, g)?;
            gencode_expr(*b, g)?;
            g.add_instruction(Instruction::Exponent)?;
        }
        Expression::Negate(a) => {
            gencode_expr(*a, g)?;
            g.add_instruction(Instruction::Negate)?;
        }
        Expression::FuncCall(f) => {
            gencode_funccall(f, g)?;
        }
    }

    Ok(())
}

fn gencode_funccall<'s>(p: FuncCall<'s>, g: &'_ mut CodeGenerator<'s>) -> Result<(), GenCodeError> {
    let FuncCall{ callee, params } = p;

    let num_params = params.len();
    for i in params.into_iter().rev() {
        gencode_primary(i, g, true)?;
    }

    gencode_primary(callee, g, false)?;

    if num_params > 0 {
        g.add_instruction(Instruction::Call {
            num_params_on_stack: num_params
        })?;
    }

    Ok(())
}

fn gencode_primary<'s>(p: Primary<'s>, g: &'_ mut CodeGenerator<'s>, in_function: bool) -> Result<(), GenCodeError> {
    match p {
        Primary::Atom(a) => gencode_atom(a, g, in_function),
        Primary::Expression(a) => gencode_expr(*a, g),
    }
}

fn gencode_atom<'s>(p: Atom<'s>, g: &'_ mut CodeGenerator<'s>, in_function: bool) -> Result<(), GenCodeError> {
    match p {
        Atom::String(_) => todo!(),
        Atom::Integer(i) => g.add_instruction(Instruction::ConstInt(i)),
        Atom::Name(n) => {
            if in_function {
                g.add_instruction(Instruction::LoadOrFunctionParam(n))
            } else {
                g.add_instruction(Instruction::Load(n))
            }
        },
        Atom::Block(b) => {
            gencode_blockexpr(b, g)
        }
    }
}

fn gencode_blockexpr<'s>(p: BlockExpr<'s>, g: &'_ mut CodeGenerator<'s>) -> Result<(), GenCodeError> {
    match p {
        BlockExpr::Dir(d) => {
            gencode_dir(d, g)
        }
    }
}

fn gencode_dir<'s>(p: Dir<'s>, g: &'_ mut CodeGenerator<'s>) -> Result<(), GenCodeError> {
    let Dir{ dir, block } = p;

    gencode_expr(*dir, g)?;

    let mut sub_generator = CodeGenerator::new();
    gencode_block(block, &mut sub_generator)?;
    let co = sub_generator.code_object();

    g.add_instruction(Instruction::DirBlock {
        code: co
    })
}

pub fn gencode_block<'s>(p: Block<'s>, g: &'_ mut CodeGenerator<'s>) -> Result<(), GenCodeError> {
    for i in p.lines {
        gencode_line(i, g)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::gencode::{gencode_program, CodeGenerator};
    use crate::ast::{Program, Line, Name, Assignment, Expression, FuncCall, Primary, Atom};
    use crate::gencode::Instruction::*;

    macro_rules! parse_test {
        ($name: ident : $input: literal => $($instr: expr),* $(,)?) => {
            #[test]
            fn $name() {
                let mut g = CodeGenerator::new();
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

    #[test]
    fn test_gen_code() {
        let mut g = CodeGenerator::new();
        let p = Program{ lines: vec![Line::Assignment(
            Assignment {
                to: Name::new("a"),
                expr: Expression::Add(
                    Box::new(Expression::FuncCall(FuncCall{callee: Primary::Atom(Atom::Integer(3)), params: vec![]})),
                    Box::new(Expression::FuncCall(FuncCall{callee: Primary::Atom(Atom::Integer(4)), params: vec![]})),
                ),
            }
        )] };

        gencode_program(p, &mut g).unwrap();

        assert_eq!(
            g.instructions().cloned().collect::<Vec<_>>(),
            vec![
                ConstInt(3),
                ConstInt(4),
                Add,
                Store(Name::new("a"))
            ]
        );
    }

    parse_test!(simple : "a = 3 + 4" =>
        ConstInt(3),
        ConstInt(4),
        Add,
        Store(Name::new("a")),
    );
    parse_test!(compound : "a = 3 + 4; b = a + 3" =>
        ConstInt(3),
        ConstInt(4),
        Add,
        Store(Name::new("a")),
        Load(Name::new("a")),
        ConstInt(3),
        Add,
        Store(Name::new("b")),
    );
    parse_test!(precedence : "a = 3 + 4 * 5" =>
        ConstInt(3),
        ConstInt(4),
        ConstInt(5),
        Mult,
        Add,
        Store(Name::new("a")),
    );
    parse_test!(precedence_2 : "a = 3 * 4 + 5" =>
        ConstInt(3),
        ConstInt(4),
        Mult,
        ConstInt(5),
        Add,
        Store(Name::new("a")),
    );
    parse_test!(parens : "a = (3 + 4) * 5" =>
        ConstInt(3),
        ConstInt(4),
        Add,
        ConstInt(5),
        Mult,
        Store(Name::new("a")),
    );
    parse_test!(pop_sideeffect_expr : "(3 + 4) * 5" =>
        ConstInt(3),
        ConstInt(4),
        Add,
        ConstInt(5),
        Mult,
        Pop
    );
}
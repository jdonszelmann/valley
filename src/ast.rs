
use pest::Parser;
use pest_derive::Parser;
use thiserror::Error;
use std::num::{ParseIntError, ParseFloatError};
use std::borrow::Cow;
use std::fmt;
use std::fmt::Debug;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct GrammarParser;

type Pair<'s> =  pest::iterators::Pair<'s, Rule>;

#[derive(Clone, PartialEq, Debug, Eq, Hash)]
pub struct Name<'s> {
    pub value: Cow<'s, str>
}

impl<'s> fmt::Display for Name<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.value, f)
    }
}

impl<'s> Name<'s> {
    pub const fn new(value: &'s str) -> Self {
        Self {
            value: Cow::Borrowed(value)
        }
    }
}

#[derive(Debug)]
pub struct Block<'s> {
    pub lines: Vec<Line<'s>>
}

impl<'s> Block<'s> {
    pub fn parse(block: Pair<'s>) -> Result<Self, ParseProgramError> {
        let mut lines = Vec::new();
        let mut inner = block.into_inner();

        for line in inner {
            match line.as_rule() {
                Rule::line => lines.push(Line::parse(line)?),
                _ => unreachable!()
            }
        }

        Ok(Block {
            lines,
        })
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    U8,
    U16,
    Pointer(Box<Type>)
}

impl Type {
    pub fn size(&self) -> u64 {
        match self {
            Type::U8 => 1,
            Type::U16 => 2,
            Type::Pointer(_) => 2,
        }
    }

    pub fn parse(tp: &str) -> Result<Self, ParseProgramError> {
        Ok(match tp {
            "u8" => Type::U8,
            "u16" => Type::U16,
            i if i.starts_with("*") => Type::Pointer(Box::new(Type::parse(&i[1..])?)),
            _ => unreachable!()
        })
    }
}

#[derive(Debug)]
pub struct Param<'s> {
    pub param_type: Type,
    pub name: Name<'s>,
}

#[derive(Debug)]
pub struct FuncDef<'s> {
    pub(crate) block: Block<'s>,
    pub(crate) name: Name<'s>,
    pub(crate) params: Vec<Param<'s>>
}


impl<'s> FuncDef<'s> {
    fn parse_param(params: Pair<'s>) -> Result<Param<'s>, ParseProgramError> {

        let mut inner = params.into_inner();

        let name = Name::new(inner.next().unwrap().as_str());
        let param_type = Type::parse(inner.next().unwrap().as_str())?;

        Ok(Param {
            param_type,
            name,
        })
    }

    pub fn parse(funcdef: Pair<'s>) -> Result<Self, ParseProgramError> {
        let mut inner = funcdef.into_inner();

        let name = Name::new(inner.next().unwrap().as_str());

        let mut maybe_params = inner.next().unwrap();
        let mut params = Vec::new();
        while maybe_params.as_rule() == Rule::param {
            params.push(Self::parse_param(maybe_params)?);
            maybe_params = inner.next().unwrap();
        }

        let block = Block::parse(maybe_params)?;

        Ok(Self {
            block,
            params,
            name,
        })
    }
}

#[derive(Debug)]
pub enum BlockExpr<'s> {
    FuncDef(FuncDef<'s>)
}

impl<'s> BlockExpr<'s> {
    pub fn parse(block: Pair<'s>) -> Result<Self, ParseProgramError> {
        let mut inner = block.into_inner();
        let next = inner.next().unwrap();

        Ok(match next.as_rule() {
            Rule::funcdef => {
                BlockExpr::FuncDef(FuncDef::parse(next)?)
            }
            _ => unreachable!()
        })
    }
}

#[derive(Debug)]
pub enum Atom<'s> {
    String(Cow<'s, str>),
    Integer(i64),
    Name(Name<'s>),
    Block(BlockExpr<'s>),
}

impl<'s> Atom<'s> {
    pub fn parse(expr: Pair<'s>) -> Result<Self, ParseProgramError> {
        let mut inner = expr.into_inner();

        let next = inner.next().unwrap();

        Ok(match next.as_rule() {
            Rule::integer => {
                let int_str = next.as_str();
                let int = int_str
                    .parse()
                    .map_err(|e| ParseProgramError::ParseInt(next.as_str(), e))?;

                Self::Integer(int)
            },
            Rule::name => Self::Name(
                Name::new(next.as_str())
            ),
            Rule::block_expr => {
                Self::Block(BlockExpr::parse(next)?)
            },
            Rule::string => {
                let string = next.as_str();
                Self::String(Cow::Borrowed(&string[1..string.len() - 1]))
            }
            _ => unreachable!()
        })
    }
}

#[derive(Debug)]
pub enum Primary<'s> {
    Atom(Atom<'s>),
    Expression(Box<Expression<'s>>)
}

impl<'s> Primary<'s> {
    pub fn parse(expr: Pair<'s>) -> Result<Self, ParseProgramError> {
        let mut inner = expr.into_inner();

        let possible_atom = inner.next().unwrap();
        Ok(match possible_atom.as_rule() {
            Rule::expr => {
                Self::Expression(
                    Box::new(Expression::parse_expr(
                        possible_atom
                    )?)
                )
            }
            _ => {
                Self::Atom(
                    Atom::parse(possible_atom)?
                )
            }
        })
    }
}

#[derive(Debug)]
pub struct FuncCall<'s> {
    pub callee: Primary<'s>,
    pub params: Vec<Expression<'s>>,
}

impl<'s> FuncCall<'s> {
    pub fn parse(expr: Pair<'s>) -> Result<Self, ParseProgramError> {
        let mut inner = expr.into_inner();

        let callee = Primary::parse(inner.next().unwrap())?;
        let mut params = Vec::new();
        for param in inner {
            params.push(Expression::parse_expr(param)?);
        }

        Ok(Self {
            callee,
            params
        })
    }
}

#[derive(Debug)]
pub enum Expression<'s> {
    Add(Box<Expression<'s>>, Box<Expression<'s>>),
    Subtract(Box<Expression<'s>>, Box<Expression<'s>>),
    Mult(Box<Expression<'s>>, Box<Expression<'s>>),
    Divide(Box<Expression<'s>>, Box<Expression<'s>>),
    Modulo(Box<Expression<'s>>, Box<Expression<'s>>),
    Exponent(Box<Expression<'s>>, Box<Expression<'s>>),
    Increment(Box<Expression<'s>>),
    Decrement(Box<Expression<'s>>),
    FuncCall(FuncCall<'s>)
}

impl<'s> Expression<'s> {
    fn parse_sum(sum: Pair<'s>) -> Result<Expression, ParseProgramError> {
        let mut inner = sum.into_inner();
        let mut left = Self::parse_term(
            inner.next().unwrap()
        )?;

        while let Some(i) = inner.next() {
            left = match i.as_str() {
                "+" => Expression::Add(
                    Box::new(left),
                    Box::new(Self::parse_term(inner.next().unwrap())?)
                ),
                "-" => Expression::Subtract(
                    Box::new(left),
                    Box::new(Self::parse_term(inner.next().unwrap())?)
                ),
                _ => unreachable!()
            }
        }

        Ok(left)
    }

    fn parse_term(term: Pair<'s>) -> Result<Expression, ParseProgramError> {
        let mut inner = term.into_inner();
        let mut left = Self::parse_factor(
            inner.next().unwrap()
        )?;

        while let Some(i) = inner.next() {
            left = match i.as_str() {
                "*" => Expression::Mult(
                    Box::new(left),
                    Box::new(Self::parse_factor(inner.next().unwrap())?)
                ),
                "/" => Expression::Divide(
                    Box::new(left),
                    Box::new(Self::parse_factor(inner.next().unwrap())?)
                ),
                "%" => Expression::Modulo(
                    Box::new(left),
                    Box::new(Self::parse_factor(inner.next().unwrap())?)
                ),
                _ => unreachable!()
            };
        }

        Ok(left)
    }

    fn parse_factor(factor: Pair<'s>) -> Result<Expression, ParseProgramError> {
        let mut inner = factor.into_inner();

        let next = inner.next().unwrap();


        Ok(match next.as_str() {
            "++" => Expression::Increment(Box::new(Self::parse_factor(
                inner.next().unwrap()
            )?)),
            "--" => {
                Expression::Decrement(Box::new(Self::parse_factor(
                    inner.next().unwrap()
                )?))
            },
            _ => Self::parse_power(
                next
            )?,
        })
    }

    fn parse_power(factor: Pair<'s>) -> Result<Expression, ParseProgramError> {
        let mut inner = factor.into_inner();

        let left = Expression::FuncCall(FuncCall::parse(
            inner.next().unwrap()
        )?);


        if let Some(_ /* this is the ** operator */) = inner.next() {
            let right = Self::parse_factor(
                inner.next().unwrap()
            )?;

            Ok(Expression::Exponent(Box::new(left), Box::new(right)))
        } else {
            Ok(left)
        }

    }

    fn parse_expr(expr: Pair<'s>) -> Result<Self, ParseProgramError> {
        Self::parse_sum(expr.into_inner().next().unwrap())
    }
}

#[derive(Debug)]
pub struct Assignment<'s> {
    pub to: Name<'s>,
    pub expr: Expression<'s>,
}

impl<'s> Assignment<'s> {
    pub fn parse(line: Pair<'s>) -> Result<Self, ParseProgramError> {
        let mut inner = line.into_inner();
        let name = inner.next().unwrap();
        let expression = inner.next().unwrap();

        let name_string = name.as_str();
        let expr = Expression::parse_expr(expression)?;

        Ok(Self {
            to: Name::new(name_string),
            expr,
        })
    }
}

#[derive(Debug)]
pub struct LetAssignment<'s> {
    pub tp: Type,
    pub to: Name<'s>,
    pub expr: Expression<'s>,
}

impl<'s> LetAssignment<'s> {
    pub fn parse(line: Pair<'s>) -> Result<Self, ParseProgramError> {
        let mut inner = line.into_inner();
        let name = inner.next().unwrap();
        let tp = inner.next().unwrap();
        let expression = inner.next().unwrap();

        let name_string = name.as_str();
        let tp = Type::parse(tp.as_str())?;
        let expr = Expression::parse_expr(expression)?;

        Ok(Self {
            tp,
            to: Name::new(name_string),
            expr,
        })
    }
}

#[derive(Debug)]
pub enum Line<'s> {
    Assignment(Assignment<'s>),
    LetAssignment(LetAssignment<'s>),
    Expression(Expression<'s>),
}

impl<'s> Line<'s> {
    pub fn parse(line: Pair<'s>) -> Result<Self, ParseProgramError> {
        let inner = line.into_inner().next().unwrap();
        Ok(match inner.as_rule() {
            Rule::assignment => {
                Line::Assignment(Assignment::parse(inner)?)
            }
            Rule::let_assignment => {
                Line::LetAssignment(LetAssignment::parse(inner)?)
            }
            Rule::expr => {
                Line::Expression(Expression::parse_expr(inner)?)
            }
            _ => unreachable!()
        })
    }
}

#[derive(Debug)]
pub struct Program<'s> {
    pub functions: Vec<FuncDef<'s>>
}

#[derive(Error, Debug)]
pub enum ParseProgramError<'s> {
    #[error("Parsing error: {0}")]
    Pest(#[from] pest::error::Error<Rule>),

    #[error("Failed to parse {0} as int ({1})")]
    ParseInt(&'s str, ParseIntError),
    #[error("Failed to parse {0} as float ({1})")]
    ParseFloat(&'s str, ParseFloatError),
}


impl<'s> Program<'s> {
    pub fn from_string(input: &'s str) -> Result<Self, ParseProgramError> {
        let program = GrammarParser::parse(Rule::program, input)?;
        let mut lines = Vec::new();


        for line in program {
            match line.as_rule() {
                Rule::funcdef => lines.push(FuncDef::parse(line)?),
                Rule::EOI => {},
                _ => unreachable!()
            }
        }

        Ok(Self {
            functions: lines,
        })
    }
}


#[cfg(test)]
mod tests {
    use crate::ast::Program;

    macro_rules! test_parse {
        ($name: ident $($inp: literal)*) => {
            #[test]
            fn $name() {
                $(
                    println!("{}", $inp);
                    match Program::from_string($inp) {
                        Ok(_) => {},
                        Err(e) => {
                            panic!("{}", e)
                        }
                    }
                )*
            }
        };
    }

    test_parse!(assignment_and_lines
        "fn main(){a = 3;}"
        "fn main(){a = 3;}"
        "fn main(){a = 3; b = 4}"
        "fn main(){a = 3; b = 4;}"
        "fn main(){a = 3;


        b = 3;}
        "
        "fn main() {a = 3;


        b = 3;}"
        "fn main() {a = 3;


        b = 3;}"
        "fn main() {a = 3;


        b = 3;}"
    );

    test_parse!(arithmetic
       "fn main() {3 + 4;}"
       "fn main() {3 - 4;}"
       "fn main() {3 * 4;}"
       "fn main() {3 / 4;}"
       "fn main() {3 % 4;}"
       "fn main() {3 ** 4;}"
       "fn main() {3 ** --4;}"
       "fn main() {--3 ** --4;}"
       "fn main() {--3 - --4;}"
       "fn main() {--3 + --4;}"
       "fn main() {--3 + --4;}"
       "fn main() {--3 + ----4;}"
       "fn main() {--3 + ++4;}"
       "fn main() {--3 + ++--4;}"
       "fn main() {3 + 4 + 3;}"
       "fn main() {3 * 4 * 3;}"
       "fn main() {3 * 4 / 3;}"
       "fn main() {(3 + 5);}"
       "fn main() {(3 + 5) + 5;}"
       "fn main() {3 + (5 + 5);}"
    );
    test_parse!(functions
        "fn main() { test();}"
        "fn main() { test(kees);}"
        "fn main() { test(kees, 3, vierentwintig, 8 + 8, \"test\");}"
        "fn test() { }"
        "fn test(x: u8) { }"
        "fn test(x: u8, y: u8, z: u8) { }"
        "fn test(x: *u8) { }"
        "fn test(x: ***u8) { }"
    );
}
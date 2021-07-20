
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

#[derive(Clone, PartialEq, Debug)]
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

#[derive(Debug)]
pub struct Dir<'s> {
    pub dir: Box<Expression<'s>>,
    pub block: Block<'s>,
}

impl<'s> Dir<'s> {
    pub fn parse(block: Pair<'s>) -> Result<Self, ParseProgramError> {
        let mut inner = block.into_inner();

        let dir = Expression::parse_expr(inner.next().unwrap())?;
        let block = Block::parse(inner.next().unwrap())?;

        Ok(Self {
            dir: Box::new(dir),
            block
        })
    }
}

#[derive(Debug)]
pub enum BlockExpr<'s> {
    Dir(Dir<'s>)
}

impl<'s> BlockExpr<'s> {
    pub fn parse(block: Pair<'s>) -> Result<Self, ParseProgramError> {
        let mut inner = block.into_inner();
        let next = inner.next().unwrap();

        Ok(match next.as_rule() {
            Rule::dir => {
                BlockExpr::Dir(Dir::parse(next)?)
            }
            _ => unreachable!()
        })
    }
}

#[derive(Debug)]
pub enum Atom<'s> {
    String(Cow<'s, str>),
    Integer(i64),
    Float(f64),
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
            Rule::float => {
                let float_str = next.as_str();
                let float = float_str
                    .parse()
                    .map_err(|e| ParseProgramError::ParseFloat(next.as_str(), e))?;

                Self::Float(float)
            },
            Rule::name => Self::Name(
                Name::new(next.as_str())
            ),
            Rule::block_expr => {
                Self::Block(BlockExpr::parse(next)?)
            },
            _ => unreachable!()
        })
    }
}

#[derive(Debug)]
pub enum Primary<'s> {
    Atom(Atom<'s>),
    Expression(Box<ExpressionType<'s>>)
}

impl<'s> Primary<'s> {
    pub fn parse(expr: Pair<'s>) -> Result<Self, ParseProgramError> {
        let mut inner = expr.into_inner();

        let possible_atom = inner.next().unwrap();
        Ok(match possible_atom.as_rule() {
            Rule::expr => {
                Self::Expression(
                    Box::new(Expression::parse_expr_type(
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
    pub params: Vec<Primary<'s>>,
}

impl<'s> FuncCall<'s> {
    pub fn parse(expr: Pair<'s>) -> Result<Self, ParseProgramError> {
        let mut inner = expr.into_inner();

        let callee = Primary::parse(inner.next().unwrap())?;
        let mut params = Vec::new();
        for param in inner {
            params.push(Primary::parse(param)?);
        }

        Ok(Self {
            callee,
            params
        })
    }
}

#[derive(Debug)]
pub enum ExpressionType<'s> {
    Add(Box<ExpressionType<'s>>, Box<ExpressionType<'s>>),
    Subtract(Box<ExpressionType<'s>>, Box<ExpressionType<'s>>),
    Mult(Box<ExpressionType<'s>>, Box<ExpressionType<'s>>),
    Divide(Box<ExpressionType<'s>>, Box<ExpressionType<'s>>),
    IntegerDivide(Box<ExpressionType<'s>>, Box<ExpressionType<'s>>),
    Modulo(Box<ExpressionType<'s>>, Box<ExpressionType<'s>>),
    Exponent(Box<ExpressionType<'s>>, Box<ExpressionType<'s>>),
    Negate(Box<ExpressionType<'s>>),
    FuncCall(FuncCall<'s>)
}

#[derive(Debug)]
pub enum StreamParticle<'s> {
    Filter(Box<Expression<'s>>),
    Map(Box<Expression<'s>>),
}

#[derive(Debug)]
pub struct Expression<'s> {
    pub et: ExpressionType<'s>,
    pub streams: Vec<StreamParticle<'s>>
}

impl<'s> Expression<'s> {
    fn parse_sum(sum: Pair<'s>) -> Result<ExpressionType, ParseProgramError> {
        let mut inner = sum.into_inner();
        let mut left = Self::parse_term(
            inner.next().unwrap()
        )?;

        while let Some(i) = inner.next() {
            left = match i.as_str() {
                "+" => ExpressionType::Add(
                    Box::new(left),
                    Box::new(Self::parse_term(inner.next().unwrap())?)
                ),
                "-" => ExpressionType::Subtract(
                    Box::new(left),
                    Box::new(Self::parse_term(inner.next().unwrap())?)
                ),
                _ => unreachable!()
            }
        }

        Ok(left)
    }

    fn parse_term(term: Pair<'s>) -> Result<ExpressionType, ParseProgramError> {
        let mut inner = term.into_inner();
        let mut left = Self::parse_factor(
            inner.next().unwrap()
        )?;

        while let Some(i) = inner.next() {
            left = match i.as_str() {
                "*" => ExpressionType::Mult(
                    Box::new(left),
                    Box::new(Self::parse_factor(inner.next().unwrap())?)
                ),
                "/" => ExpressionType::Divide(
                    Box::new(left),
                    Box::new(Self::parse_factor(inner.next().unwrap())?)
                ),
                "//" => ExpressionType::IntegerDivide(
                    Box::new(left),
                    Box::new(Self::parse_factor(inner.next().unwrap())?)
                ),
                "%" => ExpressionType::Modulo(
                    Box::new(left),
                    Box::new(Self::parse_factor(inner.next().unwrap())?)
                ),
                _ => unreachable!()
            };
        }

        Ok(left)
    }

    fn parse_factor(factor: Pair<'s>) -> Result<ExpressionType, ParseProgramError> {
        let mut inner = factor.into_inner();

        let next = inner.next().unwrap();


        Ok(match next.as_str() {
            "+" => Self::parse_factor(
                inner.next().unwrap()
            )?,
            "-" => {
                ExpressionType::Negate(Box::new(Self::parse_factor(
                    inner.next().unwrap()
                )?))
            },
            _ => Self::parse_power(
                next
            )?,
        })
    }

    fn parse_power(factor: Pair<'s>) -> Result<ExpressionType, ParseProgramError> {
        let mut inner = factor.into_inner();

        let left = ExpressionType::FuncCall(FuncCall::parse(
            inner.next().unwrap()
        )?);


        if let Some(_ /* this is the ** operator */) = inner.next() {
            let right = Self::parse_factor(
                inner.next().unwrap()
            )?;

            Ok(ExpressionType::Exponent(Box::new(left), Box::new(right)))
        } else {
            Ok(left)
        }

    }

    fn parse_expr_type(expr: Pair<'s>) -> Result<ExpressionType, ParseProgramError> {
        Self::parse_sum(expr.into_inner().next().unwrap())
    }

    fn parse_expr(expr: Pair<'s>) -> Result<Self, ParseProgramError> {
        Ok(Self {
            et: Self::parse_expr_type(expr)?,
            streams: vec![],
        })
    }

    pub fn parse_extended_expr(expr: Pair<'s>) -> Result<Self, ParseProgramError> {
        let mut inner = expr.into_inner();
        let et = Self::parse_expr_type(inner.next().unwrap())?;

        Ok(Self {
            et,
            streams: vec![],
        })
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
        let expr = Expression::parse_extended_expr(expression)?;

        Ok(Self {
            to: Name::new(name_string),
            expr,
        })
    }
}

#[derive(Debug)]
pub enum Line<'s> {
    Assignment(Assignment<'s>),
    Expression(Expression<'s>),
}

impl<'s> Line<'s> {
    pub fn parse(line: Pair<'s>) -> Result<Self, ParseProgramError> {
        let inner = line.into_inner().next().unwrap();
        Ok(match inner.as_rule() {
            Rule::assignment => {
                Line::Assignment(Assignment::parse(inner)?)
            }
            Rule::extended_expr => {
                Line::Expression(Expression::parse_extended_expr(inner)?)
            }
            _ => unreachable!()
        })
    }
}

#[derive(Debug)]
pub struct Program<'s> {
    pub lines: Vec<Line<'s>>
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
                Rule::line => lines.push(Line::parse(line)?),
                Rule::EOI => {},
                _ => unreachable!()
            }
        }

        Ok(Self {
            lines,
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
                    let _ = Program::from_string($inp).unwrap();
                )*
            }
        };
    }

    test_parse!(assignment_and_lines
        "a = 3"
        "a = 3;"
        "a = 3; b = 4"
        "a = 3; b = 4;"
        "a = 3


        b = 3
        "
        "a = 3


        b = 3"
        "a = 3


        b = 3;"
        "a = 3;


        b = 3;"
    );

    test_parse!(arithmetic
        "3 + 4"
        "3 - 4"
        "3 * 4"
        "3 / 4"
        "3 // 4"
        "3 % 4"
        "3 ** 4"
        "3 ** -4"
        "-3 ** -4"
        "-3 - -4"
        "-3 + -4"
        "-3 + --4"
        "-3 + -----4"
        "-3 + +4"
        "-3 + +-+-+4"
        "3 + 4 + 3"
        "3 * 4 * 3"
        "3 * 4 / 3"
        "(3 + 5)"
        "(3 + 5) + 5"
        "3 + (5 + 5)"
    );
}
use logos::{Filter, Lexer, Logos};
use std::num::ParseIntError;
use thiserror::Error;

#[derive(Error, Default, Clone, PartialEq, Debug)]
pub enum LexErrorKind {
    #[default]
    #[error("unknown character")]
    UnknownCharacter,
    #[error("integer error")]
    IntError(#[source] ParseIntError),
}

#[derive(Error, Clone, PartialEq, Debug)]
#[error("error tokenising lexeme '{lexeme}'")]
pub struct LexError<'a> {
    lexeme: &'a str,
    source: LexErrorKind,
}

impl From<ParseIntError> for LexErrorKind {
    fn from(value: ParseIntError) -> Self {
        Self::IntError(value)
    }
}

fn keep_newline(lex: &mut Lexer<Token>) {
    lex.extras = true;
}

fn ignore_newline(lex: &mut Lexer<Token>) {
    lex.extras = false;
}

#[derive(Logos, Hash, PartialEq, Eq, Clone, Debug)]
#[logos(extras = bool)]
#[logos(error = LexErrorKind)]
#[logos(skip r"[\s--\n]")]
pub enum Token {
    #[token("=", ignore_newline)]
    Assign,
    #[token(":", keep_newline)]
    OfType,
    #[token("type", keep_newline)]
    TypeDef,
    #[token("+", ignore_newline)]
    TypeSum,
    #[token("(", ignore_newline)]
    LeftParen,
    #[token(")", keep_newline)]
    RightParen,
    #[regex(r"λ|\\", keep_newline)]
    Lambda,
    #[regex(r"->|→", ignore_newline)]
    Arrow,
    #[token("let", keep_newline)]
    Let,
    #[token("in", ignore_newline)]
    In,
    #[token("match", keep_newline)]
    Match,
    #[token("with", ignore_newline)]
    With,
    #[regex(r"=>|⇒", ignore_newline)]
    FatArrow,
    #[token("builtin", keep_newline)]
    BuiltIn,
    #[token("()", keep_newline)]
    Unit,
    #[regex(r"[0-9]+", |lex| { keep_newline(lex); lex.slice().parse() })]
    Int(i64),
    #[regex(r"[A-Za-z][A-Za-z0-9]*", |lex| { keep_newline(lex); lex.slice().to_owned() })]
    Ident(String),
    #[regex(r"_[A-Za-z0-9_]*", keep_newline)]
    Discard,
    #[token(";", ignore_newline)]
    #[regex(r"\n+", |lex| if lex.extras { Filter::Emit(()) } else { Filter::Skip })]
    Separator,
}

pub fn lex(source: &str) -> Result<Vec<Token>, LexError> {
    let mut lexer = Token::lexer(source);
    let mut tokens = Vec::new();
    while let Some(token) = lexer.next() {
        tokens.push(token.map_err(|source| LexError {
            lexeme: lexer.slice(),
            source,
        })?);
    }
    Ok(tokens)
}

use logos::{Filter, Lexer, Logos};
use std::num::ParseIntError;
use thiserror::Error;

#[derive(Error, Default, Clone, PartialEq, Eq, Debug)]
pub enum LexErrorKind {
    #[default]
    #[error("unknown character")]
    UnknownCharacter,
    #[error("integer error")]
    IntError(#[from] ParseIntError),
}

#[derive(Error, Clone, PartialEq, Debug)]
#[error("error tokenising lexeme '{lexeme}'")]
pub struct LexError<'a> {
    lexeme: &'a str,
    source: LexErrorKind,
}

fn emit_eol<'source>(lex: &mut Lexer<'source, Token<'source>>) {
    lex.extras = true;
}

fn skip_eol<'source>(lex: &mut Lexer<'source, Token<'source>>) {
    lex.extras = false;
}

#[derive(Logos, Hash, PartialEq, Eq, Clone, Debug)]
#[logos(extras = bool)]
#[logos(error = LexErrorKind)]
#[logos(skip r"[\s--\n]")]
pub enum Token<'source> {
    #[token("=", skip_eol)]
    Assign,
    #[token(":", emit_eol)]
    OfType,
    #[token("type", emit_eol)]
    TypeDef,
    #[token("+", skip_eol)]
    TypeSum,
    #[token("(", skip_eol)]
    LeftParen,
    #[token(")", emit_eol)]
    RightParen,
    #[regex(r"λ|\\", emit_eol)]
    Lambda,
    #[regex(r"->|→", skip_eol)]
    Arrow,
    #[token("let", emit_eol)]
    Let,
    #[token("in", skip_eol)]
    In,
    #[token("match", emit_eol)]
    Match,
    #[token("with", skip_eol)]
    With,
    #[regex(r"=>|⇒", skip_eol)]
    FatArrow,
    #[token("builtin", emit_eol)]
    BuiltIn,
    #[token("()", emit_eol)]
    Unit,
    #[regex(r"[0-9]+", |lex| { emit_eol(lex); lex.slice().parse() })]
    Int(i64),
    #[regex(r"[A-Za-z][A-Za-z0-9]*", |lex| { emit_eol(lex); lex.slice() })]
    Ident(&'source str),
    #[regex(r"_[A-Za-z0-9_]*", emit_eol)]
    Discard,
    #[token(";", skip_eol)]
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

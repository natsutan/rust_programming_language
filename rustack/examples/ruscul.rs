// https://github.com/msakuta/ruscal/tree/master/examples
use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::{
      alpha1, alphanumeric1, char, multispace0
  },
  combinator::recognize,
  multi::{fold_many0, many0},
  sequence::{delimited, pair},
  IResult
};


#[derive(Debug, PartialEq)]
enum Token<'src> {
    Ident(&'src str),
    Number(f64),
    LParen,
    RParen
}

#[derive(Debug, PartialEq)]
enum TokenTree<'src> {
    Token(Token<'src>),
    Tree(Vec<TokenTree<'src>>)
}

#[derive(Debug, PartialEq)]
enum Expression<'src> {
    Ident(&'src str),
    NumLiteral(f64),
    Add(Box<Expression<'src>>, Box<Expression<'src>>)
}



fn token(i :&str) -> Option<(&str, Token)> {
    if let Some(res) = ident(whitespace(i)) {
        return Some(res)
    }
    if let Some(number_res) = number(whitespace(i)) {
        return Some(number_res);
    }
    if let Some(lparen_res) = lparen(whitespace(i)) {
        return Some(lparen_res)
    }
    if let Some(rparen_res) = rparen(whitespace(i)) {
        return Some(rparen_res)
    }

    None
}

fn advance_char(input: &str) -> &str {
    let mut chars = input.chars();
    chars.next();
    chars.as_str()
}

fn peek_char(input: &str) -> Option<char> {
    input.chars().next()
}

fn lparen(mut input: &str) -> (&str, Option<Token>) {
    if matches!(peek_char(input), Some('(')) {
        input = advance_char(input);
        (input , Some(Token::LParen))
    } else {
        (input, None)
    }
}

fn rparen(mut input: &str) -> (&str, Option<Token>) {
    if matches!(peek_char(input), Some(')')) {
        input = advance_char(input);
        (input , Some(Token::LParen))
    } else {
        (input, None)
    }
}


fn ident(mut input: &str) -> Option<(&str, Token)> {
    let start = input;
    if matches!(peek_char(input), Some(_x @ ('a'..='z' | 'A'..='Z'))) {
        input = advance_char(input);
        while matches!(peek_char(input), Some(_x @ ('a'..='z' | 'A'..='Z' | '0'..='9'))) {
            input = advance_char(input);
        }
        Some((input, Token::Ident(&start[..(start.len() - input.len())])))
    } else {
        None
    }
}

fn whitespace(mut input: &str) -> &str {
    while matches!(peek_char(input), Some(' ')) {
        input = advance_char(input);
    }
    input
}

fn number(mut input: &str) -> (&str, Option<Token>) {
    if matches!(peek_char(input), Some(_x @ ('-' | '+' | '.' | '0' ..='9'))) {
        input = advance_char(input);

        while matches!(peek_char(input), Some(_x @ ('.' | '0'..='9'))) {
            input = advance_char(input);
        }
        (input , Some(Token::Number))
    } else {
        (input, None)
    }
}

fn expr(input: &str) -> Option<(&str, Expression)> {
    if let Some(res) = add(input) {
        return Some(res);
    }
    if let Some(res) = term(input) {
        return Some(res);
    }
    None
}

fn term(input: &str) -> Option<(&str, Expression)> {
    if let Some(res) = paren(input) {
        return Some(res);
    }
    if let Some(res) = token(input) {
        return Some(res);
    }
    None
}

fn paren(input: &str) -> Option<(&str, Expression)> {
    let next_input = lparen(whitespace(input))?;
    let (next_input ,expr ) = expr(next_input)?;
    let next_input = rparen(whitespace(next_input))?;

    Some((next_input, expr))
}

fn add_term(input: &str) -> Option<(&str, Expression)> {
    let (next_input, lhs) = term(input)?;
    let next_input = plus(whitespace(next_input))?;

    Some((next_input, lhs))
}

fn add(mut input: &str) -> Option<(&str, Expression)> {
    let mut left = None;

    while let Some((next_input, expr)) = add_term(input) {
        if let Some(prev_left) = left {
            left = Some(Expression::Add(
                Box::new(prev_left),
                Box::new(expr),
            ));
        } else {
            left = Some(expr);
        }
        input = next_input;
    }

    let left = left?;

    let (next_input, rhs) = expr(input)?;

    Some((
        next_input,
        Expression::Add(Box::new(left), Box::new(rhs))
    ))

}

fn plus(mut input: &str) -> Option<&str> {
    if matches!(peek_char(input), Some('+')) {
        Some(advance_char(input))
    } else{
        None
    }
}
fn source(mut input:& str) -> (&str, TokenTree) {
    let mut tokens = vec![];

    while !input.is_empty() {
        input = if let (next_input, Some(token)) = token(input) {
            match token{
                Token::LParen => {
                    let (next_input, tt) = source(next_input);
                    tokens.push(tt);
                    next_input
                }
                Token::RParen => {
                    return (next_input, TokenTree::Tree(tokens))
                }
                _ => {
                    tokens.push(TokenTree::Token(token));
                    next_input
                }
            }


        } else {
            break
        }
    }


    (input, TokenTree::Tree(tokens))
}

fn main() {
    let input = "Hello World";
    println!(
        "source: {}, parsed: {:?}", input, source(input)
    );

    let input = "(123 456 ) world";
    println!(
        "source: {}, parsed: {:?}", input, source(input)
    );

    let input = "((car cdr) cdr)";
    println!(
        "source: {}, parsed: {:?}", input, source(input)
    );

}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_whitespace() {
        assert_eq!(whitespace("    "), "");
    }

    #[test]
    fn test_ident() {
        assert_eq!(ident("Adam"),  ("", Some(Token::Ident)));
    }

    #[test]
    fn test_number() {
        assert_eq!(number("123.45 "), (" ", Some(Token::Number)));
    }



}
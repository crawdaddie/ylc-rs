extern crate ylc;

use std::io::{self, Read, Write};
use ylc::lexer::Token;
use ylc::parser::{parse, Ast};
use ylc::symbols::{Numeric, Ttype};

fn format_explicit_ttype<B: Write>(ttype: &Ttype, indent: usize, buffer: &mut B) -> () {
    match ttype {
        Ttype::Var(var) => (),
        Ttype::Numeric(numeric) => {
            let identifier = match numeric {
                Numeric::Int8 => "int8",
                Numeric::Int => "int",
                Numeric::Num => "double",
            };

            let _ = write!(buffer, "{}", identifier);
        }
        Ttype::MaxNumeric(v) => (),
        Ttype::Str => {
            let _ = write!(buffer, "str");
        }
        Ttype::Bool => {
            let _ = write!(buffer, "bool");
        }
        Ttype::Tuple(types) => (),
        // Ttype::Struct => write!(f, "Struct"),
        // Ttype::Void => write!(f, "Void"),
        Ttype::Fn(params) => (),
        Ttype::FnRecRef => (),
        // Ttype::Ptr => write!(f, "Ttype::Ptr"),
        Ttype::List(inner_type) => (),
        // Ttype::Uptr => write!(f, "Ttype::Uptr"),
        // Ttype::Nth(i, t) => write!(f, "{:?}[{}]", t, i),
        // Ttype::Application(n, args, _) => write!(f, "apply({:?} ({:?}))", n, args),
        Ttype::Union(v) => (),
        Ttype::Nth(t, i) => (),
    }
}
fn write_tabs<B: Write>(buf: &mut B, indent: usize) {
    let tabs: String = std::iter::repeat('\t').take(indent).collect();
    let _ = write!(buf, "{}", tabs);
}
fn format_arrayed_items<B: Write>(
    els: &Vec<Ast>,
    begin: &str,
    end: &str,
    indent: usize,
    buffer: &mut B,
) {
    let list_len = els.len();
    write!(buffer, "{}", begin);

    for (index, p) in els.iter().enumerate() {
        if list_len > 4 {
            write!(buffer, "\n");
            write_tabs(buffer, indent + 1);
        }
        format_ast(p, indent, buffer);

        if list_len > 4 || (index < list_len - 1 && list_len > 1) {
            write!(buffer, ", ");
        }
    }

    if list_len > 4 {
        write!(buffer, "\n");
        write_tabs(buffer, indent);
    }

    write!(buffer, "{}", end);
}
fn format_operator(tok: &Token) -> &str {
    match tok {
        Token::Dot => ".",
        Token::DoubleDot => "..",
        Token::TripleDot => "...",
        Token::Minus => "-",
        Token::Plus => "+",
        Token::Bang => "!",
        Token::Modulo => "%",
        Token::Slash => "/",
        Token::Star => "*",
        Token::Assignment => "=",
        Token::Equality => "==",
        Token::NotEqual => "!=",
        Token::Lt => "<",
        Token::Gt => ">",
        Token::Lte => "<=",
        Token::Gte => ">=",
        _ => "",
    }
}

fn format_body<B: Write>(body: &Vec<Ast>, indent: usize, buffer: &mut B) -> () {
    if body.len() > 0 {
        write!(buffer, " {{\n");
        for l in body {
            write_tabs(buffer, indent + 1);
            format_ast(l, indent + 1, buffer);
            write!(buffer, " \n");
            // write_tabs(buffer, indent + 1);
        }

        write_tabs(buffer, indent);
        write!(buffer, "}}");
    }
}

fn format_ast<B: Write>(ast: &Ast, indent: usize, buffer: &mut B) -> () {
    match ast {
        Ast::Let(
            id,
            _t,          // optional explicit type parameter
            Some(value), // optional immediate assignment expression
        ) => {
            let _ = write!(buffer, "let {} = ", id);
            format_ast(value, indent, buffer);
        }

        Ast::Let(id, Some(t), None) => {
            let _ = write!(buffer, "let {}: ", id);
            format_explicit_ttype(t, indent, buffer);
        }

        Ast::Let(id, None, None) => {
            let _ = write!(buffer, "let {}\n", id);
            write_tabs(buffer, indent);
        }

        Ast::FnDeclaration(id, fn_expr) => {
            let _ = write!(buffer, "let {} = fn ", id);
            format_ast(fn_expr, indent, buffer);
        }
        Ast::Fn(params, body, t) => {
            format_arrayed_items(params, "(", ")", indent, buffer);

            if let Ttype::Fn(fn_types) = t {
                let ret_type = fn_types.last().unwrap();
                match ret_type {
                    Ttype::Var(_) => (),
                    _ => {
                        write!(buffer, ": ");
                        format_explicit_ttype(ret_type, indent, buffer);
                    }
                }
            }
            format_body(body, indent, buffer);
        }

        Ast::TypeDeclaration(id, type_expr) => (),

        Ast::Id(id, t) => {
            let _ = write!(buffer, "{}", id);
            match t {
                Ttype::Var(_) => {}
                _ => {
                    write!(buffer, ": ");
                    format_explicit_ttype(t, indent, buffer);
                }
            }
        }
        Ast::Binop(tok, l_box, r_box, _) => {
            format_ast(l_box, indent, buffer);
            let tok_str = format_operator(tok);
            let _ = write!(buffer, " {} ", tok_str);
            format_ast(r_box, indent, buffer);
        }
        Ast::Unop(tok, operand_box, t) => {
            let tok_str = format_operator(tok);
            let _ = write!(buffer, "{}", tok_str);
            format_ast(operand_box, indent, buffer);
        }

        Ast::Tuple(els, t) => {
            format_arrayed_items(els, "(", ")", indent, buffer);
        }

        Ast::List(els, t) => {
            format_arrayed_items(els, "[", "]", indent, buffer);
        }

        Ast::Index(l, i, t) => {
            format_ast(l, indent, buffer);
            write!(buffer, "[");
            format_ast(i, indent, buffer);
            write!(buffer, "]");
        }

        Ast::Assignment(_, _, t) => (),

        Ast::Call(callee, call_args, t) => {
            format_ast(callee, indent, buffer);
            format_arrayed_items(call_args, "(", ")", indent, buffer);
        }
        Ast::If(cond, then, elze, t) => {
            write!(buffer, "if ");
            format_ast(cond, indent, buffer);
            format_body(then, indent, buffer);
            if let Some(elze) = elze {
                write!(buffer, " else ");
                format_body(elze, indent, buffer);
            }
        }
        Ast::Match(matched, match_arms, t) => {
            let _ = write!(buffer, "match ");
            format_ast(matched, indent, buffer);
            let _ = write!(buffer, "\n");
            for (case, match_body) in match_arms {
                write_tabs(buffer, indent);
                let _ = write!(buffer, "| ");
                match case {
                    Ast::Unop(Token::If, tuple, _) => {
                        if let Ast::Tuple(tuple_vec, _) = *tuple.clone() {
                            format_ast(&tuple_vec[0], indent, buffer);
                            let _ = write!(buffer, " if ");
                            format_ast(&tuple_vec[1], indent, buffer);
                        }
                    }
                    _ => format_ast(case, indent, buffer),
                }
                let _ = write!(buffer, " -> ");
                let body_len = match_body.len();
                if body_len > 1 {
                    let _ = write!(buffer, "{{\n");
                }
                for l in match_body {
                    if body_len > 1 {
                        write_tabs(buffer, indent + 1);
                    }

                    format_ast(l, indent + 1, buffer);
                    let _ = write!(buffer, "\n");
                }

                if body_len > 1 {
                    write_tabs(buffer, indent);
                    let _ = write!(buffer, "}}\n");
                }
            }
        }

        Ast::Int8(i) => {
            let _ = write!(buffer, "{}", i);
        }
        Ast::Integer(i) => {
            let _ = write!(buffer, "{}", i);
        }
        Ast::Number(f) => {
            let _ = write!(buffer, "{}", f);
        }
        Ast::Bool(b) => {
            let _ = write!(buffer, "{}", b);
        }
        Ast::String(s) => {
            let _ = write!(buffer, "{:?}", s);
        }
        Ast::VarArg => {
            let _ = write!(buffer, "...");
        }
    }
}

fn main() -> Result<(), io::Error> {
    // let args: Vec<String> = env::args().collect();
    // let input_src = read_file_to_string(args[1].as_str())?;
    let mut input_src = Vec::new();
    io::stdin().read_to_end(&mut input_src)?;

    let program = parse(String::from_utf8(input_src).unwrap());

    for s in &program {
        format_ast(s, 0, &mut io::stdout());
        let _ = write!(io::stdout(), "\n\n");
        // print!("\n");
    }
    let _ = write!(io::stdout(), "\n");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    use ylc::int_expr;

    #[test]
    fn test_format_ast() {
        // Create a mock buffer for testing
        struct MockBuffer(Vec<u8>);

        impl Write for MockBuffer {
            fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
                self.0.extend_from_slice(buf);
                Ok(buf.len())
            }

            fn flush(&mut self) -> io::Result<()> {
                Ok(())
            }
        }

        // Create an instance of Ast
        let ast = Ast::Let("x".to_string(), None, Some(Box::new(int_expr!(1))));

        // Call format_ast with the mock buffer
        let mut mock_buffer = MockBuffer(Vec::new());
        format_ast(&ast, 0, &mut mock_buffer);

        // Verify the content written to the mock buffer
        let expected_content = "let x = 1";
        assert_eq!(
            String::from_utf8(mock_buffer.0).unwrap(),
            expected_content.to_string()
        );
    }
}

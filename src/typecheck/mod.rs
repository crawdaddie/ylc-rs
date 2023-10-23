use crate::{
    lexer::Token,
    parser::{Ast, Program},
    symbols::Ttype,
};

/*
typedef enum {
  T_VAR,    // 'x
  T_INT8,   // int8 - alias char
  T_INT,    // int
  T_NUM,    // double
  T_STR,    // str
  T_BOOL,   // bool
  T_TUPLE,  // struct or tuple
  T_STRUCT, // struct or tuple
  T_VOID,   // void
  T_FN,     // t1 -> t2 -> ... -> return_type
  T_PTR,    // &'x
  T_ARRAY,  // 't[n]
  T_UPTR,
} ttype_tag;
*/

#[derive(Debug, Clone, PartialEq)]
pub struct Constraint {
    lhs: Ttype,
    rhs: Ttype,
}
pub fn infer_types(expr: &mut Program) {
    let mut cons = vec![];
    for mut e in expr {
        generate_constraints(&mut e, &mut cons);
    }
    for c in &cons {
        println!("Constraint: {:?}", c);
    }
}

pub static mut TVAR_COUNT: usize = 0;

fn tvar() -> Ttype {
    let tv;
    unsafe {
        tv = Ttype::Var(format!("t{}", TVAR_COUNT));
        TVAR_COUNT += 1;
    };
    tv
}

pub fn generate_constraints(expr: &mut Ast, constraints: &mut Vec<Constraint>) {
    match expr {
        Ast::Let(
            id,
            type_expr, // optional explicit type parameter
            expr,      // optional immediate assignment expression
        ) => {
            if let Some(type_expr) = type_expr {
                generate_constraints(&mut *type_expr, constraints);
            };

            if let Some(expr) = expr {
                generate_constraints(&mut *expr, constraints);
            };
        }
        Ast::FnDeclaration(id, fn_expr) => {
            fn_expr.ttype = tvar();
        }
        Ast::TypeDeclaration(id, type_expr) => {}

        // expressions
        Ast::Id(expr) => {
            expr.ttype = tvar();
        }
        Ast::Binop(expr) => {
            expr.ttype = tvar();
            generate_constraints(&mut *expr.left, constraints);
            generate_constraints(&mut *expr.right, constraints);
            match expr.token {
                Token::Plus | Token::Minus | Token::Star | Token::Slash | Token::Modulo => {
                    // numeric binop
                }
                Token::Equality
                | Token::NotEqual
                | Token::Lt
                | Token::Lte
                | Token::Gt
                | Token::Gte => {
                    // bool binop
                }

                _ => {}
            }
        }
        Ast::Unop(expr) => {
            expr.ttype = tvar();
            generate_constraints(&mut *expr.operand, constraints);
            match expr.token {
                Token::Minus => {
                    // numeric unop
                }
                Token::Bang => {
                    // boolean unop
                }
                _ => {}
            }
        }
        Ast::Tuple(exprs) => {
            for memb in &mut exprs.members {
                generate_constraints(memb, constraints);
            }
        }
        Ast::Index(expr) => {
            expr.ttype = tvar();
        }
        Ast::Assignment(expr) => {
            expr.ttype = tvar();
        }
        Ast::Fn(expr) => {
            expr.ttype = tvar();
        }

        // literals
        // Ast::Integer(_) => {}
        // Ast::Number(_) => {}
        // Ast::Bool(_) => {}
        // Ast::String(_) => {}
        _ => {}
    }

    // println!("generated constraints for {:?}", expr);
}

// #[cfg(test)]
// mod tests {
//     use crate::{lexer::Lexer, parser::Parser};
//
//     use super::*;
//     #[test]
//     fn test_constraints() {
//         let input = r#"
//         let a = 1
//         "#;
//         let mut parser = Parser::new(Lexer::new(input.into()));
//         let program = parser.parse_program();
//         let mut cons = vec![];
//         generate_constraints(program[0].clone(), &mut cons);
//         // assert_eq!(vec![], cons,)
//     }
// }

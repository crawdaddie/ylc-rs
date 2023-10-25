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
    println!("Constraints:");
    for c in &cons {
        println!("{:?}", c);
    }
    println!("");
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

fn push_constraint(left: Ttype, right: Ttype, constraints: &mut Vec<Constraint>) {
    match &left {
        Ttype::Var(_) => {
            constraints.push(Constraint {
                lhs: left,
                rhs: right,
            });
        }
        _ => {}
    }
}
fn max_numeric_type(l: Ttype, r: Ttype) -> Option<Ttype> {
    match (l, r) {
        (Ttype::Numeric(lnum), Ttype::Numeric(rnum)) => {
            Some(Ttype::Numeric(if rnum >= lnum { rnum } else { lnum }))
        }
        _ => None,
    }
}

pub fn generate_constraints(ast: &mut Ast, constraints: &mut Vec<Constraint>) {
    ast.set_ttype(tvar());
    match ast {
        Ast::Let(
            id,
            type_expr, // optional explicit type parameter
            expr,      // optional immediate assignment expression
        ) => {
            id.ttype = tvar();
            if let Some(type_expr) = type_expr {
                generate_constraints(&mut *type_expr, constraints);
                push_constraint(
                    id.ttype.clone(),
                    (*type_expr).get_ttype().unwrap(),
                    constraints,
                );
                if let Some(expr) = expr {
                    generate_constraints(&mut *expr, constraints);
                    push_constraint(
                        (*expr).get_ttype().unwrap(),
                        (*type_expr).get_ttype().unwrap(),
                        constraints,
                    );
                };
            };

            if let Some(expr) = expr {
                generate_constraints(&mut *expr, constraints);
                push_constraint(id.ttype.clone(), (*expr).get_ttype().unwrap(), constraints);
            };
        }
        Ast::FnDeclaration(id, fn_expr) => {}
        Ast::TypeDeclaration(id, type_expr) => {}

        // expressions
        Ast::Id(expr) => {}
        Ast::Binop(expr) => {
            let l = &mut *expr.left;
            let r = &mut *expr.right;

            generate_constraints(l, constraints);
            generate_constraints(r, constraints);

            if let (Some(l_type), Some(r_type)) = (l.get_ttype(), r.get_ttype()) {
                push_constraint(l_type.clone(), r_type.clone(), constraints);

                match expr.token {
                    Token::Plus | Token::Minus | Token::Star | Token::Slash | Token::Modulo => {
                        // numeric binop
                        if let Some(max_type) = max_numeric_type(l_type, r_type) {
                            push_constraint(expr.ttype.clone(), max_type, constraints);
                        } else {
                            // not numeric types??
                        }
                    }
                    Token::Equality
                    | Token::NotEqual
                    | Token::Lt
                    | Token::Lte
                    | Token::Gt
                    | Token::Gte => {
                        // bool binop
                        push_constraint(expr.ttype.clone(), Ttype::Bool, constraints)
                    }

                    _ => {}
                }
            }
        }
        Ast::Unop(expr) => {
            generate_constraints(&mut *expr.operand, constraints);
            match expr.token {
                Token::Minus => {
                    // numeric unop
                    push_constraint(
                        expr.ttype.clone(),
                        (*expr.operand).get_ttype().unwrap(),
                        constraints,
                    );
                }
                Token::Bang => {
                    // boolean unop
                    //
                    push_constraint(
                        expr.ttype.clone(),
                        (*expr.operand).get_ttype().unwrap(),
                        constraints,
                    );
                }
                _ => {}
            }
        }
        Ast::Tuple(exprs) => {
            let mut expr_types: Vec<Ttype> = vec![];
            for memb in &mut exprs.members {
                generate_constraints(memb, constraints);
                expr_types.push(memb.get_ttype().unwrap());
            }
            push_constraint(exprs.ttype.clone(), Ttype::Tuple(expr_types), constraints);
        }
        Ast::Index(expr) => {}
        Ast::Assignment(expr) => {
            let assignee = &mut *expr.assignee;
            let expr = &mut *expr.expr;

            generate_constraints(assignee, constraints);
            generate_constraints(expr, constraints);
            push_constraint(
                assignee.get_ttype().unwrap(),
                expr.get_ttype().unwrap(),
                constraints,
            );
        }
        Ast::Fn(expr) => {}
        Ast::If(expr) => {
            generate_constraints(&mut *expr.condition, constraints);

            push_constraint(
                (*expr.condition).get_ttype().unwrap().clone(),
                Ttype::Bool,
                constraints,
            );

            let mut then_types: Vec<Ttype> = vec![];
            for stmt in &mut expr.then {
                generate_constraints(stmt, constraints);
                then_types.push(stmt.get_ttype().unwrap());
            }
            if let Some(ret_type) = then_types.last() {
                push_constraint(expr.ttype.clone(), ret_type.clone(), constraints);
            }
            match expr.elze {
                Some(ref mut else_stmts) => {
                    let mut else_types: Vec<Ttype> = vec![];
                    for stmt in else_stmts {
                        generate_constraints(stmt, constraints);
                        else_types.push(stmt.get_ttype().unwrap());
                    }

                    if let Some(ret_type) = else_types.last() {
                        push_constraint(expr.ttype.clone(), ret_type.clone(), constraints);
                    }
                }
                _ => {}
            }
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

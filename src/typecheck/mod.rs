use crate::symbols::Env;
#[macro_use]
use crate::{
    lexer::Token,
    parser::{Ast, Expr, Program},
    symbols::{Numeric, Ttype},
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
    let mut env = Env::new();
    for mut e in expr {
        generate_constraints(&mut e, &mut cons, &mut env);
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
fn zip_option<T>(t: (Option<T>, Option<T>)) -> Option<(T, T)> {
    t.0.zip(t.1)
}

fn generate_body_constraints(
    body: &mut Vec<Ast>,
    final_ttype: Option<Ttype>,
    constraints: &mut Vec<Constraint>,
    env: &mut Env,
) {
    if body.len() == 0 {
        return;
    }

    let mut fin = body[0].clone();
    for s in body {
        generate_constraints(s, constraints, env);
        fin = s.clone();
    }

    match (final_ttype, fin.get_ttype()) {
        (Some(l), Some(r)) => {
            push_constraint(l, r, constraints);
        }
        _ => (),
    }
}

/// iterate over AST nodes, for those which return a value (Expr)
/// find type constraints and add them to the constraints array
pub fn generate_constraints(ast: &mut Ast, constraints: &mut Vec<Constraint>, env: &mut Env) {
    match ast {
        Ast::Expr(e) => ast.set_ttype(tvar()),
        _ => {}
    }

    match ast {
        Ast::Let(id, type_expr, value) => {}
        Ast::FnDeclaration(id, fn_expr) => {}
        Ast::TypeDeclaration(id, type_expr) => {}
        Ast::Expr(e) => match e {
            Expr::Id(id, ttype) => {
                // lookup id in env and constrain ttype to the lookup's ttype
            }
            Expr::Binop(token, left_box, right_box, ttype) => {}
            Expr::Unop(token, operand_box, ttype) => {}

            Expr::Tuple(exprs_vec, ttype) => {
                let mut ttv = vec![];
                for e in exprs_vec {
                    let mut a = Ast::Expr(e.clone());
                    generate_constraints(&mut a, constraints, env);
                    ttv.push(a.get_ttype().unwrap())
                }
                push_constraint(ttype.clone(), Ttype::Tuple(ttv), constraints);
            }

            Expr::Index(object_box, index_box, ttype) => {
                let mut obj = Ast::Expr(*object_box.clone());
                generate_constraints(&mut obj, constraints, env);
                // TODO: create Ttype::Array(last type of obj array types)
                // TODO: push_constraint obj.ttype == Ttype::Array['t]

                let mut idx = Ast::Expr(*index_box.clone());
                generate_constraints(&mut idx, constraints, env);
                push_constraint(
                    idx.get_ttype().unwrap(),
                    Ttype::Numeric(Numeric::Int),
                    constraints,
                );
                // TODO:
                // if obj has ttype Array<'t>, then ast node ttype == 't
            }
            Expr::Assignment(assignee_box, value_box, ttype) => {
                let mut ass = Ast::Expr(*assignee_box.clone());
                generate_constraints(&mut ass, constraints, env);
                let mut val = Ast::Expr(*value_box.clone());
                generate_constraints(&mut val, constraints, env);
                push_constraint(ttype.clone(), ass.get_ttype().unwrap(), constraints);
                push_constraint(ttype.clone(), val.get_ttype().unwrap(), constraints);
            }

            Expr::Fn(args_vec, ret_type, stmts, ttype) => {}
            // Expr::Body(stmts, ttype) => {} -- Expr::Body == Vec<Ast>
            Expr::If(condition, then, elze, ttype) => {
                let cond = &mut *condition;
                generate_constraints(cond, constraints, env);

                if let Some(b_type) = cond.get_ttype() {
                    push_constraint(b_type, Ttype::Bool, constraints);
                }
                generate_body_constraints(then, Some(ttype.clone()), constraints, env);

                if let Some(e) = elze {
                    generate_body_constraints(e, Some(ttype.clone()), constraints, env);
                }
            }
            Expr::Call(callee_box, params_vec, ttype) => {
                let mut callee = Ast::Expr(*callee_box.clone());
            }
            _ => (),
        },
        _ => (),
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{ast_expr, bool_expr, if_expr, int_expr, tuple_expr};
    use pretty_assertions::{assert_eq, assert_ne};
    macro_rules! t {
        ($name: expr) => {
            Ttype::Var($name.into())
        };
    }

    #[test]
    fn test_constraints() {
        let mut cons = vec![];
        let mut program = [Ast::Expr(if_expr!(
            ast_expr!(bool_expr!(true)),
            vec![ast_expr!(tuple_expr!(
                vec![Expr::Integer(1), Expr::Integer(2)],
                t!("tuple1")
            ))],
            Some(vec![ast_expr!(tuple_expr!(
                vec![int_expr!(3), int_expr!(4)],
                t!("tuple2")
            ))]),
            t!("if_expr_type")
        ))];
        let mut env = Env::new();
        generate_constraints(&mut program[0], &mut cons, &mut env);

        let ex: Vec<Constraint> = vec![
            Constraint {
                lhs: t!("tuple1"),
                rhs: Ttype::Tuple(vec![
                    Ttype::Numeric(Numeric::Int),
                    Ttype::Numeric(Numeric::Int),
                ]),
            },
            Constraint {
                lhs: t!("if_expr_type"),
                rhs: t!("tuple1"),
            },
            Constraint {
                lhs: t!("tuple2"),
                rhs: Ttype::Tuple(vec![
                    Ttype::Numeric(Numeric::Int),
                    Ttype::Numeric(Numeric::Int),
                ]),
            },
            Constraint {
                lhs: t!("if_expr_type"),
                rhs: t!("tuple2"),
            },
        ];
        assert_eq!(ex, cons)
    }
}

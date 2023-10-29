use std::ops::Deref;

use crate::{
    parser::Identifier,
    symbols::{Env, SymbolValue},
};
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constraint {
    lhs: Ttype,
    rhs: Ttype,
}

pub fn infer_types(expr: &mut Program) {
    let mut cons = vec![];
    let mut env = Env::new();
    env.push();
    for mut e in expr {
        generate_constraints(&mut e, &mut cons, &mut env);
    }
    env.pop();
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
    println!("Max numeric: {:?} {:?}", l, r);
    // Ttype::MaxNumeric(
    //     Box::new(l.get_ttype().unwrap()),
    //     Box::new(r.get_ttype().unwrap()),
    // ),
    match (&l, &r) {
        (Ttype::Numeric(lnum), rnum) => match rnum {
            Ttype::Numeric(rnum) => Some(Ttype::Numeric(if rnum >= lnum {
                rnum.clone()
            } else {
                lnum.clone()
            })),
            Ttype::Var(_) => Some(Ttype::MaxNumeric(Box::new(l), Box::new(r))),
            _ => None,
        },

        (lnum, Ttype::Numeric(rnum)) => match lnum {
            Ttype::Numeric(lnum) => Some(Ttype::Numeric(if rnum >= lnum {
                rnum.clone()
            } else {
                lnum.clone()
            })),
            Ttype::Var(_) => Some(Ttype::MaxNumeric(Box::new(l), Box::new(r))),
            _ => None,
        },
        (Ttype::Var(_), Ttype::Var(_)) => Some(Ttype::MaxNumeric(Box::new(l), Box::new(r))),
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
) -> Option<Ttype> {
    if body.len() == 0 {
        return None;
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
        _ => {}
    };

    fin.get_ttype()
}
fn generate_function_declaration_constraints(
    id: Identifier,
    fn_expr: Expr,
    constraints: &mut Vec<Constraint>,
    env: &mut Env,
) {
    match fn_expr {
        Expr::Fn(args_vec, _ret_type_ast, stmts, ttype) => {
            env.push();
            let mut fn_types = vec![];
            for mut arg in args_vec {
                generate_constraints(&mut arg, constraints, env);
                fn_types.push(arg.get_ttype().unwrap());
                match arg {
                    Ast::Expr(e) => match e {
                        Expr::Id(arg_id, arg_type) => {
                            env.bind_symbol(
                                arg_id.clone(),
                                SymbolValue::Variable(arg_type.clone()),
                            );
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }
            env.bind_symbol(id.clone(), SymbolValue::Function(ttype.clone()));

            // let mut final_stmt_type = stmts[0].get_ttype().unwrap();
            let final_stmt_type = generate_body_constraints(
                &mut stmts.clone(),
                None, // TODO: ret_type
                constraints,
                env,
            );

            fn_types.push(final_stmt_type.unwrap_or(tvar()));
            env.pop();

            let fn_type = Ttype::Fn(fn_types);
            env.bind_symbol(id, SymbolValue::Function(fn_type.clone()));
            push_constraint(ttype, fn_type, constraints);
        }
        _ => {}
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
        Ast::FnDeclaration(id, fn_expr) => {
            generate_function_declaration_constraints(
                id.to_owned(),
                fn_expr.to_owned(),
                constraints,
                env,
            );
        }
        Ast::TypeDeclaration(id, type_expr) => {}
        Ast::Expr(e) => match e {
            Expr::Id(id, ttype) => {
                // lookup id in env and constrain ttype to the lookup's ttype
                match env.lookup(id.to_string()) {
                    Some(SymbolValue::Variable(t)) | Some(SymbolValue::Function(t)) => {
                        push_constraint(ttype.clone(), t.clone(), constraints);
                    }
                    _ => {}
                }
            }
            Expr::Binop(token, left_box, right_box, ttype) => {
                let mut l = Ast::Expr(*left_box.clone());
                let mut r = Ast::Expr(*right_box.clone());
                generate_constraints(&mut l, constraints, env);
                generate_constraints(&mut r, constraints, env);
                match token {
                    Token::Plus | Token::Minus | Token::Star | Token::Slash | Token::Modulo => {
                        // numeric binop
                        if let Some(num_binop_type) =
                            max_numeric_type(l.get_ttype().unwrap(), r.get_ttype().unwrap())
                        {
                            push_constraint(ttype.clone(), num_binop_type, constraints);
                        }
                    }
                    _ => {}
                }
            }
            Expr::Unop(token, operand_box, ttype) => {
                let mut op = Ast::Expr(*operand_box.clone());
                generate_constraints(&mut op, constraints, env);
                match token {
                    Token::Minus => {}
                    _ => {}
                }
            }

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

            Expr::Fn(args_vec, ret_type, stmts, ttype) => {
                env.push();
                let mut fn_types = vec![];
                for arg in args_vec {
                    generate_constraints(arg, constraints, env);
                    fn_types.push(arg.get_ttype().unwrap());
                }

                let mut final_stmt_type = stmts[0].get_ttype().unwrap();
                for stmt in stmts {
                    generate_constraints(stmt, constraints, env);
                    final_stmt_type = stmt.get_ttype().unwrap();
                }
                fn_types.push(final_stmt_type);
                env.pop();
            }
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
                let callee_expr = *callee_box.to_owned();
                let mut callee_ast = Ast::Expr(callee_expr.clone());
                generate_constraints(&mut callee_ast, constraints, env);

                let fn_types_opt = match &callee_expr {
                    Expr::Id(id, _) => match env.lookup(id.into()) {
                        Some(SymbolValue::Function(Ttype::Fn(fn_types_vec))) => {
                            push_constraint(
                                ttype.clone(),
                                fn_types_vec.last().unwrap().clone(),
                                constraints,
                            );
                            Some(fn_types_vec.clone())
                        }
                        _ => None,
                    },
                    _ => None,
                };
                for (idx, param) in params_vec.iter().enumerate() {
                    let mut param_ast = Ast::Expr(param.clone());
                    generate_constraints(&mut param_ast, constraints, env);
                    match (&fn_types_opt, param.get_ttype()) {
                        (Some(fn_types), Some(param_type)) => {
                            push_constraint(param_type.clone(), fn_types[idx].clone(), constraints);
                        }
                        _ => {}
                    }
                }
            }
            _ => (),
        },
        _ => (),
    }
}

#[cfg(test)]
mod tests {

    use std::collections::HashSet;

    use super::*;
    use crate::{
        ast_wrap, binop_expr, bool_expr, call_expr, id_expr, if_expr, int_expr, tuple_expr,
    };
    use pretty_assertions::{assert_eq, assert_ne};
    macro_rules! t {
        ($name: expr) => {
            Ttype::Var($name.into())
        };
    }
    fn assert_eq_unordered<T: PartialEq + std::hash::Hash + Eq + std::fmt::Debug>(
        expected: Vec<T>,
        actual: Vec<T>,
    ) {
        let expected_set: HashSet<_> = expected.iter().collect();
        let actual_set: HashSet<_> = actual.iter().collect();

        assert_eq!(expected_set, actual_set);
    }

    #[test]
    fn test_constraints() {
        let mut cons = vec![];
        let mut program = vec![Ast::Expr(if_expr!(
            ast_wrap!(bool_expr!(true)),
            vec![ast_wrap!(tuple_expr!(
                vec![Expr::Integer(1), Expr::Integer(2)],
                t!("tuple1")
            ))],
            Some(vec![ast_wrap!(tuple_expr!(
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
        assert_eq_unordered::<Constraint>(ex, cons)
    }

    #[test]
    fn test_simple_constraints() {
        let tests: Vec<(Vec<Ast>, Vec<Constraint>)> = vec![
            (
                vec![Ast::FnDeclaration(
                    "f".into(),
                    Expr::Fn(
                        vec![
                            Ast::Expr(id_expr!("a", Ttype::tvar("arg_a"))),
                            Ast::Expr(id_expr!("b", Ttype::tvar("arg_b"))),
                        ],
                        None,
                        vec![Ast::Expr(binop_expr!(
                            Token::Plus,
                            id_expr!("a", Ttype::tvar("arg_a_ref")),
                            id_expr!("b", Ttype::tvar("arg_b_ref")),
                            Ttype::tvar("fn_return")
                        ))],
                        Ttype::tvar("fn_type"),
                    ),
                )],
                vec![
                    Constraint {
                        lhs: Ttype::tvar("arg_a_ref"),
                        rhs: Ttype::tvar("arg_a"),
                    },
                    Constraint {
                        lhs: Ttype::tvar("arg_b_ref"),
                        rhs: Ttype::tvar("arg_b"),
                    },
                    Constraint {
                        lhs: Ttype::tvar("fn_return"),
                        rhs: Ttype::MaxNumeric(
                            Box::new(Ttype::tvar("arg_a_ref")),
                            Box::new(Ttype::tvar("arg_b_ref")),
                        ),
                    },
                    Constraint {
                        lhs: Ttype::tvar("fn_type"),
                        rhs: Ttype::Fn(vec![
                            Ttype::tvar("arg_a"),
                            Ttype::tvar("arg_b"),
                            Ttype::tvar("fn_return"),
                        ]),
                    },
                ],
            ),
            // Binop
            (
                vec![ast_wrap!(binop_expr!(
                    Token::Plus,
                    int_expr!(1),
                    int_expr!(2),
                    Ttype::tvar("+")
                ))],
                vec![Constraint {
                    lhs: Ttype::tvar("+"),
                    rhs: Ttype::Numeric(Numeric::Int),
                }],
            ),
            // Binop generic
            (
                vec![ast_wrap!(binop_expr!(
                    Token::Plus,
                    id_expr!("a", Ttype::tvar("a")),
                    id_expr!("b", Ttype::tvar("b")),
                    Ttype::tvar("+")
                ))],
                vec![Constraint {
                    lhs: Ttype::tvar("+"),
                    rhs: Ttype::MaxNumeric(Box::new(Ttype::tvar("a")), Box::new(Ttype::tvar("b"))),
                }],
            ),
        ];

        for (program, expect) in tests {
            let mut cons = vec![];
            let mut env = Env::new();
            env.push();
            for mut stmt in program {
                generate_constraints(&mut stmt, &mut cons, &mut env);
            }
            println!("constraints {:?} {:?}", cons, env);
            env.pop();
            assert_eq_unordered::<Constraint>(expect, cons)
        }
    }
    #[test]
    fn test_call() {
        let mut cons = vec![];
        let mut env = Env::new();
        env.push();
        env.bind_symbol(
            "f".into(),
            SymbolValue::Function(Ttype::Fn(vec![
                Ttype::tvar("fn_arg_0"),
                Ttype::tvar("fn_ret"),
            ])),
        );
        generate_constraints(
            &mut Ast::Expr(call_expr!(
                id_expr!("f", Ttype::tvar("fn_ref")),
                vec![int_expr!(1)],
                Ttype::tvar("call_expr")
            )),
            &mut cons,
            &mut env,
        );
        println!("constraints {:?} {:?}", cons, env);
        env.pop();
        assert_eq_unordered::<Constraint>(
            vec![
                Constraint {
                    lhs: Ttype::tvar("call_expr"),
                    rhs: Ttype::tvar("fn_ret"),
                },
                Constraint {
                    lhs: Ttype::tvar("fn_ref"),
                    rhs: Ttype::Fn(vec![Ttype::tvar("fn_arg_0"), Ttype::tvar("fn_ret")]),
                },
            ],
            cons,
        )
    }

    #[test]
    fn test_call_arg_constraints() {
        let mut cons = vec![];
        let mut env = Env::new();
        env.push();
        env.bind_symbol(
            "f".into(),
            SymbolValue::Function(Ttype::Fn(vec![
                Ttype::tvar("fn_arg_0"),
                Ttype::tvar("fn_arg_1"),
                Ttype::tvar("fn_ret"),
            ])),
        );
        generate_constraints(
            &mut Ast::Expr(call_expr!(
                id_expr!("f", Ttype::tvar("fn_ref")),
                vec![
                    id_expr!("a", Ttype::tvar("a_ref")),
                    id_expr!("b", Ttype::tvar("b_ref")),
                ],
                Ttype::tvar("call_expr")
            )),
            &mut cons,
            &mut env,
        );
        env.pop();
        assert_eq_unordered::<Constraint>(
            vec![
                Constraint {
                    lhs: Ttype::tvar("call_expr"),
                    rhs: Ttype::tvar("fn_ret"),
                },
                Constraint {
                    lhs: Ttype::tvar("a_ref"),
                    rhs: Ttype::tvar("fn_arg_0"),
                },
                Constraint {
                    lhs: Ttype::tvar("b_ref"),
                    rhs: Ttype::tvar("fn_arg_1"),
                },
                Constraint {
                    lhs: Ttype::tvar("fn_ref"),
                    rhs: Ttype::Fn(vec![
                        Ttype::tvar("fn_arg_0"),
                        Ttype::tvar("fn_arg_1"),
                        Ttype::tvar("fn_ret"),
                    ]),
                },
            ],
            cons,
        )
    }
}

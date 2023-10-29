use std::{mem, ops::Deref};

use crate::{
    parser::Identifier,
    symbols::{Env, SymbolValue},
};
#[macro_use]
use crate::{
    lexer::Token,
    parser::{Ast, Program},
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
    let mut constraint_generator = ConstraintGenerator::new();
    for mut e in expr {
        constraint_generator.generate_constraints(&mut e);
    }

    constraint_generator.env.pop();
    println!("Constraints:");
    for c in &constraint_generator.constraints {
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

struct ConstraintGenerator {
    pub constraints: Vec<Constraint>,
    pub env: Env,
}
impl ConstraintGenerator {
    pub fn new() -> Self {
        let mut cgen = Self {
            constraints: vec![],
            env: Env::new(),
        };
        cgen.env.push();
        cgen
    }
    fn body(&mut self, body: &mut Vec<Ast>, final_ttype: Option<Ttype>) -> Option<Ttype> {
        if body.len() == 0 {
            return None;
        }

        let mut fin = body[0].clone();
        for s in body {
            self.generate_constraints(s);
            fin = s.clone();
        }

        match (final_ttype, fin.get_ttype()) {
            (Some(l), Some(r)) => {
                push_constraint(l, r, &mut self.constraints);
            }
            _ => {}
        };

        fin.get_ttype()
    }
    fn fn_declaration(&mut self, id: Identifier, fn_expr: &mut Ast) {
        fn_expr.set_ttype(tvar());
        match fn_expr {
            Ast::Fn(args_vec, _ret_type_ast, ref mut stmts, ttype) => {
                self.env.push();
                let mut fn_types = vec![];
                for mut arg in args_vec {
                    self.generate_constraints(&mut arg);
                    fn_types.push(arg.get_ttype().unwrap());
                    match arg {
                        Ast::Id(arg_id, arg_type) => {
                            self.env.bind_symbol(
                                arg_id.clone(),
                                SymbolValue::Variable(arg_type.clone()),
                            );
                        }
                        _ => {}
                    }
                }
                self.env
                    .bind_symbol(id.clone(), SymbolValue::Function(ttype.clone()));

                let final_stmt_type = self.body(
                    stmts, None, // TODO: ret_type
                );

                fn_types.push(final_stmt_type.unwrap_or(tvar()));
                self.env.pop();

                let fn_type = Ttype::Fn(fn_types);
                self.env
                    .bind_symbol(id, SymbolValue::Function(fn_type.clone()));
                self.push_constraint(ttype.clone(), fn_type);
            }
            _ => {}
        }
    }

    fn id(&mut self, id: Identifier, ttype: Ttype) {
        // lookup id in env and constrain ttype to the lookup's ttype
        match self.env.lookup(id.to_string()) {
            Some(SymbolValue::Variable(t)) | Some(SymbolValue::Function(t)) => {
                self.push_constraint(ttype, t.clone());
            }
            _ => {}
        }
    }
    fn binop(&mut self, token: Token, left: &mut Ast, right: &mut Ast, ttype: Ttype) {
        self.generate_constraints(left);
        self.generate_constraints(right);
        println!("binop operands {:?} {:?}", left, right);
        match token {
            Token::Plus | Token::Minus | Token::Star | Token::Slash | Token::Modulo => {
                // numeric binop
                if let Some(num_binop_type) =
                    max_numeric_type(left.get_ttype().unwrap(), right.get_ttype().unwrap())
                {
                    self.push_constraint(ttype, num_binop_type);
                }
            }
            Token::Equality | Token::NotEqual | Token::Lt | Token::Lte | Token::Gt | Token::Gte => {
                self.push_constraint(ttype, Ttype::Bool);
            }
            _ => {}
        }
    }
    fn unop(&mut self, token: Token, operand: &mut Ast, ttype: Ttype) {
        self.generate_constraints(operand);
        match token {
            Token::Minus => {
                self.push_constraint(ttype, operand.get_ttype().unwrap());
            }
            Token::Bang => {
                self.push_constraint(ttype, Ttype::Bool);
            }
            _ => {}
        }
    }

    fn tuple(&mut self, exprs: &mut Vec<Ast>, ttype: Ttype) {
        let mut ttv = vec![];
        for mut e in exprs {
            self.generate_constraints(&mut e);
            ttv.push(e.get_ttype().unwrap());
        }
        self.push_constraint(ttype, Ttype::Tuple(ttv));
    }
    fn index(&mut self, object: &mut Ast, idx: &mut Ast, ttype: Ttype) {
        self.generate_constraints(object);
        //     // TODO: create Ttype::Array(last type of obj array types)
        //     // TODO: push_constraint obj.ttype == Ttype::Array['t]
        //
        self.generate_constraints(idx);
        self.push_constraint(idx.get_ttype().unwrap(), Ttype::Numeric(Numeric::Int));
        // TODO:
        // if obj has ttype Array<'t>, then ast node ttype == 't
    }
    fn assignment(&mut self, assignee: &mut Ast, value: &mut Ast, ttype: Ttype) {
        self.generate_constraints(assignee);
        self.generate_constraints(value);
        self.push_constraint(ttype.clone(), assignee.get_ttype().unwrap());
        self.push_constraint(ttype.clone(), value.get_ttype().unwrap());
    }
    fn if_then_expr(
        &mut self,
        condition: &mut Ast,
        then: &mut Vec<Ast>,
        // elze: &mut Option<Vec<Ast>>,
        ttype: Ttype,
    ) {
        self.generate_constraints(condition);

        if let Some(b_type) = condition.get_ttype() {
            self.push_constraint(b_type, Ttype::Bool);
        }
        self.body(then, Some(ttype.clone()));
    }

    fn call(&mut self, callee: &mut Ast, params: &mut Vec<Ast>, ttype: Ttype) {
        self.generate_constraints(callee);
        let env = &self.env;

        let fn_types = match callee {
            Ast::Id(id, callee_ttype) => match env.lookup(id.clone()) {
                Some(SymbolValue::Function(Ttype::Fn(fn_types_vec))) => fn_types_vec.clone(),
                _ => {
                    // callee not found in env
                    return;
                }
            },
            _ => {
                // TODO: handle inline fn expr + call
                return;
            }
        };

        self.push_constraint(ttype.clone(), fn_types.last().unwrap().clone());

        if params.len() < fn_types.len() - 1 {
            // TODO: typecheck curried fn
        }

        for (idx, mut param) in params.iter_mut().enumerate() {
            self.generate_constraints(&mut param);
            match param.get_ttype() {
                Some(param_type) => {
                    self.push_constraint(param_type.clone(), fn_types[idx].clone());
                }
                _ => {}
            }
        }
    }

    pub fn generate_constraints(&mut self, ast: &mut Ast) {
        ast.set_ttype(tvar());

        match ast {
            Ast::Let(id, type_expr, value) => {}
            Ast::FnDeclaration(id, fn_expr) => self.fn_declaration(id.clone(), &mut *fn_expr),
            Ast::TypeDeclaration(id, type_expr) => {}
            Ast::Id(id, ttype) => {
                self.id(id.clone(), ttype.clone());
            }
            Ast::Binop(token, left_box, right_box, ttype) => {
                self.binop(
                    token.clone(),
                    &mut *left_box,
                    &mut *right_box,
                    ttype.clone(),
                );
            }
            Ast::Unop(token, operand_box, ttype) => {
                self.unop(token.clone(), &mut *operand_box, ttype.clone());
            }
            //
            Ast::Tuple(exprs_vec, ttype) => self.tuple(exprs_vec, ttype.clone()),
            //
            Ast::Index(object_box, index_box, ttype) => {
                self.index(&mut *object_box, &mut *index_box, ttype.clone());
            }

            Ast::Assignment(assignee_box, value_box, ttype) => {
                self.assignment(&mut *assignee_box, &mut *value_box, ttype.clone());
            }
            //
            // Ast::Fn(mut args_vec, ret_type, mut stmts, ttype) => {
            //     env.push();
            //     let mut fn_types = vec![];
            //     for mut arg in args_vec {
            //         generate_constraints(&mut arg, constraints, env);
            //         fn_types.push(arg.get_ttype().unwrap());
            //     }
            //
            //     let mut final_stmt_type = stmts[0].get_ttype().unwrap();
            //     for mut stmt in stmts {
            //         generate_constraints(&mut stmt, constraints, env);
            //         final_stmt_type = stmt.get_ttype().unwrap();
            //     }
            //     fn_types.push(final_stmt_type);
            //     env.pop();
            // }
            // // Expr::Body(stmts, ttype) => {} -- Expr::Body == Vec<Ast>
            Ast::If(condition, then, elze, ttype) => {
                self.if_then_expr(&mut *condition, then, ttype.clone());
                if let Some(e) = elze {
                    self.body(e, Some(ttype.clone()));
                }
            }
            Ast::Call(callee_box, params_vec, ttype) => {
                self.call(&mut *callee_box, params_vec, ttype.clone());
            }
            _ => (),
        }
        // let _ = mem::replace(ast_o, ast);

        println!("ast with ttype: {:?}", ast);
    }

    fn push_constraint(&mut self, left: Ttype, right: Ttype) {
        match &left {
            Ttype::Var(_) => {
                self.constraints.push(Constraint {
                    lhs: left,
                    rhs: right,
                });
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {

    use std::collections::HashSet;

    use super::*;
    use crate::{binop_expr, bool_expr, call_expr, id_expr, if_expr, int_expr, tuple_expr};
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
        let mut program = vec![if_expr!(
            bool_expr!(true),
            vec![tuple_expr!(
                vec![Ast::Integer(1), Ast::Integer(2)],
                t!("tuple1")
            )],
            Some(vec![tuple_expr!(
                vec![int_expr!(3), int_expr!(4)],
                t!("tuple2")
            )]),
            t!("if_expr_type")
        )];
        let mut constraint_generator = ConstraintGenerator::new();
        constraint_generator.generate_constraints(&mut program[0]);

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
        assert_eq_unordered::<Constraint>(ex, constraint_generator.constraints)
    }

    #[test]
    fn test_simple_constraints() {
        let tests: Vec<(Vec<Ast>, Vec<Constraint>)> = vec![
            (
                vec![Ast::FnDeclaration(
                    "f".into(),
                    Box::new(Ast::Fn(
                        vec![
                            id_expr!("a", Ttype::tvar("arg_a")),
                            id_expr!("b", Ttype::tvar("arg_b")),
                        ],
                        None,
                        vec![binop_expr!(
                            Token::Plus,
                            id_expr!("a", Ttype::tvar("arg_a_ref")),
                            id_expr!("b", Ttype::tvar("arg_b_ref")),
                            Ttype::tvar("fn_return")
                        )],
                        Ttype::tvar("fn_type"),
                    )),
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
                vec![binop_expr!(
                    Token::Plus,
                    int_expr!(1),
                    int_expr!(2),
                    Ttype::tvar("+")
                )],
                vec![Constraint {
                    lhs: Ttype::tvar("+"),
                    rhs: Ttype::Numeric(Numeric::Int),
                }],
            ),
            // Binop generic
            (
                vec![binop_expr!(
                    Token::Plus,
                    id_expr!("a", Ttype::tvar("a")),
                    id_expr!("b", Ttype::tvar("b")),
                    Ttype::tvar("+")
                )],
                vec![Constraint {
                    lhs: Ttype::tvar("+"),
                    rhs: Ttype::MaxNumeric(Box::new(Ttype::tvar("a")), Box::new(Ttype::tvar("b"))),
                }],
            ),
            // recursive func
            (
                vec![Ast::FnDeclaration(
                    "f".into(),
                    Box::new(Ast::Fn(
                        vec![
                            id_expr!("a", Ttype::tvar("arg_a")),
                            id_expr!("b", Ttype::tvar("arg_b")),
                        ],
                        None,
                        vec![
                            binop_expr!(
                                Token::Plus,
                                id_expr!("a", Ttype::tvar("arg_a_ref")),
                                id_expr!("b", Ttype::tvar("arg_b_ref")),
                                Ttype::tvar("tmp_binop")
                            ),
                            call_expr!(
                                id_expr!("f", Ttype::tvar("fn_ref")),
                                vec![int_expr!(1), int_expr!(2)],
                                Ttype::tvar("rec_call_expr")
                            ),
                        ],
                        Ttype::tvar("fn_type"),
                    )),
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
                        lhs: Ttype::tvar("tmp_binop"),
                        rhs: Ttype::MaxNumeric(
                            Box::new(Ttype::tvar("arg_a_ref")),
                            Box::new(Ttype::tvar("arg_b_ref")),
                        ),
                    },
                    Constraint {
                        lhs: Ttype::tvar("fn_ref"),
                        rhs: Ttype::tvar("fn_type"),
                    },
                    Constraint {
                        lhs: Ttype::tvar("fn_type"),
                        rhs: Ttype::Fn(vec![
                            Ttype::tvar("arg_a"),
                            Ttype::tvar("arg_b"),
                            Ttype::tvar("rec_call_expr"),
                        ]),
                    },
                ],
            ),
        ];

        for (program, expect) in tests {
            let mut cg = ConstraintGenerator::new();
            for mut stmt in program {
                cg.generate_constraints(&mut stmt);
            }
            println!("constraints {:?} {:?}", cg.constraints, cg.env);
            assert_eq_unordered::<Constraint>(expect, cg.constraints)
        }
    }
    #[test]
    fn test_call() {
        let mut cg = ConstraintGenerator::new();
        cg.env.bind_symbol(
            "f".into(),
            SymbolValue::Function(Ttype::Fn(vec![
                Ttype::tvar("fn_arg_0"),
                Ttype::tvar("fn_ret"),
            ])),
        );
        cg.generate_constraints(&mut call_expr!(
            id_expr!("f", Ttype::tvar("fn_ref")),
            vec![int_expr!(1)],
            Ttype::tvar("call_expr")
        ));
        println!("constraints {:?} {:?}", cg.constraints, cg.env);
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
            cg.constraints,
        )
    }

    #[test]
    fn test_call_arg_constraints() {
        let mut cg = ConstraintGenerator::new();
        cg.env.bind_symbol(
            "f".into(),
            SymbolValue::Function(Ttype::Fn(vec![
                Ttype::tvar("fn_arg_0"),
                Ttype::tvar("fn_arg_1"),
                Ttype::tvar("fn_ret"),
            ])),
        );
        cg.generate_constraints(&mut call_expr!(
            id_expr!("f", Ttype::tvar("fn_ref")),
            vec![
                id_expr!("a", Ttype::tvar("a_ref")),
                id_expr!("b", Ttype::tvar("b_ref")),
            ],
            Ttype::tvar("call_expr")
        ));
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
            cg.constraints,
        )
    }
}

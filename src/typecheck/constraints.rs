use crate::{
    lexer::Token,
    parser::{Ast, Identifier},
    symbols::{max_numeric_type, tint, transform_generic_vec, Env, Environment, Symbol, Ttype},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constraint {
    pub lhs: Ttype,
    pub rhs: Ttype,
}
pub struct ConstraintGenerator {
    pub constraints: Vec<Constraint>,
    pub env: Env<Symbol>,
}

impl ConstraintGenerator {
    pub fn new() -> Self {
        let mut env = Env::<Symbol>::new();
        env.push();
        Self {
            constraints: vec![],
            env,
        }
    }

    fn push_constraint(&mut self, left: Ttype, right: Ttype) {
        if left != right {
            self.constraints.push(Constraint {
                lhs: left,
                rhs: right,
            });
        }
    }
    fn fn_proto(&mut self, params_vec: &Vec<Ast>) -> Vec<Ttype> {
        let mut fn_types: Vec<Ttype> = vec![];
        for p in params_vec {
            self.generate_constraints(p);
            fn_types.push(p.ttype());

            if let Ast::Id(p_id, p_type) = p {
                self.env
                    .bind_symbol(p_id.clone(), Symbol::Variable(p_type.clone()))
            }
        }

        fn_types
    }

    fn body(&mut self, body: &Vec<Ast>) -> Ttype {
        let mut fin = body[0].clone();
        for s in body {
            self.generate_constraints(s);
            fin = s.clone();
        }

        // if let (Some(l), r) = (expected_return, fin.ttype()) {
        //     self.push_constraint(l, r);
        // };

        fin.ttype()
    }
    fn fn_declaration(&mut self, id: &String, fn_expr: &Ast) {
        self.env
            .bind_symbol(id.clone(), Symbol::Function(fn_expr.ttype())); // bind symbol as early as
                                                                         // possible - in the stack
                                                                         // of the function this type will be available as a recursive reference
        self.env.push();

        match fn_expr {
            Ast::Fn(params_vec, body, ttype) => {
                let mut fn_types_vec = self.fn_proto(params_vec);
                let ret_type = self.body(body);
                fn_types_vec.push(ret_type);

                self.push_constraint(ttype.clone(), Ttype::Fn(fn_types_vec));
            }
            _ => {
                panic!("no fn after declaration!")
            }
        };
        self.env.pop();
    }
    fn id(&mut self, id: &Identifier, ttype: &Ttype) {
        if let Some(Symbol::Variable(t)) | Some(Symbol::Function(t)) =
            self.env.lookup(id.to_string())
        {
            self.push_constraint(ttype.clone(), t.clone());
        }
    }
    fn get_function_type(&self, name: Identifier) -> Option<&Ttype> {
        match self.env.lookup(name) {
            Some(Symbol::Function(fn_type)) => Some(fn_type),
            _ => None,
        }
    }
    fn call(&mut self, callable: &Ast, call_args: &Vec<Ast>, ttype: Ttype) {
        // self.generate_constraints(callable);

        for p in call_args {
            self.generate_constraints(p);
        }

        let fn_types = match callable {
            Ast::Id(callable, callable_type) => match self.get_function_type(callable.clone()) {
                Some(fn_type) if fn_type.is_generic() => {
                    if let Ttype::Fn(tvec) =
                        fn_type.transform_generic(call_args.iter().map(|x| x.ttype()).collect())
                    {
                        self.push_constraint(callable_type.clone(), Ttype::Fn(tvec.clone()));
                        tvec
                    } else {
                        self.push_constraint(callable_type.clone(), fn_type.clone());
                        return;
                    }
                }
                Some(Ttype::Fn(fn_types)) => {
                    // self.push_constraint(callable_type.clone(), Ttype::Fn(fn_types.clone()));
                    fn_types.clone()
                }

                _ => {
                    return;
                }
            },
            _ => {
                return;
            }
        };

        if call_args.len() < fn_types.len() - 1 {
            // typecheck curried fn
            let curried_fn_components = &fn_types[call_args.len()..];
            self.push_constraint(ttype.clone(), Ttype::Fn(curried_fn_components.into()));
            return;
        };
        // fn_types;
        // if let Ast::Id(callable, callable_type) = callable {
        // let mut cons = &self.constraints;
        // cons.push(Constraint {
        //     lhs: callable_type.clone(),
        //     rhs: Ttype::Fn(fn_types.clone()),
        // });
        // self.push_constraint(callable_type.clone(), Ttype::Fn(fn_types.clone()));
        // };

        self.push_constraint(ttype.clone(), fn_types.last().unwrap().clone());
    }

    pub fn generate_constraints(&mut self, ast: &Ast) {
        match ast {
            Ast::Let(
                id,
                None,        // optional explicit type parameter
                Some(value), // optional immediate assignment expression
            ) => {
                self.generate_constraints(value);
                self.env
                    .bind_symbol(id.to_string(), Symbol::Variable((*value).ttype()))
            }

            Ast::Let(
                id,
                Some(t),     // optional explicit type parameter
                Some(value), // optional immediate assignment expression
            ) => {
                self.generate_constraints(value);
                self.env
                    .bind_symbol(id.to_string(), Symbol::Variable((*value).ttype()));
                self.push_constraint(t.clone(), value.ttype());
            }

            Ast::Let(
                id,
                None, // optional explicit type parameter
                None, // optional immediate assignment expression
            ) => {}

            Ast::FnDeclaration(id, fn_expr) => self.fn_declaration(id, &**fn_expr),
            Ast::TypeDeclaration(id, type_expr) => {}
            Ast::Id(id, ttype) => {
                self.id(id, ttype);
            }
            Ast::Binop(token, left, right, ttype) => {
                self.generate_constraints(left);
                self.generate_constraints(right);

                match token {
                    Token::Plus | Token::Minus | Token::Star | Token::Slash | Token::Modulo => {
                        // numeric binop
                        let num_binop_type = max_numeric_type(left.ttype(), right.ttype());
                        self.push_constraint(ttype.clone(), num_binop_type);
                    }
                    Token::Equality
                    | Token::NotEqual
                    | Token::Lt
                    | Token::Lte
                    | Token::Gt
                    | Token::Gte => {
                        self.push_constraint(ttype.clone(), Ttype::Bool);
                        self.push_constraint(left.ttype(), right.ttype());
                    }
                    _ => {}
                }
            }
            Ast::Unop(token, operand, ttype) => {
                self.generate_constraints(operand);
                match token {
                    Token::Minus => {
                        self.push_constraint(ttype.clone(), operand.ttype());
                    }
                    Token::Bang => self.push_constraint(ttype.clone(), Ttype::Bool),
                    _ => {}
                }
            }
            Ast::Tuple(exprs, ttype) => {}
            Ast::Index(obj, idx, ttype) => {
                self.generate_constraints(obj);
                self.generate_constraints(idx);
                self.push_constraint(idx.ttype(), tint());
                // TODO:
                // if obj has ttype Array<'t>, then ttype == 't
            }
            Ast::Assignment(assignee, val, ttype) => {
                self.generate_constraints(assignee);
                self.generate_constraints(val);
                self.push_constraint(ttype.clone(), assignee.ttype());
                self.push_constraint(ttype.clone(), val.ttype());
            }
            // Ast::Fn(params, body, ttype) => {}
            // Ast::Body(stmts, ttype) => {}
            Ast::If(cond, then, elze, ttype) => {
                self.generate_constraints(cond);
                self.push_constraint(cond.ttype(), Ttype::Bool);

                let then_result_type = self.body(then);
                self.push_constraint(then_result_type, ttype.clone());

                if let Some(else_body) = elze {
                    let else_result_type = self.body(else_body);
                    self.push_constraint(else_result_type, ttype.clone());
                }
            }
            Ast::Call(callable, args, ttype) => self.call(callable, args, ttype.clone()),
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::{
        binop_expr, bool_expr, call_expr, id_expr, if_expr, int_expr, symbols::tvar, tuple_expr,
    };

    use super::*;

    fn assert_eq_unordered<T: PartialEq + std::hash::Hash + Eq + std::fmt::Debug>(
        expected: Vec<T>,
        actual: Vec<T>,
    ) {
        let expected_set: HashSet<_> = expected.iter().collect();
        let actual_set: HashSet<_> = actual.iter().collect();

        assert_eq!(expected_set, actual_set);
    }

    fn cons(l: Ttype, r: Ttype) -> Constraint {
        Constraint { lhs: l, rhs: r }
    }
    #[test]
    fn constraints() {
        let program = vec![if_expr!(
            bool_expr!(true),
            vec![tuple_expr!(
                vec![int_expr!(1), int_expr!(2)],
                tvar("tuple1")
            )],
            Some(vec![tuple_expr!(
                vec![int_expr!(3), int_expr!(4)],
                tvar("tuple2")
            )]),
            tvar("if_expr_type")
        )];
        let mut cg = ConstraintGenerator::new();
        cg.generate_constraints(&program[0]);
        println!("{:?}", cg.constraints);
        assert_eq_unordered::<Constraint>(
            vec![
                cons(tvar("tuple2"), tvar("if_expr_type")),
                cons(tvar("tuple1"), tvar("if_expr_type")),
            ],
            cg.constraints,
        );
    }

    #[test]
    fn simple_constraints() {
        let tests: Vec<(Vec<Ast>, Vec<Constraint>)> = vec![
            (
                vec![Ast::FnDeclaration(
                    "f".into(),
                    Box::new(Ast::Fn(
                        vec![id_expr!("a", tvar("arg_a")), id_expr!("b", tvar("arg_b"))],
                        vec![binop_expr!(
                            Token::Plus,
                            id_expr!("a", tvar("arg_a_ref")),
                            id_expr!("b", tvar("arg_b_ref")),
                            tvar("fn_return")
                        )],
                        tvar("fn_type"),
                    )),
                )],
                vec![
                    cons(tvar("arg_a_ref"), tvar("arg_a")),
                    cons(tvar("arg_b_ref"), tvar("arg_b")),
                    cons(
                        tvar("fn_return"),
                        Ttype::MaxNumeric(vec![tvar("arg_a_ref"), tvar("arg_b_ref")]),
                    ),
                    cons(
                        tvar("fn_type"),
                        Ttype::Fn(vec![tvar("arg_a"), tvar("arg_b"), tvar("fn_return")]),
                    ),
                ],
            ),
            // Binop
            (
                vec![binop_expr!(
                    Token::Plus,
                    int_expr!(1),
                    int_expr!(2),
                    tvar("+")
                )],
                vec![cons(tvar("+"), tint())],
            ),
            // Binop generic
            (
                vec![binop_expr!(
                    Token::Plus,
                    id_expr!("a", tvar("a")),
                    id_expr!("b", tvar("b")),
                    tvar("+")
                )],
                vec![cons(
                    tvar("+"),
                    Ttype::MaxNumeric(vec![tvar("a"), tvar("b")]),
                )],
            ),
            // recursive func
            (
                vec![Ast::FnDeclaration(
                    "f".into(),
                    Box::new(Ast::Fn(
                        vec![id_expr!("a", tvar("arg_a")), id_expr!("b", tvar("arg_b"))],
                        vec![
                            binop_expr!(
                                Token::Plus,
                                id_expr!("a", tvar("arg_a_ref")),
                                id_expr!("b", tvar("arg_b_ref")),
                                tvar("tmp_binop")
                            ),
                            call_expr!(
                                id_expr!("f", tvar("fn_ref")),
                                vec![int_expr!(1), int_expr!(2)],
                                tvar("rec_call_expr")
                            ),
                        ],
                        tvar("fn_type"),
                    )),
                )],
                vec![
                    cons(tvar("arg_a_ref"), tvar("arg_a")),
                    cons(tvar("arg_b_ref"), tvar("arg_b")),
                    cons(
                        tvar("tmp_binop"),
                        Ttype::MaxNumeric(vec![tvar("arg_a_ref"), tvar("arg_b_ref")]),
                    ),
                    cons(tvar("fn_ref"), tvar("fn_type")),
                    cons(
                        tvar("fn_type"),
                        Ttype::Fn(vec![tvar("arg_a"), tvar("arg_b"), tvar("rec_call_expr")]),
                    ),
                ],
            ),
        ];
        for (prog, expect) in tests {
            let mut cg = ConstraintGenerator::new();
            for stmt in prog {
                cg.generate_constraints(&stmt);
            }
            assert_eq_unordered::<Constraint>(expect, cg.constraints);
        }
    }
    #[test]
    fn call() {
        let mut cg = ConstraintGenerator::new();
        let fn_type = Ttype::Fn(vec![tvar("fn_arg_0"), tvar("fn_ret")]);
        cg.env
            .bind_symbol("f".into(), Symbol::Function(fn_type.clone()));

        cg.generate_constraints(&call_expr!(
            id_expr!("f", tvar("fn_ref")),
            vec![int_expr!(1)],
            tvar("call_expr")
        ));

        let mut cons_set: HashSet<Constraint> = HashSet::new();
        for c in cg.constraints {
            cons_set.insert(c);
        }
        assert!(cons_set.contains(&cons(tvar("call_expr"), tvar("fn_ret"))))
    }

    #[test]
    fn call_generic() {
        let mut cg = ConstraintGenerator::new();
        let fn_type = Ttype::Fn(vec![tvar("fn_arg_0"), tvar("fn_arg_0")]);
        cg.env
            .bind_symbol("f".into(), Symbol::Function(fn_type.clone()));

        cg.generate_constraints(&call_expr!(
            id_expr!("f", tvar("fn_ref")),
            vec![int_expr!(1)],
            tvar("call_expr")
        ));

        // println!("constraints {:?} {:?}", cg.constraints, cg.env);
        // panic!();
        assert_eq_unordered::<Constraint>(
            vec![
                cons(tvar("fn_ref"), Ttype::Fn(vec![tint(), tint()])),
                cons(tvar("call_expr"), tint()),
            ],
            cg.constraints,
        );
    }
}

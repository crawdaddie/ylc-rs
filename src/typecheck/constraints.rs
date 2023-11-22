use crate::{
    lexer::Token,
    parser::{Ast, Identifier},
    symbols::{max_numeric_type, tint, Env, Environment, Symbol, Ttype},
};

pub type Constraint = (Ttype, Ttype);

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
            self.constraints.push((left, right));
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

    fn callable_identifier(&mut self, callable_id: &Identifier, callable_type: &Ttype) -> Ttype {
        if let Some(fn_type) = self.get_function_type(callable_id.clone()) {
            let f = fn_type.clone();
            self.push_constraint(callable_type.clone(), f.clone());
            f
        } else {
            panic!("Constraint generator: no function {callable_id} found")
        }
    }

    fn call(&mut self, callable: &Ast, call_args: &Vec<Ast>, ttype: Ttype) {
        for p in call_args {
            self.generate_constraints(p);
        }

        match callable {
            Ast::Id(callable, callable_type) => {
                if let Ttype::Fn(fn_types) = self.callable_identifier(callable, callable_type) {
                    let l = ttype.clone();
                    let r = fn_types.last().unwrap();
                    self.push_constraint(l, r.clone());
                }
            }
            _ => panic!(),
        };
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
                    Token::Equality | Token::NotEqual => {
                        // println!(" constraints {:?} {:?}", left, right);
                        self.push_constraint(ttype.clone(), Ttype::Bool);
                        self.push_constraint(left.ttype(), right.ttype());
                    }
                    Token::Lt | Token::Lte | Token::Gt | Token::Gte => {
                        // println!(" constraints {:?} {:?}", left, right);
                        self.push_constraint(ttype.clone(), Ttype::Bool);
                        // if left.ttype().is_var() {
                        //     self.push_constraint(left.ttype(), Ttype::MaxNumeric(vec![]));
                        // }
                        // if right.ttype().is_var() {
                        //     self.push_constraint(right.ttype(), Ttype::MaxNumeric(vec![]));
                        // }
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
                self.push_constraint(ttype.clone(), then_result_type);

                if let Some(else_body) = elze {
                    let else_result_type = self.body(else_body);
                    self.push_constraint(ttype.clone(), else_result_type);
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
        assert_eq_unordered::<Constraint>(
            vec![
                (tvar("if_expr_type"), tvar("tuple2")),
                (tvar("if_expr_type"), tvar("tuple1")),
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
                    (tvar("arg_a_ref"), tvar("arg_a")),
                    (tvar("arg_b_ref"), tvar("arg_b")),
                    (
                        tvar("fn_return"),
                        Ttype::MaxNumeric(vec![tvar("arg_a_ref"), tvar("arg_b_ref")]),
                    ),
                    (
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
                vec![(tvar("+"), tint())],
            ),
            // Binop generic
            (
                vec![binop_expr!(
                    Token::Plus,
                    id_expr!("a", tvar("a")),
                    id_expr!("b", tvar("b")),
                    tvar("+")
                )],
                vec![(tvar("+"), Ttype::MaxNumeric(vec![tvar("a"), tvar("b")]))],
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
                    (tvar("arg_a_ref"), tvar("arg_a")),
                    (tvar("arg_b_ref"), tvar("arg_b")),
                    (
                        tvar("tmp_binop"),
                        Ttype::MaxNumeric(vec![tvar("arg_a_ref"), tvar("arg_b_ref")]),
                    ),
                    (tvar("fn_ref"), tvar("fn_type")),
                    (
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

        let mut set: HashSet<Constraint> = HashSet::new();
        for c in cg.constraints {
            set.insert(c);
        }
        assert!(set.contains(&(tvar("call_expr"), tvar("fn_ret"))))
    }
}

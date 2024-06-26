use crate::{
    parser::{Ast, Program},
    symbols::{Env, Environment, Symbol, Ttype},
    typecheck::unify::{lookup_contained_types, Substitutions},
};

mod constraints;
mod unify;

use constraints::ConstraintGenerator;
use unify::unify_constraints;

fn substitute(ttype: &mut Ttype, subs: &Substitutions) {
    match ttype {
        Ttype::Var(_) => {
            if let Some(replace) = subs.get(ttype) {
                *ttype = replace.clone();
            }
        }
        Ttype::Fn(ts) => {
            for t in ts.iter_mut() {
                substitute(t, subs)
            }
        }
        Ttype::Tuple(ts) => {
            for t in ts.iter_mut() {
                substitute(t, subs)
            }
        }
        Ttype::MaxNumeric(ts) => {
            for t in ts.iter_mut() {
                substitute(t, subs)
            }
        }
        Ttype::List(t) => substitute(t, subs),
        Ttype::Nth(t, _i) => substitute(t, subs),
        _ => (),
    }
}

pub fn update_ast_types(ast: &mut Ast, subs: &Substitutions) {
    match ast {
        Ast::Let(_id, _type_expr, Some(value)) => update_ast_types(&mut *value, subs),
        Ast::FnDeclaration(_id, fn_expr) => {
            update_ast_types(&mut *fn_expr, subs);
        }
        Ast::TypeDeclaration(_id, _type_expr) => {}
        Ast::Id(_id, ttype) => {
            substitute(ttype, subs);
        }
        Ast::Fn(params_vec, body, ttype) => {
            substitute(ttype, subs);
            for p in params_vec {
                update_ast_types(p, subs);
            }
            for s in body {
                update_ast_types(s, subs);
            }
        }

        Ast::Binop(_token, left_box, right_box, ttype) => {
            substitute(ttype, subs);

            // self.binop(token.clone(), &*left_box, &*right_box, ttype.clone());
            update_ast_types(&mut *left_box, subs);
            update_ast_types(&mut *right_box, subs);
        }

        Ast::Unop(_token, operand_box, ttype) => {
            substitute(ttype, subs);
            update_ast_types(&mut *operand_box, subs);
        }

        Ast::Tuple(exprs_vec, ttype) => {
            substitute(ttype, subs);
            for x in exprs_vec {
                update_ast_types(x, subs);
            }
        }

        Ast::List(exprs_vec, ttype) => {
            substitute(ttype, subs);
            for x in exprs_vec {
                update_ast_types(x, subs);
            }
        }

        Ast::Index(object_box, index_box, ttype) => {
            substitute(ttype, subs);
            update_ast_types(&mut *object_box, subs);
            update_ast_types(&mut *index_box, subs);
        }

        Ast::Assignment(assignee_box, value_box, ttype) => {
            substitute(ttype, subs);
            update_ast_types(&mut *assignee_box, subs);
            update_ast_types(&mut *value_box, subs);
        }
        Ast::If(condition, then, elze, ttype) => {
            substitute(ttype, subs);
            update_ast_types(&mut *condition, subs);
            for t in then {
                update_ast_types(t, subs);
            }
            if let Some(e) = elze {
                for t in e {
                    update_ast_types(t, subs);
                }
            }
        }
        Ast::Call(ref mut callee_box, ref mut params_vec, _ttype) => {
            update_ast_types(&mut *callee_box, subs);
            for a in &mut *params_vec {
                update_ast_types(a, subs);
            }
        }
        _ => (),
    }
}

pub fn apply_substitution(t: &mut Ttype, subs: &Substitutions) {
    *t = lookup_contained_types(t.clone(), subs);
}
pub fn update_types(ast: &mut Ast, subs: &Substitutions, env: &mut Env<Symbol>) {
    match ast {
        Ast::Let(_id, _type_expr, Some(value)) => update_types(&mut *value, subs, env),
        Ast::FnDeclaration(id, fn_expr) => {
            env.bind_symbol(id.clone(), Symbol::Variable(Ttype::FnRecRef));
            update_types(&mut *fn_expr, subs, env);
            env.bind_symbol(id.clone(), Symbol::Variable(fn_expr.ttype()));
        }
        Ast::TypeDeclaration(_id, _type_expr) => {}
        Ast::Id(_id, ttype) => {
            apply_substitution(ttype, subs);
        }
        Ast::Fn(params_vec, body, ttype) => {
            apply_substitution(ttype, subs);
            for p in params_vec {
                update_types(p, subs, env);
                if let Ast::Id(id, ptype) = p {
                    env.bind_symbol(id.clone(), Symbol::Variable(ptype.clone()))
                }
            }
            for s in body {
                update_types(s, subs, env);
            }
        }

        Ast::Binop(_token, left_box, right_box, ttype) => {
            apply_substitution(ttype, subs);

            // self.binop(token.clone(), &*left_box, &*right_box, ttype.clone());
            update_types(&mut *left_box, subs, env);
            update_types(&mut *right_box, subs, env);
        }

        Ast::Unop(_token, operand_box, ttype) => {
            apply_substitution(ttype, subs);
            update_types(&mut *operand_box, subs, env);
        }

        Ast::Tuple(exprs_vec, ttype) => {
            apply_substitution(ttype, subs);
            for x in exprs_vec {
                update_types(x, subs, env);
            }
        }

        Ast::List(exprs_vec, ttype) => {
            apply_substitution(ttype, subs);
            for x in exprs_vec {
                update_types(x, subs, env);
            }
        }

        Ast::Index(object_box, index_box, ttype) => {
            apply_substitution(ttype, subs);
            update_types(&mut *object_box, subs, env);
            update_types(&mut *index_box, subs, env);
        }

        Ast::Assignment(assignee_box, value_box, ttype) => {
            apply_substitution(ttype, subs);
            update_types(&mut *assignee_box, subs, env);
            update_types(&mut *value_box, subs, env);
        }
        Ast::If(condition, then, elze, ttype) => {
            apply_substitution(ttype, subs);
            update_types(&mut *condition, subs, env);
            for t in then {
                update_types(t, subs, env);
            }
            if let Some(e) = elze {
                for t in e {
                    update_types(t, subs, env);
                }
            }
        }
        Ast::Call(ref mut callee_box, ref mut params_vec, ttype) => {
            update_types(&mut *callee_box, subs, env);
            for a in &mut *params_vec {
                update_types(a, subs, env);
            }

            if let Ast::Id(ref mut fn_name, ref mut fn_ref_type) = **callee_box {
                let fn_type = match env.lookup(fn_name.clone()) {
                    Some(Symbol::Variable(fn_type)) if fn_type.is_generic() => {
                        let arg_types = params_vec.iter().map(|x| x.ttype()).collect::<Vec<_>>();
                        let spec_fn_type = fn_type.clone().transform_generic(arg_types);

                        *fn_ref_type = spec_fn_type;
                        fn_ref_type
                    }
                    Some(Symbol::Variable(fn_type)) => fn_type,
                    _ => panic!("Typecheck: fn {fn_name} not found in scope"),
                };
                match fn_type {
                    Ttype::Fn(ts) if ts.len() == params_vec.len() + 1 => {
                        *ttype = ts.last().unwrap().clone();
                    }

                    Ttype::Fn(ts) => {
                        *ttype = Ttype::Fn(ts[2..].into());
                    }
                    _ => {}
                }
            } else {
                apply_substitution(ttype, subs);
            };
        }
        Ast::Match(var, arms, ttype) => {
            update_types(var, subs, env);

            for (pattern, expr) in arms {
                env.push();
                update_types(pattern, subs, env);
                for sub_expr in expr.iter_mut() {
                    update_types(sub_expr, subs, env);
                }
                env.pop();
            }
            apply_substitution(ttype, subs);
        }
        _ => (),
    }
}
pub fn infer_types(expr: &mut Program) {
    let mut cg = ConstraintGenerator::new();
    for e in expr.clone() {
        cg.generate_constraints(&e);
    }
    println!("constraints\n--------");
    for c in &cg.constraints {
        println!("{:?}", c);
    }

    let mut subs = Substitutions::new();
    unify_constraints(cg.constraints, &mut subs);
    println!("\x1b[1;31m");
    println!("substitutions\n----------");
    for (k, v) in subs.iter() {
        println!("{:?}:{:?}", k, v);
    }
    println!("\x1b[1;0m");

    let mut env = Env::<Symbol>::new();
    env.push();
    for e in expr {
        // println!("update types {:?}", e);
        update_types(e, &subs, &mut env);
    }
}
#[cfg(test)]
mod tests {
    use crate::{int_expr, lexer::Lexer, parser::Parser, symbols::tint};

    use super::*;

    #[test]
    fn curried_function() {
        let input = r#"
        let f = fn (a, b, c, d) {
            a + b + c + d
        }
        f(1, 2)
        "#;

        let mut parser = Parser::new(Lexer::new(input.into()));
        let mut program = parser.parse_program();
        infer_types(&mut program);

        if let Ast::Call(fn_id, args, ttype) = program[1].clone() {
            let mut fn_types = vec![];
            if let Ast::Id(_, fn_type) = *fn_id {
                if let Ttype::Fn(fn_types_vec) = fn_type {
                    fn_types = fn_types_vec;
                } else {
                    panic!()
                }
            };
            if fn_types.is_empty() {
                panic!()
            }
            assert!(fn_types.len() == 5);
            assert_eq!(ttype, Ttype::Fn(fn_types[2..].into()));
            assert_eq!(args, vec![int_expr!(1), int_expr!(2)]);
        }
    }
    #[test]
    fn if_function() {
        let input = r#"
        let f = fn (arg) {
          if arg <= 1 {
            arg + 2
          } else {
            arg + 7
          }
        }
        f(-1)
        "#;

        let mut parser = Parser::new(Lexer::new(input.into()));
        let mut program = parser.parse_program();
        infer_types(&mut program);

        if let Ast::Call(fn_id, _args, _ttype) = program[1].clone() {
            let mut fn_types = vec![];
            if let Ast::Id(_, fn_type) = *fn_id {
                if let Ttype::Fn(fn_types_vec) = fn_type {
                    fn_types = fn_types_vec;
                } else {
                    panic!()
                }
            };
            if fn_types.is_empty() {
                panic!()
            }

            println!("if func {:?}", fn_types);
            assert_eq!(fn_types, vec![tint(), tint()]);
            // assert_eq!(ttype, Ttype::Fn(fn_types[2..].into()));
            // assert_eq!(args, vec![int_expr!(1), int_expr!(2)]);
        }
    }

    #[test]
    fn recursive_function() {
        let input = r#"
        let fib = fn(n): int {
          if n < 2 {
            n
          } else {
            fib(n - 1) + fib(n - 2)
          }
        }

        fib(10)
        "#;

        let mut parser = Parser::new(Lexer::new(input.into()));
        let mut program = parser.parse_program();
        infer_types(&mut program);

        if let Ast::Call(fn_id, _args, _ttype) = program[1].clone() {
            let mut fn_types = vec![];
            if let Ast::Id(_, fn_type) = *fn_id {
                if let Ttype::Fn(fn_types_vec) = fn_type {
                    fn_types = fn_types_vec;
                } else {
                    panic!()
                }
            };
            if fn_types.is_empty() {
                panic!()
            }

            assert_eq!(fn_types, vec![tint(), tint()]);
        }
    }
}

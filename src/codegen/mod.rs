use crate::parser::{Ast, Program};
use crate::symbols::SymbolValue;
use crate::CodegenCtx;
use inkwell::execution_engine::{JitFunction, UnsafeFunctionPointer};
use inkwell::values::{AnyValue, BasicValue, FunctionValue};
use std::error::Error;
//
// fn codegen_literal<'ctx>(literal: Literal, ctx: &CodegenCtx<'ctx>) -> AnyValueEnum<'ctx> {
//     let context = ctx.context;
//     match literal {
//         Literal::Integer(i) => {
//             AnyValueEnum::IntValue(context.i64_type().const_int(i.try_into().unwrap(), false))
//         }
//         Literal::Number(n) => AnyValueEnum::FloatValue(context.f64_type().const_float(n)),
//         Literal::Bool(b) => {
//             AnyValueEnum::IntValue(context.bool_type().const_int(if b { 1 } else { 0 }, false))
//         }
//         Literal::String(s) => AnyValueEnum::ArrayValue(context.const_string(s.as_bytes(), false)),
//     }
// }
// fn codegen_binop<'ctx>(op: Token, left: Ast, right: Expression, ctx: &CodegenCtx<'ctx>) {
//     let vleft = codegen_expression(left, ctx).unwrap();
//     let vright = codegen_expression(right, ctx).unwrap();
//     let builder = ctx.builder;
//     match op {
//         Token::Plus => Some(builder.build_int_add(vleft, vright, "add_result")),
//         Token::Minus => Some(builder.build_int_sub(vleft, vright, "subtract_result")),
//         Token::Star => Some(builder.build_int_mul(vleft, vright, "subtract_result")),
//         Token::Slash => Some(builder.build_int_signed_div(vleft, vright, "divide_result")),
//         _ => None,
//     }
// }
// fn codegen_unop<'ctx>(op: Token, operand: Ast, ctx: &CodegenCtx<'ctx>) {}
// fn codegen_tuple<'ctx>(exprs: Vec<Ast>, ctx: &CodegenCtx<'ctx>) {}
// fn codegen_identifier<'ctx>(id: Identifier, ctx: &CodegenCtx<'ctx>) {
//     None
// }
//
// fn codegen_expression<'ctx>(
//     expr: Ast,
//     ctx: &CodegenCtx<'ctx>,
// ) -> Option<AnyValueEnum<'ctx>> {
//     match expr {
//         Ast::Id(id) => codegen_identifier(id, ctx),
//         Ast::Literal(lit) => Some(codegen_literal(lit, ctx)),
//         Ast::Binop(op_token, left, right) => codegen_binop(op_token, *left, *right, ctx),
//         Ast::Unop(op_token, operand) => codegen_unop(op_token, *operand, ctx),
//         Ast::Tuple(exprs) => codegen_tuple(exprs, ctx),
//         // Index(Box<Ast>, Box<Expression>),
//         // Assignment(Identifier, Box<Ast>),
//         // _ => {}
//     }
// }

// fn codegen_function_decl<'ctx>(id: Identifier, fn_expr: FnExpr, ctx: &mut CodegenCtx<'ctx>) {
//     // let function = ctx.module.add_function(
//     //     id.as_str(),
//     //     ctx.context.void_type().fn_type(&[], false),
//     //     None,
//     // );
//     //
//     // let basic_block = ctx.context.append_basic_block(function, "entry");
//     // ctx.builder.position_at_end(basic_block);
//     ctx.env.bind_symbol(id, SymbolValue::Function)
// }

// fn codegen_let<'ctx>(
//     id: Identifier,
//     type_expr: Option<Box<Ast>>,
//     expr: Option<Box<Ast>>,
//     ctx: &mut CodegenCtx<'ctx>,
// ) {
//     ctx.env.bind_symbol(id, SymbolValue::Variable)
// }

// fn codegen_type_decl<'ctx>(id: Identifier, expr: Box<Ast>, ctx: &mut CodegenCtx<'ctx>) {
//     ctx.env.bind_symbol(id, SymbolValue::TypeDecl)
// }
fn codegen<'ctx>(ast: Ast, ctx: &mut CodegenCtx<'ctx>) -> Option<Box<dyn AnyValue<'ctx>>> {
    match ast {
        Ast::Let(
            id,
            t,           // optional explicit type parameter
            Some(value), // optional immediate assignment expression
        ) => {
            ctx.env
                .bind_symbol(id, SymbolValue::Variable(value.get_ttype().unwrap()));
            None
        }

        Ast::FnDeclaration(id, fn_expr) => {
            let v = codegen(*fn_expr.clone(), ctx);
            ctx.env
                .bind_symbol(id, SymbolValue::Function((*fn_expr).get_ttype().unwrap()));
            v
        }

        Ast::TypeDeclaration(id, type_expr) => None,

        Ast::Id(id, ttype) => None,
        Ast::Binop(token, left, right, ttype) => {
            // let l = codegen(*left, ctx);
            // let r = codegen(*right, ctx);
            None
        }
        Ast::Unop(token, operand, ttype) => None,
        Ast::Tuple(exprs, ttype) => None,
        Ast::Index(obj, idx, ttype) => None,
        Ast::Assignment(assignee, val, ttype) => None,
        Ast::Fn(params, ret_type, body, ttype) => None,
        Ast::Call(callable, args, ttype) => None,
        Ast::Body(stmts, ttype) => None,
        Ast::If(cond, then, elze, ttype) => None,

        Ast::Int8(i) => None,
        Ast::Integer(i) => None,
        Ast::Number(f) => None,
        Ast::Bool(b) => None,
        Ast::String(s) => None,
        _ => None,
    }
}

pub fn codegen_program<'ctx>(
    program: Program,
    ctx: &mut CodegenCtx<'ctx>,
) -> Result<FunctionValue<'ctx>, Box<dyn Error>> {
    let main_fn =
        ctx.module
            .add_function("main", ctx.context.void_type().fn_type(&[], false), None);

    let basic_block = ctx.context.append_basic_block(main_fn, "entry");
    ctx.builder.position_at_end(basic_block);

    for stmt in program {
        codegen(stmt, ctx);
    }
    ctx.builder.build_return(None);
    Ok(main_fn)
}

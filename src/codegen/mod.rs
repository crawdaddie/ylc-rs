use inkwell::execution_engine::{JitFunction, UnsafeFunctionPointer};
use inkwell::values::FunctionValue;

use std::error::Error;

use crate::parser::types::{Expression, Identifier, Literal, Program, Statement};
use crate::symbols::SymbolValue;
use crate::CodegenCtx;

/// Convenience type alias for the `sum` function.
///

impl<'ctx> CodegenCtx<'ctx> {
    pub fn get_function<T>(&self, fn_name: &str) -> Option<JitFunction<'ctx, T>>
    where
        T: UnsafeFunctionPointer,
    {
        unsafe { self.execution_engine.get_function::<T>(fn_name).ok() }
    }
}

fn codegen_literal<'ctx>(literal: Literal, ctx: &CodegenCtx<'ctx>) {
    match literal {
        Literal::Integer(i) => {}
        Literal::Number(n) => {}
        Literal::Bool(b) => {}
        Literal::String(s) => {}
    }
}

fn codegen_expression<'ctx>(expr: Expression, ctx: &CodegenCtx<'ctx>) {
    match expr {
        Expression::Id(id) => {}
        Expression::Literal(lit) => codegen_literal(lit, ctx),
        Expression::Binop(op_token, left, right) => {}
        Expression::Unop(op_token, operand) => {}
        Expression::Tuple(exprs) => {}

        // Index(Box<Expression>, Box<Expression>),
        // Assignment(Identifier, Box<Expression>),
        _ => {}
    }
}

fn codegen_function_decl<'ctx>(id: Identifier, fn_expr: Expression, ctx: &mut CodegenCtx<'ctx>) {
    // let function = ctx.module.add_function(
    //     id.as_str(),
    //     ctx.context.void_type().fn_type(&[], false),
    //     None,
    // );
    //
    // let basic_block = ctx.context.append_basic_block(function, "entry");
    // ctx.builder.position_at_end(basic_block);
    ctx.env.bind_symbol(id, SymbolValue::Function)
}

fn codegen_let<'ctx>(
    id: Identifier,
    type_expr: Option<Expression>,
    expr: Option<Expression>,
    ctx: &mut CodegenCtx<'ctx>,
) {
    ctx.env.bind_symbol(id, SymbolValue::Variable)
}

fn codegen_type_decl<'ctx>(id: Identifier, expr: Expression, ctx: &mut CodegenCtx<'ctx>) {
    ctx.env.bind_symbol(id, SymbolValue::Type)
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
        match stmt {
            Statement::Let(id, t, expr) => codegen_let(id, t, expr, ctx),
            Statement::FnDeclaration(id, fn_expr) => codegen_function_decl(id, fn_expr, ctx),
            Statement::TypeDeclaration(id, expr) => codegen_type_decl(id, expr, ctx),
            Statement::Expression(expr) => codegen_expression(expr, ctx),
            _ => {}
        }
    }
    ctx.builder.build_return(None);
    Ok(main_fn)
}

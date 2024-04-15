use inkwell::values::{AnyValue, AnyValueEnum};

use crate::parser::Ast;
use crate::symbols::Ttype;

use super::{to_basic_value_enum, Compiler};
impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn build_list(&mut self, exprs: &Vec<Ast>, ttype: &Ttype) -> Option<AnyValueEnum<'ctx>> {
        let es: Vec<AnyValueEnum> = exprs.iter().map(|e| self.codegen(e).unwrap()).collect();
        if let Ttype::List(list_type) = ttype {
            let llvm_list_type =
                self.type_to_llvm_array_type(*list_type.clone(), es.len().try_into().unwrap());

            let array_alloca = self
                .builder
                .build_array_alloca(
                    llvm_list_type,
                    self.context
                        .i32_type()
                        .const_int(es.len().try_into().unwrap(), false),
                    "array_alloca",
                )
                .unwrap();

            let z = self.context.i32_type().const_int(0, true);

            for (i, v) in es.iter().enumerate() {
                unsafe {
                    let gep = self
                        .builder
                        .build_in_bounds_gep(
                            llvm_list_type,
                            array_alloca,
                            &[
                                z,
                                self.context
                                    .i64_type()
                                    .const_int(TryInto::try_into(i).unwrap(), true),
                            ],
                            "inbounds_gep",
                        )
                        .unwrap();

                    self.builder.build_store(gep, to_basic_value_enum(*v));
                };
            }

            Some(array_alloca.as_any_value_enum())
        } else {
            None
        }
    }
    pub fn get_list_element(
        &mut self,
        obj: &Ast,
        idx: &Ast,
        _ttype: &Ttype,
    ) -> Option<AnyValueEnum<'ctx>> {
        let obj = self.codegen(obj)?;
        let idx = self.codegen(idx)?;
        let ptr_type = obj.get_type().into_pointer_type();

        unsafe {
            let gep = self
                .builder
                .build_in_bounds_gep(
                    ptr_type,
                    obj.into_pointer_value(),
                    &[idx.into_int_value()],
                    "get_index",
                )
                .unwrap();
            let value = self
                .builder
                .build_load(ptr_type, gep, "load_from_gep")
                .unwrap();
            Some(value.as_any_value_enum())
        }
    }
}

use inkwell::values::{AnyValue, AnyValueEnum};

use crate::parser::Ast;
use crate::symbols::Ttype;

use super::{to_basic_value_enum, Compiler};
impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn build_list(
        &mut self,
        exprs: &Vec<Ast>,
        list_type: &Ttype,
    ) -> Option<AnyValueEnum<'ctx>> {
        let es: Vec<AnyValueEnum> = exprs.iter().map(|e| self.codegen(e).unwrap()).collect();
        let llvm_list_type =
            self.type_to_llvm_array_type(list_type.clone(), es.len().try_into().unwrap());

        let list_len = es.len();
        let array_alloca = self
            .builder
            .build_array_alloca(
                llvm_list_type,
                self.context
                    .i32_type()
                    .const_int(list_len.try_into().unwrap(), false),
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

                let _ = self.builder.build_store(gep, to_basic_value_enum(*v));
            };
        }

        let list_struct = self.context.const_struct(
            &[
                array_alloca.into(),
                self.context
                    .i32_type()
                    .const_int(list_len.try_into().unwrap(), false)
                    .into(),
            ],
            false,
        );

        Some(list_struct.as_any_value_enum())
    }
    pub fn get_list_size(
        &mut self,
        var: &AnyValueEnum<'ctx>,
        var_type: &Ttype,
    ) -> Option<AnyValueEnum<'ctx>> {
        if let Ttype::List(el_type) = var_type {
            let element_ptr_type = self.type_to_llvm_ptr_type(*el_type.clone());
            let list_struct_type = self
                .context
                .struct_type(&[element_ptr_type, self.context.i32_type().into()], false);

            let size_gep = self
                .builder
                .build_struct_gep(
                    list_struct_type,
                    var.into_pointer_value(),
                    1,
                    "list_struct_gep_size",
                )
                .unwrap();
            let val_len = self
                .builder
                .build_load(self.context.i32_type(), size_gep, "list_len")
                .unwrap();
            Some(val_len.as_any_value_enum())
        } else {
            None
        }
    }
    pub fn get_list_element(
        &mut self,
        obj: &Ast,
        idx: &Ast,
        element_type: &Ttype,
    ) -> Option<AnyValueEnum<'ctx>> {
        let element_ptr_type = self.type_to_llvm_ptr_type(element_type.clone());
        let _list_struct_type = self
            .context
            .struct_type(&[element_ptr_type, self.context.i32_type().into()], false);

        match self.codegen(obj) {
            Some(AnyValueEnum::PointerValue(list_struct_ptr)) => {
                let idx = self.codegen(idx)?;
                unsafe {
                    let struct_gep_to_array = self
                        .builder
                        .build_in_bounds_gep(
                            element_ptr_type,
                            list_struct_ptr,
                            &[self
                                .builder
                                .build_int_cast(
                                    idx.into_int_value(),
                                    self.context.i32_type(),
                                    "cast_index",
                                )
                                .unwrap()],
                            "list_struct_gep",
                        )
                        .unwrap();

                    let element_type = self.type_to_llvm_type(element_type.clone());

                    let value = self
                        .builder
                        .build_load(element_type, struct_gep_to_array, "load_from_gep")
                        .unwrap();

                    Some(value.as_any_value_enum())
                }
            }
            Some(AnyValueEnum::StructValue(list_struct)) => {
                // let array = self
                //     .builder
                //     .build_struct_gep(list_struct_type, list_struct_ptr, 1, "list_struct_gep")
                //     .unwrap();

                None
            }
            _ => None,
        }
    }
}

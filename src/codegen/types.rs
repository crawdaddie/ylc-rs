use inkwell::{
    types::{BasicMetadataTypeEnum, BasicTypeEnum, FunctionType},
    AddressSpace,
};

use super::Compiler;

use crate::symbols::{Numeric, Ttype};
impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn get_type_enum(&self, t: Ttype) -> BasicMetadataTypeEnum<'ctx> {
        self.type_to_llvm_type(t).into()
        // match t {
        //     Ttype::Numeric(Numeric::Int) => self.context.i64_type().into(),
        //
        //     Ttype::Numeric(Numeric::Num) => self.type_to_llvm_type(t).into(),
        //
        //     Ttype::Str => self.type_to_llvm_type(t).into(),
        //
        //     _ => self.context.bool_type().into(),
        // }
    }

    pub fn type_to_llvm_type(&self, ttype: Ttype) -> BasicTypeEnum<'ctx> {
        match ttype {
            Ttype::Numeric(Numeric::Int) => self.context.i64_type().into(),
            Ttype::Bool => self.context.bool_type().into(),
            Ttype::Numeric(Numeric::Num) => self.context.f64_type().into(),
            Ttype::Str => self
                .context
                .struct_type(
                    &[
                        self.context
                            .i8_type()
                            .ptr_type(AddressSpace::default())
                            .into(),
                        self.context.i64_type().into(),
                    ],
                    false,
                )
                .into(),
            Ttype::Tuple(ts) => {
                let llvm_types = ts
                    .iter()
                    .map(|x| self.type_to_llvm_type(x.clone()))
                    .collect::<Vec<_>>();

                self.context
                    .struct_type(llvm_types.as_slice(), false)
                    .into()
            }
            _ => panic!("Type -> LLVM Type Not implemented {:?}", ttype),
        }
    }

    pub fn type_to_llvm_array_type(&self, ttype: Ttype, size: u32) -> BasicTypeEnum<'ctx> {
        match ttype {
            Ttype::Numeric(Numeric::Int) => self.context.i64_type().array_type(size).into(),
            Ttype::Bool => self.context.bool_type().array_type(size).into(),
            Ttype::Numeric(Numeric::Num) => self.context.f64_type().array_type(size).into(),
            Ttype::Tuple(ts) => {
                let llvm_types = ts
                    .iter()
                    .map(|x| self.type_to_llvm_type(x.clone()))
                    .collect::<Vec<_>>();

                self.context
                    .struct_type(llvm_types.as_slice(), false)
                    .array_type(size)
                    .into()
            }
            _ => panic!("Type -> LLVM Type Not implemented {:?}", ttype),
        }
    }

    pub fn type_to_llvm_fn(&self, ttype: Ttype) -> FunctionType<'ctx> {
        match ttype {
            Ttype::Numeric(Numeric::Int) => self.context.i64_type().fn_type(&[], false),
            Ttype::Bool => self.context.bool_type().fn_type(&[], false),
            Ttype::Numeric(Numeric::Num) => self.context.f64_type().fn_type(&[], false),
            Ttype::Tuple(ts) => {
                let llvm_types = ts
                    .iter()
                    .map(|x| self.type_to_llvm_type(x.clone()))
                    .collect::<Vec<_>>();

                self.context
                    .struct_type(llvm_types.as_slice(), false)
                    .fn_type(&[], false)
            }

            _ => self.context.void_type().fn_type(&[], false),
        }
    }
}

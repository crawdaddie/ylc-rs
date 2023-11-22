

use inkwell::types::{BasicMetadataTypeEnum};



use super::Compiler;


use crate::symbols::{Numeric, Ttype};
impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn get_type_enum(&self, t: Ttype) -> BasicMetadataTypeEnum<'ctx> {
        match t {
            Ttype::Numeric(Numeric::Int) => {
                TryInto::<BasicMetadataTypeEnum>::try_into(self.context.i64_type()).unwrap()
            }

            Ttype::Numeric(Numeric::Num) => {
                TryInto::<BasicMetadataTypeEnum>::try_into(self.context.f64_type()).unwrap()
            }
            _ => TryInto::<BasicMetadataTypeEnum>::try_into(self.context.bool_type()).unwrap(),
        }
    }
}

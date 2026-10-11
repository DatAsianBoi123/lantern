use crate::{def::{LanternNative, NativeRepr, PrimitiveOps}, expr::{BinaryOperation, UnaryOperation}, ty::{LanternType, TypeId}};

macro_rules! define_native_funs {
    (for $tcx:pat, $(#[$meta:meta])* $vis:vis enum $ident:ident {
        $(
        $variant:ident = $([$base_pat:pat = $base:expr])? $name:literal ($($pat:pat = $ty:expr),* $(,)?) -> $ret_pat:pat $( = $ret:expr )?,
        )*
    }) => {
        $(#[$meta])*
        $vis enum $ident {$(
            $variant,
        )*}

        impl $ident {
            #[allow(unused)]
            pub fn from_def<'t>(
                base: Option<TypeId<'t>>,
                name: &str,
                args: &[TypeId<'t>],
                ret: TypeId<'t>,
                tcx: &$crate::ty::TypeContext<'t>,
            ) -> Result<Self, FromDefError> {
                let $tcx = tcx;
                match (base.as_deref(), name) {
                    $(
                        (base, $name) $( if let Some($base_pat) = base && $base )? => {
                            let mut args_iter = args.iter().copied();
                            $(
                                match args_iter.next().as_deref() {
                                    Some($pat) if $ty => {}
                                    _ => return Err(FromDefError::MismatchedArgs),
                                }
                            )*
                            if args_iter.next().is_some() {
                                return Err(FromDefError::MismatchedArgs);
                            }
                            match &*ret {
                                $ret_pat $(if $ret)? => {}
                                _ => return Err(FromDefError::MismatchedRet),
                            }
                            Ok(Self::$variant)
                        }
                    )*
                    _ => Err(FromDefError::NotFound),
                }
            }
        }
    };
}

define_native_funs! { for tcx,
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum NativeFun {
        Write = "write"(ty = ty.is_primitive_type(&STR_NATIVE)) -> LanternType::None,
        Flush = "flush"() -> LanternType::None,
        Gc = "gc"() -> LanternType::None,
        FloatToStr = [ty = ty.is_primitive_type(&FLOAT_NATIVE)]"to_str"(ty = ty.is_primitive_type(&FLOAT_NATIVE))
            -> ty = ty.is_primitive_type(&STR_NATIVE),
        FloatFromStr = [ty = ty.is_primitive_type(&FLOAT_NATIVE)]"from_str"(ty = ty.is_primitive_type(&STR_NATIVE))
            -> ty = ty.is_primitive_type(&FLOAT_NATIVE),
        IntToStr = [ty = ty.is_primitive_type(&INT_NATIVE)]"to_str"(ty = ty.is_primitive_type(&INT_NATIVE))
            -> ty = ty.is_primitive_type(&STR_NATIVE),
        IntFromStr = [ty = ty.is_primitive_type(&INT_NATIVE)]"from_str"(ty = ty.is_primitive_type(&STR_NATIVE))
            -> ty = ty.is_primitive_type(&INT_NATIVE),
        Input = "input"() -> ty = ty.is_primitive_type(&STR_NATIVE),
        Concat = [ty = ty.is_primitive_type(&STR_NATIVE)]"concat"(
            ty = ty.is_primitive_type(&STR_NATIVE),
            ty = ty.is_primitive_type(&STR_NATIVE),
        ) -> ty = ty.is_primitive_type(&STR_NATIVE),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FromDefError {
    NotFound,
    MismatchedArgs,
    MismatchedRet,
}

pub static BYTE_NATIVE: LanternNative = LanternNative {
    name: "byte",
    id: 0,
    repr: NativeRepr::Value { size: 1, align: 1 },
    ops: PrimitiveOps {
        not_op: None,
        negate_op: Some(UnaryOperation::Negi),
        add_op: Some(BinaryOperation::Addi),
        sub_op: Some(BinaryOperation::Subi),
        mult_op: Some(BinaryOperation::Multi),
        div_op: Some(BinaryOperation::Divi),
        mod_op: Some(BinaryOperation::Modi),
        lt_op: Some(BinaryOperation::ICompareLt),
        le_op: Some(BinaryOperation::ICompareLe),
        gt_op: Some(BinaryOperation::ICompareGt),
        ge_op: Some(BinaryOperation::ICompareGe),
        eq_op: Some(BinaryOperation::ICompareEq),
        neq_op: Some(BinaryOperation::ICompareNeq),
    },
};
pub static INT_NATIVE: LanternNative = LanternNative {
    name: "int",
    id: 1,
    repr: NativeRepr::Value { size: 8, align: 8 },
    ops: PrimitiveOps {
        not_op: None,
        negate_op: Some(UnaryOperation::Negi),
        add_op: Some(BinaryOperation::Addi),
        sub_op: Some(BinaryOperation::Subi),
        mult_op: Some(BinaryOperation::Multi),
        div_op: Some(BinaryOperation::Divi),
        mod_op: Some(BinaryOperation::Modi),
        lt_op: Some(BinaryOperation::ICompareLt),
        le_op: Some(BinaryOperation::ICompareLe),
        gt_op: Some(BinaryOperation::ICompareGt),
        ge_op: Some(BinaryOperation::ICompareGe),
        eq_op: Some(BinaryOperation::ICompareEq),
        neq_op: Some(BinaryOperation::ICompareNeq),
    },
};
pub static FLOAT_NATIVE: LanternNative = LanternNative {
    name: "float",
    id: 2,
    repr: NativeRepr::Value { size: 8, align: 8 },
    ops: PrimitiveOps {
        not_op: None,
        negate_op: Some(UnaryOperation::Negf),
        add_op: Some(BinaryOperation::Addf),
        sub_op: Some(BinaryOperation::Subf),
        mult_op: Some(BinaryOperation::Multf),
        div_op: Some(BinaryOperation::Divf),
        mod_op: Some(BinaryOperation::Modf),
        lt_op: Some(BinaryOperation::FCompareLt),
        le_op: Some(BinaryOperation::FCompareLe),
        gt_op: Some(BinaryOperation::FCompareGt),
        ge_op: Some(BinaryOperation::FCompareGe),
        eq_op: Some(BinaryOperation::FCompareEq),
        neq_op: Some(BinaryOperation::FCompareNeq),
    },
};
pub static BOOL_NATIVE: LanternNative = LanternNative {
    name: "bool",
    id: 3,
    repr: NativeRepr::Value { size: 1, align: 1 },
    ops: PrimitiveOps {
        not_op: Some(UnaryOperation::Not),
        eq_op: Some(BinaryOperation::ICompareEq),
        neq_op: Some(BinaryOperation::ICompareNeq),
        ..PrimitiveOps::none()
    },
};
pub static STR_NATIVE: LanternNative = LanternNative {
    name: "str",
    id: 4,
    repr: NativeRepr::Ref,
    ops: PrimitiveOps::none(),
};

pub fn get_native(name: &str) -> Option<&'static LanternNative> {
    match name {
        "byte" => Some(&BYTE_NATIVE),
        "int" => Some(&INT_NATIVE),
        "float" => Some(&FLOAT_NATIVE),
        "bool" => Some(&BOOL_NATIVE),
        "str" => Some(&STR_NATIVE),
        _ => None,
    }
}


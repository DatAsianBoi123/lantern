use crate::{primitive::{BYTE_PRIMITIVE, FLOAT_PRIMITIVE, INT_PRIMITIVE}, ty::{BuiltinType, LanternType, TypeId}};

macro_rules! define_natives {
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

define_natives! { for tcx,
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum NativeFun {
        Write = "write"(LanternType::Array(inner) = inner.is_primitive_type(&BYTE_PRIMITIVE)) -> LanternType::None,
        Flush = "flush"() -> LanternType::None,
        Gc = "gc"() -> LanternType::None,
        FloatToStr = [ty = ty.is_primitive_type(&FLOAT_PRIMITIVE)]"to_str"(ty = ty.is_primitive_type(&FLOAT_PRIMITIVE))
            -> ty = tcx.builtin(BuiltinType::String).eq_ty(ty),
        FloatFromStr = [ty = ty.is_primitive_type(&FLOAT_PRIMITIVE)]"from_str"(ty = tcx.builtin(BuiltinType::String).eq_ty(ty))
            -> ty = ty.is_primitive_type(&FLOAT_PRIMITIVE),
        IntToStr = [ty = ty.is_primitive_type(&INT_PRIMITIVE)]"to_str"(ty = ty.is_primitive_type(&INT_PRIMITIVE))
            -> ty = tcx.builtin(BuiltinType::String).eq_ty(ty),
        IntFromStr = [ty = ty.is_primitive_type(&INT_PRIMITIVE)]"from_str"(ty = tcx.builtin(BuiltinType::String).eq_ty(ty))
            -> ty = ty.is_primitive_type(&INT_PRIMITIVE),
        Input = "input"() -> ty = tcx.builtin(BuiltinType::String).eq_ty(ty),
        Concat = [ty = tcx.builtin(BuiltinType::String).eq_ty(ty)]"concat"(
            ty = tcx.builtin(BuiltinType::String).eq_ty(ty),
            ty = tcx.builtin(BuiltinType::String).eq_ty(ty),
        ) -> ty = tcx.builtin(BuiltinType::String).eq_ty(ty),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FromDefError {
    NotFound,
    MismatchedArgs,
    MismatchedRet,
}


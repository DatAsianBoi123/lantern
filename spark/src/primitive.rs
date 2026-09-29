use crate::{def::{LanternPrimitive, PrimitiveOps}, expr::{BinaryOperation, UnaryOperation}};

pub static BYTE_PRIMITIVE: LanternPrimitive = LanternPrimitive {
    name: "byte",
    id: 0,
    size: 1,
    align: 1,
    ops: PrimitiveOps {
        not_inst: None,
        negate_inst: Some(UnaryOperation::Negi),
        add_inst: Some(BinaryOperation::Addi),
        sub_inst: Some(BinaryOperation::Subi),
        mult_inst: Some(BinaryOperation::Multi),
        div_inst: Some(BinaryOperation::Divi),
        mod_inst: Some(BinaryOperation::Modi),
        lt_inst: Some(BinaryOperation::ICompareLt),
        le_inst: Some(BinaryOperation::ICompareLe),
        gt_inst: Some(BinaryOperation::ICompareGt),
        ge_inst: Some(BinaryOperation::ICompareGe),
        eq_inst: Some(BinaryOperation::ICompareEq),
        neq_inst: Some(BinaryOperation::ICompareNeq),
    },
};
pub static INT_PRIMITIVE: LanternPrimitive = LanternPrimitive {
    name: "int",
    id: 1,
    size: 8,
    align: 8,
    ops: PrimitiveOps {
        not_inst: None,
        negate_inst: Some(UnaryOperation::Negi),
        add_inst: Some(BinaryOperation::Addi),
        sub_inst: Some(BinaryOperation::Subi),
        mult_inst: Some(BinaryOperation::Multi),
        div_inst: Some(BinaryOperation::Divi),
        mod_inst: Some(BinaryOperation::Modi),
        lt_inst: Some(BinaryOperation::ICompareLt),
        le_inst: Some(BinaryOperation::ICompareLe),
        gt_inst: Some(BinaryOperation::ICompareGt),
        ge_inst: Some(BinaryOperation::ICompareGe),
        eq_inst: Some(BinaryOperation::ICompareEq),
        neq_inst: Some(BinaryOperation::ICompareNeq),
    },
};
pub static FLOAT_PRIMITIVE: LanternPrimitive = LanternPrimitive {
    name: "float",
    id: 2,
    size: 8,
    align: 8,
    ops: PrimitiveOps {
        not_inst: None,
        negate_inst: Some(UnaryOperation::Negf),
        add_inst: Some(BinaryOperation::Addf),
        sub_inst: Some(BinaryOperation::Subf),
        mult_inst: Some(BinaryOperation::Multf),
        div_inst: Some(BinaryOperation::Divf),
        mod_inst: Some(BinaryOperation::Modf),
        lt_inst: Some(BinaryOperation::FCompareLt),
        le_inst: Some(BinaryOperation::FCompareLe),
        gt_inst: Some(BinaryOperation::FCompareGt),
        ge_inst: Some(BinaryOperation::FCompareGe),
        eq_inst: Some(BinaryOperation::FCompareEq),
        neq_inst: Some(BinaryOperation::FCompareNeq),
    },
};
pub static BOOL_PRIMITIVE: LanternPrimitive = LanternPrimitive {
    name: "bool",
    id: 3,
    size: 1,
    align: 1,
    ops: PrimitiveOps {
        not_inst: Some(UnaryOperation::Not),
        negate_inst: None,
        add_inst: None,
        sub_inst: None,
        mult_inst: None,
        div_inst: None,
        mod_inst: None,
        lt_inst: None,
        le_inst: None,
        gt_inst: None,
        ge_inst: None,
        eq_inst: Some(BinaryOperation::ICompareEq),
        neq_inst: Some(BinaryOperation::ICompareNeq),
    },
};

pub fn get_primitive(name: &str) -> Option<&'static LanternPrimitive> {
    match name {
        "byte" => Some(&BYTE_PRIMITIVE),
        "int" => Some(&INT_PRIMITIVE),
        "float" => Some(&FLOAT_PRIMITIVE),
        "bool" => Some(&BOOL_PRIMITIVE),
        _ => None,
    }
}


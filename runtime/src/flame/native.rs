use std::io::Write;

use anyhow::anyhow;

use crate::{Slot, VM, error::RuntimeError, flame::{LanternPrimitive, PrimitiveOps, instruction::Instruction}, heap::HeapArray};

macro_rules! native_funs {
    (for $vm: pat, $( $name: literal = ( $($pat: pat),* $(,)? ) => $expr: expr ),* $(,)?) => {
        pub fn get_native_fn(name: &str) -> Option<$crate::flame::NativeFn> {
            match name {
            $(
                $name => {
                    #[allow(unused)]
                    fn inner(vm: &mut $crate::VM) -> Result<crate::Slot, $crate::error::RuntimeError> {
                        let stack = vm.stack();
                        let frame = vm.frames().last().expect("called from a frame");
                        let mut curr = 0;
                        $(
                            let $pat = stack[frame.bottom + curr];
                            curr += 1;
                        )*
                        let $vm = vm;
                        $expr
                    }
                    Some(inner as $crate::flame::NativeFn)
                },
            )*
                _ => None,
            }
        }
    };
}

pub fn dummy_native(_: &mut VM) -> Result<Slot, RuntimeError> {
    Err(RuntimeError(anyhow!("Called dummy native function! (Report this)").into()))
}

native_funs![for vm,
    "write" = (bytes) => {
        let byte_ptr = unsafe { HeapArray::from_raw(*bytes.read::<*mut u8>()) };
        let mut stdout = std::io::stdout();
        unsafe {
            let bytes = std::slice::from_raw_parts(byte_ptr.element_ptr(), byte_ptr.len());
            stdout.write_all(bytes).unwrap();
        }
        Ok(Slot::new_primitive(0))
    },
    "flush" = () => {
        std::io::stdout().flush();
        Ok(Slot::new_primitive(0))
    },
    "gc" = () => {
        vm.heap.gc(&mut vm.stack);
        Ok(Slot::new_primitive(0))
    },
    "float_to_str" = (float) => {
        let float = unsafe { *float.read::<f64>() };
        Ok(Slot::new_ref(vm.alloc_string(float.to_string().as_bytes())?.as_ptr()))
    },
    "int_to_str" = (int) => {
        let int = unsafe { *int.read::<i64>() };
        Ok(Slot::new_ref(vm.alloc_string(int.to_string().as_bytes())?.as_ptr()))
    },
    "input_float" = () => {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        Ok(Slot::new_primitive(input.trim().parse::<f64>().map_err(|_| RuntimeError(anyhow!("not a float").into()))?))
    },
    "input_int" = () => {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        Ok(Slot::new_primitive(input.trim().parse::<i64>().map_err(|_| RuntimeError(anyhow!("not an integer").into()))?))
    },
];

pub static BYTE_PRIMITIVE: LanternPrimitive = LanternPrimitive {
    id: 0,
    size: 1,
    align: 1,
    ops: PrimitiveOps {
        not_inst: None,
        negate_inst: Some(Instruction::Negi),
        add_inst: Some(Instruction::Addi),
        sub_inst: Some(Instruction::Subi),
        mult_inst: Some(Instruction::Multi),
        div_inst: Some(Instruction::Divi),
        mod_inst: Some(Instruction::Modi),
        lt_inst: Some(Instruction::ICompareLt),
        le_inst: Some(Instruction::ICompareLe),
        gt_inst: Some(Instruction::ICompareGt),
        ge_inst: Some(Instruction::ICompareGe),
        eq_inst: Some(Instruction::ICompareEq),
    },
};
pub static INT_PRIMITIVE: LanternPrimitive = LanternPrimitive {
    id: 1,
    size: 8,
    align: 8,
    ops: PrimitiveOps {
        not_inst: None,
        negate_inst: Some(Instruction::Negi),
        add_inst: Some(Instruction::Addi),
        sub_inst: Some(Instruction::Subi),
        mult_inst: Some(Instruction::Multi),
        div_inst: Some(Instruction::Divi),
        mod_inst: Some(Instruction::Modi),
        lt_inst: Some(Instruction::ICompareLt),
        le_inst: Some(Instruction::ICompareLe),
        gt_inst: Some(Instruction::ICompareGt),
        ge_inst: Some(Instruction::ICompareGe),
        eq_inst: Some(Instruction::ICompareEq),
    },
};
pub static FLOAT_PRIMITIVE: LanternPrimitive = LanternPrimitive {
    id: 2,
    size: 8,
    align: 8,
    ops: PrimitiveOps {
        not_inst: None,
        negate_inst: Some(Instruction::Negf),
        add_inst: Some(Instruction::Addf),
        sub_inst: Some(Instruction::Subf),
        mult_inst: Some(Instruction::Multf),
        div_inst: Some(Instruction::Divf),
        mod_inst: Some(Instruction::Modf),
        lt_inst: Some(Instruction::FCompareLt),
        le_inst: Some(Instruction::FCompareLe),
        gt_inst: Some(Instruction::FCompareGt),
        ge_inst: Some(Instruction::FCompareGe),
        eq_inst: Some(Instruction::FCompareEq),
    },
};
pub static BOOL_PRIMITIVE: LanternPrimitive = LanternPrimitive {
    id: 3,
    size: 1,
    align: 1,
    ops: PrimitiveOps {
        not_inst: Some(Instruction::Not),
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
        eq_inst: Some(Instruction::ICompareEq),
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


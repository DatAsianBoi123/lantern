use std::io::Write;

use anyhow::anyhow;

use crate::{Slot, error::RuntimeError, heap::HeapArray};

macro_rules! natives {
    (for $vm: pat, $( $name: literal = ( $($pat: pat),* $(,)? ) => $expr: expr ),* $(,)?) => {
        pub fn get_native_fn(name: &str) -> Option<$crate::flame::NativeFn> {
            match name {
            $(
                $name => {
                    #[allow(unused)]
                    fn inner(vm: &mut $crate::VM, slots: [$crate::Slot; 256]) -> Result<crate::Slot, $crate::error::RuntimeError> {
                        let $vm = vm;
                        let mut curr = 0;
                        $(
                            let $pat = slots[curr];
                            curr += 1;
                        )*
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

natives![for vm,
    "print" = (string) => {
        let string = unsafe { HeapArray::from_raw(*string.read::<*mut u8>()) };
        let mut stdout = std::io::stdout();
        unsafe {
            let len = string.len();
            for i in 0..len {
                let byte = *string.element_ptr().add(i);
                stdout.write_all(&[byte]).unwrap();
            }
            stdout.write_all(b"\n").unwrap();
            stdout.flush().unwrap();
        }
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


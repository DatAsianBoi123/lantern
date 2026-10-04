use core::str;
use std::{io::Write, ptr};

use crate::{Slot, heap::HeapArray};

macro_rules! native_funs {
    (for $vm: pat, $( $native: ident( $($pat: pat),* $(,)? ) $expr: block )*) => {
        pub fn get_native_spark_fn(native: ::spark::native::NativeFun) -> $crate::flame::NativeFn {
            match native {$(
                ::spark::native::NativeFun::$native => {
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
                    inner as $crate::flame::NativeFn
                }
            )*}
        }
    };
}

native_funs![for vm,
    Write(bytes) {
        let byte_ptr = unsafe { HeapArray::from_raw(bytes.read_ptr()) };
        let mut stdout = std::io::stdout();
        unsafe {
            let bytes = std::slice::from_raw_parts(byte_ptr.element_ptr(), byte_ptr.len());
            stdout.write_all(bytes).map_err(|err| vm.throw(err));
        }
        Ok(Slot::new_usize(0))
    }
    Flush() {
        std::io::stdout().flush().map_err(|err| vm.throw(err));
        Ok(Slot::new_usize(0))
    }
    Gc() {
        vm.heap.gc(&mut vm.stack);
        Ok(Slot::new_usize(0))
    }
    FloatToStr(float) {
        let float = unsafe { float.read_float() };
        Ok(Slot::new_ref(vm.alloc_string(float.to_string().as_bytes())?.as_mut_ptr()))
    }
    FloatFromStr(str) {
        let mut str = unsafe { HeapObject::from_raw(str.read_ptr()) };
        let bytes = unsafe { HeapArray::from_raw(str.field_ptr_mut()) };

        let slice = unsafe { std::slice::from_raw_parts(bytes.as_ptr(), bytes.len()) };
        let float = unsafe { str::from_utf8_unchecked(slice).parse().map_err(|_| vm.throw("not a float"))? };
        Ok(Slot::new_float(float))
    }
    IntToStr(int) {
        let int = unsafe { int.read_int() };
        Ok(Slot::new_ref(vm.alloc_string(int.to_string().as_bytes())?.as_mut_ptr()))
    }
    IntFromStr(str) {
        let mut str = unsafe { HeapObject::from_raw(str.read_ptr()) };
        let bytes = unsafe { HeapArray::from_raw(str.field_ptr_mut()) };

        let slice = unsafe { std::slice::from_raw_parts(bytes.as_ptr(), bytes.len()) };
        let int = unsafe { str::from_utf8_unchecked(slice).parse().map_err(|_| vm.throw("not a float"))? };
        Ok(Slot::new_int(int))
    }
    Input() {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).map_err(|err| vm.throw(err));
        Ok(Slot::new_ref(vm.alloc_string(input.trim_suffix('\n').as_bytes())?.as_mut_ptr()))
    }
];


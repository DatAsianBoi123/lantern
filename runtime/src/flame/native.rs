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
    Write(str) {
        let bytes = unsafe { HeapArray::from_raw(str.read_ptr()) };
        let mut stdout = std::io::stdout();
        unsafe {
            let bytes = std::slice::from_raw_parts(bytes.element_ptr(), bytes.len());
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
        let str = unsafe { HeapArray::from_raw(str.read_ptr()) };

        let slice = unsafe { std::slice::from_raw_parts(str.element_ptr(), str.len()) };
        let float = unsafe { str::from_utf8_unchecked(slice).parse().map_err(|_| vm.throw("not a float"))? };
        Ok(Slot::new_float(float))
    }
    IntToStr(int) {
        let int = unsafe { int.read_int() };
        Ok(Slot::new_ref(vm.alloc_string(int.to_string().as_bytes())?.as_mut_ptr()))
    }
    IntFromStr(str) {
        let str = unsafe { HeapArray::from_raw(str.read_ptr()) };

        let slice = unsafe { std::slice::from_raw_parts(str.element_ptr(), str.len()) };
        let int = unsafe { str::from_utf8_unchecked(slice).parse().map_err(|_| vm.throw("not an int"))? };
        Ok(Slot::new_int(int))
    }
    Input() {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).map_err(|err| vm.throw(err));
        Ok(Slot::new_ref(vm.alloc_string(input.trim_suffix('\n').as_bytes())?.as_mut_ptr()))
    }
    Concat() {
        let other = unsafe { HeapArray::from_raw(vm.stack.peek().expect("other argument").read_ptr()) };
        let str = unsafe { HeapArray::from_raw(vm.stack.read(vm.stack.top() - 2).expect("self argument").read_ptr()) };

        let mut new_str = vm.alloc_string_zeroed(str.len() + other.len())?;

        // pop from stack here in case allocating the new string GCs
        let other = unsafe { HeapArray::from_raw(vm.stack.pop().expect("other argument").read_ptr()) };
        let str = unsafe { HeapArray::from_raw(vm.stack.pop().expect("self argument").read_ptr()) };

        unsafe {
            ptr::copy_nonoverlapping(str.element_ptr(), new_str.element_ptr_mut(), str.len());
            ptr::copy_nonoverlapping(other.element_ptr(), new_str.element_ptr_mut().add(str.len()), other.len());
        };

        Ok(Slot::new_ref(new_str.as_mut_ptr()))
    }
];


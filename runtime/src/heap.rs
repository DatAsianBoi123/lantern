use std::{alloc::Layout, ops::Index, time::Instant};

use crate::{SlotType, flame::scope::GlobalVariables, stack::LanternStack};

#[derive(Debug, PartialEq, Eq)]
pub struct Heap {
    from_space: *mut u8,
    to_space: *mut u8,
    size: usize,
    alloc_ptr: *mut u8,
}

impl Drop for Heap {
    fn drop(&mut self) {
        let layout = Layout::from_size_align(self.size * 2, 8).expect("size overflow");
        unsafe { std::alloc::dealloc(self.from_space.min(self.to_space), layout); }
    }
}

impl Heap {
    pub fn new(space_size: usize) -> Self {
        unsafe {
            let layout = Layout::from_size_align(space_size * 2, 8).expect("size overflow");
            let ptr = std::alloc::alloc(layout);
            if ptr.is_null() {
                std::alloc::handle_alloc_error(layout);
            }

            Self {
                from_space: ptr,
                to_space: ptr.add(space_size),
                size: space_size,
                alloc_ptr: ptr,
            }
        }
    }

    pub fn used_space(&self) -> f64 {
        (self.alloc_ptr.addr() - self.from_space.addr()) as f64 / self.size as f64
    }

    pub fn gc(&mut self, stack: &mut LanternStack) {
        let alloc_before = self.alloc_ptr.addr() - self.from_space.addr();

        let before = Instant::now();
        self.collect(stack);
        let moved = self.alloc_ptr.addr() - self.from_space.addr();
        eprintln!("GC Cycle End in {:?} (moved {} bytes, {:.2}%)", Instant::now().duration_since(before), moved, (moved as f64 / alloc_before as f64) * 100.0);
    }

    fn collect(&mut self, stack: &mut LanternStack) {
        self.alloc_ptr = self.to_space;
        std::mem::swap(&mut self.from_space, &mut self.to_space);
        self.move_roots(stack);
        self.scan();
    }

    pub fn grow(&mut self, stack: &mut LanternStack) {
        self.size = self.size.checked_mul(2).unwrap_or_else(|| panic!("heap grew too large"));
        eprintln!("Heap grew to {}", self.size);
        let grow_ptr = unsafe { std::alloc::alloc(Layout::from_size_align(self.size * 2, 8).expect("heap overflow")) };
        let (new_from, new_to) = (grow_ptr, unsafe { grow_ptr.add(self.size) });
        let (old_from, old_to) = (self.from_space, self.to_space);

        // collect from the old from space to the new from space
        self.to_space = new_from;
        self.collect(stack);
        // from space and to space gets swapped during collection
        self.to_space = new_to;

        let old_layout = Layout::from_size_align(self.size, 8).expect("size overflow");
        unsafe { std::alloc::dealloc(old_from.min(old_to), old_layout); }
    }

    fn move_roots(&mut self, stack: &mut LanternStack) {
        for slot in stack {
            if slot.kind() == SlotType::Ref {
                let Some(moved) = self.move_ref(unsafe { slot.read_ptr() }) else { continue; };
                slot.write_ref(moved);
            }
        }
    }

    fn scan(&mut self) {
        let mut scan = self.from_space;

        while scan < self.alloc_ptr {
            self.move_object_refs(scan);
            let size = unsafe { obj_size_of(scan) };
            scan = scan.wrapping_add(size.next_multiple_of(align_of::<ObjectHeader>()));
        }
    }

    fn move_object_refs(&mut self, obj: *mut u8) {
        unsafe {
            let header = &*obj.cast::<ObjectHeader>();
            match &*header.type_info {
                TypeInfo::Object { ref_offets, .. } => {
                    let mut object = HeapObject::from_raw(obj);
                    let fields = object.field_ptr_mut();
                    for offset in ref_offets {
                        let field = fields.add(*offset).cast::<*mut u8>();
                        let obj = *field;
                        let Some(moved) = self.move_ref(obj) else { continue; };
                        field.write(moved);
                    }
                },
                TypeInfo::Array { element_size, is_ref } => {
                    let mut array = HeapArray::from_raw(obj);
                    if *is_ref {
                        let len = array.len();
                        let elements = array.element_ptr_mut();
                        for i in 0..len {
                            let element = elements.add(i * element_size).cast::<*mut u8>();
                            let obj = *element;
                            let Some(moved) = self.move_ref(obj) else { continue; };
                            element.write(moved);
                        }
                    }
                },
            }
        }
    }

    fn move_ref(&mut self, ptr: *mut u8) -> Option<*mut u8> {
        let to_addr = self.to_space.addr();
        if ptr.is_null() || !(to_addr..to_addr + self.size).contains(&ptr.addr()) { return None; };

        unsafe {
            let header = &mut *(ptr.cast::<ObjectHeader>());

            if !header.forwarding_ptr.is_null() {
                return Some(header.forwarding_ptr);
            }

            let total_size = obj_size_of(ptr);
            let moved_ptr = self.next_ptr(total_size).expect("heap overflow");
            std::ptr::copy_nonoverlapping(ptr, moved_ptr, total_size);
            header.forwarding_ptr = moved_ptr;

            Some(moved_ptr)
        }
    }

    pub fn alloc_obj(&mut self, type_info: &TypeInfo) -> Option<HeapObject> {
        let obj_size = match type_info {
            TypeInfo::Object { size, .. } => *size,
            _ => panic!("not an object"),
        };

        let ptr = self.next_ptr(HeapObject::size_of(obj_size))?;
        unsafe {
            Some(HeapObject::write(ptr, type_info))
        }
    }

    pub fn alloc_array(&mut self, len: usize, type_info: &TypeInfo) -> Option<HeapArray> {
        let element_size = match type_info {
            TypeInfo::Array { element_size, .. } => *element_size,
            _ => panic!("not an array"),
        };

        let ptr = self.next_ptr(HeapArray::size_of(len, element_size))?;
        unsafe {
            Some(HeapArray::write(ptr, len, type_info))
        }
    }

    fn next_ptr(&mut self, size: usize) -> Option<*mut u8> {
        if size == 0 {
            panic!("attempted to allocate a ZST");
        }
        let obj_offset = self.alloc_ptr.align_offset(align_of::<ObjectHeader>());
        let total_size = obj_offset + size;
        if self.alloc_ptr.addr() + total_size > self.from_space.addr() + self.size {
            None
        } else {
            let ptr = unsafe { self.alloc_ptr.add(obj_offset) };
            self.alloc_ptr = unsafe { self.alloc_ptr.add(total_size) };

            Some(ptr)
        }
    }
}

unsafe fn obj_size_of(obj: *const u8) -> usize {
    unsafe {
        match &*(*obj.cast::<ObjectHeader>()).type_info {
            TypeInfo::Object { .. } => HeapObject(obj as *mut _).size(),
            TypeInfo::Array { .. } => HeapArray(obj as *mut _).size()
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
#[repr(C)]
pub struct ObjectHeader {
    pub forwarding_ptr: *mut u8,
    pub type_info: *const TypeInfo,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInfo {
    Object {
        size: usize,
        // PERF: use a bitfield to store refs
        ref_offets: Box<[usize]>,
    },
    Array {
        element_size: usize,
        is_ref: bool,
    },
}

/// # Memory Layout
///
/// ┌───────────────────┐
/// │ Header            │
/// ├┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┤
/// │ ...fields         │
/// └───────────────────┘
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct HeapObject(*mut u8);

impl HeapObject {
    /// # SAFETY
    ///
    /// `ptr` must be a valid pointer to a heap object
    pub unsafe fn from_raw(ptr: *mut u8) -> Self {
        Self(ptr)
    }

    unsafe fn write(ptr: *mut u8, type_info: *const TypeInfo) -> Self {
        let header = ObjectHeader {
            forwarding_ptr: std::ptr::null_mut(),
            type_info,
        };

        let mut object = Self(ptr);
        unsafe {
            object.0.cast::<ObjectHeader>().write(header);
            object.field_ptr_mut().write_bytes(0, object.obj_size());
        };
        object
    }

    pub fn field_offset() -> usize {
        // ObjectHeader has no padding and is aligned to 8 so no need to add anything extra
        size_of::<ObjectHeader>()
    }

    pub fn size_of(size: usize) -> usize {
        Self::field_offset() + size
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.0
    }

    pub fn as_mut_ptr(&mut self) -> *mut u8 {
        self.0
    }

    pub fn header(&self) -> &ObjectHeader {
        unsafe { &*self.0.cast() }
    }

    pub fn type_info(&self) -> &TypeInfo {
        unsafe { &*(self.header().type_info) }
    }

    pub fn obj_size(&self) -> usize {
        match self.type_info() {
            TypeInfo::Object { size, .. } => *size,
            _ => unreachable!(),
        }
    }

    pub fn ref_offsets(&self) -> &[usize] {
        match self.type_info() {
            TypeInfo::Object { ref_offets, .. } => ref_offets,
            _ => unreachable!(),
        }
    }

    pub fn field_ptr(&self) -> *const u8 {
        unsafe { self.0.add(Self::field_offset()) }
    }

    pub fn field_ptr_mut(&mut self) -> *mut u8 {
        unsafe { self.0.add(Self::field_offset()) }
    }

    pub fn size(&self) -> usize {
        Self::size_of(self.obj_size())
    }
}

/// # Memory Layout
///
/// ┌───────────────────┐
/// │ Header            │
/// ├┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┤
/// │ len               │
/// │ ...elements       │
/// └───────────────────┘
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct HeapArray(*mut u8);

impl HeapArray {
    /// # SAFETY
    ///
    /// `ptr` must be a valid pointer to a heap array.
    pub unsafe fn from_raw(ptr: *mut u8) -> Self {
        Self(ptr)
    }

    unsafe fn write(ptr: *mut u8, len: usize, type_info: *const TypeInfo) -> Self {
        let header = ObjectHeader {
            forwarding_ptr: std::ptr::null_mut(),
            type_info,
        };

        let mut array = Self(ptr);
        unsafe {
            array.0.cast::<ObjectHeader>().write(header);
            array.0.add(size_of::<ObjectHeader>()).cast::<usize>().write(len);
            array.element_ptr_mut().write_bytes(0, array.len() * array.element_size());
        };
        array
    }

    pub fn element_offset() -> usize {
        // ObjectHeader and usize have the same alignment so no padding bytes needed
        size_of::<ObjectHeader>() + size_of::<usize>()
    }

    pub fn size_of(len: usize, element_size: usize) -> usize {
        Self::element_offset() + len * element_size
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.0
    }

    pub fn as_mut_ptr(&mut self) -> *mut u8 {
        self.0
    }

    pub fn header(&self) -> &ObjectHeader {
        unsafe { &*self.0.cast() }
    }

    pub fn type_info(&self) -> &TypeInfo {
        unsafe { &*(self.header().type_info) }
    }

    pub fn size(&self) -> usize {
        Self::size_of(self.len(), self.element_size())
    }

    pub fn is_ref(&self) -> bool {
        match self.type_info() {
            TypeInfo::Array { is_ref, .. } => *is_ref,
            _ => unreachable!(),
        }
    }

    pub fn element_size(&self) -> usize {
        match self.type_info() {
            TypeInfo::Array { element_size, .. } => *element_size,
            _ => unreachable!(),
        }
    }

    pub fn element_ptr(&self) -> *const u8 {
        unsafe { self.0.add(Self::element_offset()) }
    }

    pub fn element_ptr_mut(&mut self) -> *mut u8 {
        unsafe { self.0.add(Self::element_offset()) }
    }

    pub fn len(&self) -> usize {
        unsafe { *self.0.add(size_of::<ObjectHeader>()).cast() }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get(&self, index: usize) -> Option<*const u8> {
        if index >= self.len() {
            None
        } else {
            unsafe { Some(self.element_ptr().add(self.element_size() * index)) }
        }
    }

    pub fn get_sized(&self, index: isize) -> Option<*const u8> {
        if index.is_negative() {
            self.len().checked_add_signed(index).map(|index| unsafe { self.element_ptr().add(self.element_size() * index) })
        } else {
            self.get(index as usize)
        }
    }

    pub fn get_mut(&mut self, index: usize) -> Option<*mut u8> {
        self.get(index).map(|ptr| ptr as *mut _)
    }

    pub fn get_sized_mut(&mut self, index: isize) -> Option<*mut u8> {
        self.get_sized(index).map(|ptr| ptr as *mut _)
    }

    /// # Safety
    ///
    /// - `index` must be within array bounds
    /// - `ptr` must be valid within element size bytes
    /// - `ptr` must be properly aligned
    pub unsafe fn set(&mut self, index: usize, ptr: *const u8) {
        unsafe {
            let element_ptr = self.element_ptr_mut().add(self.element_size() * index);
            element_ptr.copy_from(ptr, self.element_size());
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ManagedObject(pub HeapObject);

impl ManagedObject {
    /// # Safety
    /// See [HeapObject::from_raw]
    pub unsafe fn drop(ptr: *mut u8) {
        unsafe { Self(HeapObject::from_raw(ptr)) };
    }

    /// # Safety
    /// type_info must be valid
    pub unsafe fn allocate(type_info: *const TypeInfo) -> HeapObject {
        let obj_size = match unsafe { &*type_info } {
            TypeInfo::Object { size, .. } => *size,
            _ => panic!("not an object"),
        };

        let layout = Layout::from_size_align(HeapObject::size_of(obj_size), align_of::<ObjectHeader>()).expect("size overflow");
        let ptr = unsafe { std::alloc::alloc(layout) };
        if ptr.is_null() {
            std::alloc::handle_alloc_error(layout)
        }

        unsafe { HeapObject::write(ptr, type_info) }
    }
}

impl Drop for ManagedObject {
    fn drop(&mut self) {
        unsafe {
            let layout = Layout::from_size_align_unchecked(self.0.size(), align_of::<ObjectHeader>());
            std::alloc::dealloc(self.0.as_mut_ptr(), layout);
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ManagedArray(pub HeapArray);

impl ManagedArray {
    /// # Safety
    /// See [HeapArray::from_raw]
    pub unsafe fn drop(ptr: *mut u8) {
        unsafe { Self(HeapArray::from_raw(ptr)) };
    }

    /// # Safety
    /// type_info must be valid
    pub unsafe fn allocate(len: usize, type_info: *const TypeInfo) -> HeapArray {
        let element_size = match unsafe { &*type_info } {
            TypeInfo::Array { element_size, .. } => *element_size,
            _ => unreachable!(),
        };

        let layout = Layout::from_size_align(HeapArray::size_of(len, element_size), align_of::<ObjectHeader>()).expect("size overflow");
        let ptr = unsafe { std::alloc::alloc(layout) };
        if ptr.is_null() {
            std::alloc::handle_alloc_error(layout)
        }

        unsafe { HeapArray::write(ptr, len, type_info) }
    }
}

impl Drop for ManagedArray {
    fn drop(&mut self) {
        unsafe {
            let layout = Layout::from_size_align_unchecked(self.0.size(), align_of::<ObjectHeader>());
            std::alloc::dealloc(self.0.as_mut_ptr(), layout);
        }
    }
}

#[derive(Debug)]
pub struct GlobalStorage(Box<[*mut u8]>);

impl Index<usize> for GlobalStorage {
    type Output = *mut u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl Drop for GlobalStorage {
    fn drop(&mut self) {
        for ptr in &self.0 {
            unsafe {
                let header = &*ptr.cast::<ObjectHeader>();
                // drop the allocated objects
                match &*header.type_info {
                    TypeInfo::Object { .. } => ManagedObject::drop(*ptr),
                    TypeInfo::Array { .. } => ManagedArray::drop(*ptr),
                }
            }
        }
    }
}

impl GlobalStorage {
    pub fn allocate(
        vars: GlobalVariables,
        bytes_type_info: &TypeInfo,
        string_type_info: &TypeInfo,
    ) -> Self {
        let strs = vars.into_strs();
        // each String contains a HeapObject + HeapArray
        let mut array = Box::new_uninit_slice(strs.len() * 2);
        strs.into_iter().enumerate().for_each(|(i, str)| {
            unsafe {
                let mut bytes = ManagedArray::allocate(str.len(), bytes_type_info);
                for (i, byte) in str.bytes().enumerate() {
                    bytes.set(i, &raw const byte);
                }
                array[i * 2].write(bytes.as_mut_ptr());

                let mut string = ManagedObject::allocate(string_type_info);
                let field_ptr = string.field_ptr_mut().cast::<*mut u8>();
                // this pointer won't dangle since GlobalStorage deallocates everything at once
                field_ptr.write(bytes.as_mut_ptr());
                array[i * 2 + 1].write(string.as_mut_ptr());
            }
        });
        Self(unsafe { array.assume_init() })
    }
}


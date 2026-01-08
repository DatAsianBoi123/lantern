use std::{alloc::Layout, mem::size_of, time::Instant};

use crate::{Frame, SlotType};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Heap {
    from_space: *mut u8,
    to_space: *mut u8,
    size: usize,
    alloc_ptr: *mut u8,
}

impl Drop for Heap {
    fn drop(&mut self) {
        let layout = Layout::from_size_align(self.size * 2, 8).expect("size overflow");
        if self.from_space < self.to_space {
            unsafe { std::alloc::dealloc(self.from_space, layout); };
        } else {
            unsafe { std::alloc::dealloc(self.to_space, layout); };
        }
    }
}

impl Heap {
    pub fn new(space_size: usize) -> Self {
        unsafe {
            let layout = Layout::from_size_align(space_size * 2, 8).expect("size overflow").pad_to_align();
            let ptr = std::alloc::alloc(layout);
            if ptr.is_null() {
                std::alloc::handle_alloc_error(layout);
            }

            let size = layout.size() / 2;
            Self {
                from_space: ptr,
                to_space: ptr.add(size),
                size,
                alloc_ptr: ptr,
            }
        }
    }

    pub fn gc(&mut self, frames: &mut [Frame]) {
        println!("GC Cycle Start");
        let before = Instant::now();
        self.alloc_ptr = self.to_space;
        std::mem::swap(&mut self.from_space, &mut self.to_space);

        for frame in frames {
            self.move_from_roots(frame);
        }

        println!("GC Cycle End in {:?} (moved {} bytes)", Instant::now().duration_since(before), self.alloc_ptr as usize - self.from_space as usize);
    }

    fn move_from_roots(&mut self, frame: &mut Frame) {
        for slot in frame.locals.iter_mut().chain(frame.operand_stack.iter_mut()) {
            if slot.kind() == SlotType::Ref {
                let Some(moved) = self.move_ref(slot.0 as *mut u8) else { continue; };
                println!("Moved {:?} to {moved:?}", slot.0 as *const u8);
                slot.write_ref(moved);
            }
        }
    }

    fn move_object_refs(&mut self, obj: *mut u8) {
        unsafe {
            let header = &*(obj as *const ObjectHeader);
            match &*(header.type_info) {
                TypeInfo::Object { ref_offets: _, .. } => {
                    todo!()
                },
                TypeInfo::Array { element_size, is_ref } => {
                    if !is_ref { return; };
                    let mut array = HeapArray(obj);
                    let len = array.len();
                    let elements = array.element_ptr_mut();
                    for i in 0..len {
                        let element = elements.add(i * element_size) as *mut *mut u8;
                        let obj = *element;
                        let Some(moved) = self.move_ref(obj) else { continue; };
                        println!("Moved {:?} to {moved:?}", obj);
                        element.write(moved);
                    }
                },
            }
        }
    }

    fn move_ref(&mut self, ptr: *mut u8) -> Option<*mut u8> {
        if ptr.is_null() { return None; };

        unsafe {
            let header = &mut *(ptr as *mut ObjectHeader);

            if !header.forwarding_ptr.is_null() && (self.to_space..self.to_space.add(self.size)).contains(&header.forwarding_ptr) {
                return Some(header.forwarding_ptr);
            }

            let total_size = total_size_of(ptr);
            let moved_ptr = self.next_ptr(total_size).expect("heap overflow");
            std::ptr::copy_nonoverlapping(ptr, moved_ptr, total_size);
            header.forwarding_ptr = moved_ptr;

            self.move_object_refs(moved_ptr);

            Some(moved_ptr)
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
        unsafe {
            if self.alloc_ptr.add(size) > self.from_space.add(self.size) {
                None
            } else {
                let ptr = self.alloc_ptr;
                self.alloc_ptr = self.alloc_ptr.add(size);

                Some(ptr)
            }
        }
    }
}

unsafe fn total_size_of(obj: *const u8) -> usize {
    unsafe {
        match &*(*(obj as *const ObjectHeader)).type_info {
            TypeInfo::Object { size, .. } => Layout::from_size_align_unchecked(size_of::<ObjectHeader>() + size, 8).pad_to_align().size(),
            TypeInfo::Array { .. } => HeapArray(obj as *mut u8).size()
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
        ref_offets: &'static [usize],
    },
    Array {
        element_size: usize,
        is_ref: bool,
    },
}

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
            (array.0 as *mut ObjectHeader).write(header);
            (array.0.add(size_of::<ObjectHeader>()) as *mut usize).write(len);
            array.element_ptr_mut().write_bytes(0, array.len());
        };
        array
    }

    pub fn size_of(len: usize, element_size: usize) -> usize {
        unsafe { Layout::from_size_align_unchecked(size_of::<ObjectHeader>() + 8 + len * element_size, 8).pad_to_align().size() }
    }

    pub fn size(&self) -> usize {
        Self::size_of(self.len(), self.element_size())
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.0
    }

    pub fn header(&self) -> &ObjectHeader {
        unsafe { &*(self.0 as *const ObjectHeader) }
    }

    pub fn type_info(&self) -> &TypeInfo {
        unsafe { &*(self.header().type_info) }
    }

    pub fn element_size(&self) -> usize {
        match self.type_info() {
            TypeInfo::Array { element_size, .. } => *element_size,
            _ => unreachable!(),
        }
    }

    pub fn element_ptr(&self) -> *const u8 {
        unsafe { self.0.add(size_of::<ObjectHeader>() + 8) }
    }

    pub fn element_ptr_mut(&mut self) -> *mut u8 {
        unsafe { self.0.add(size_of::<ObjectHeader>() + 8) }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        unsafe { *(self.0.add(size_of::<ObjectHeader>()) as *const usize) }
    }

    pub fn get(&self, index: usize) -> Option<*const u8> {
        if index >= self.len() {
            None
        } else {
            unsafe { Some(self.element_ptr().add(self.element_size() * index)) }
        }
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


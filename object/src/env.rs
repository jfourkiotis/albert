use crate::{Object, Value};
use fnv::FnvHashMap;
use std::cell::RefCell;
use std::rc::Rc;
use string_interner::Sym;
use std::hint::unreachable_unchecked;

pub enum Env<'a> {
    Global { globals: FnvHashMap<Sym, Value<'a>> },
    Local { parent: Rc<RefCell<Env<'a>>>, indexed_entries: Vec<Value<'a>> },
}

impl<'a> Env<'a> {
    pub fn new_global() -> Self {
        Env::Global { globals: Default::default() }
    }
    pub fn with_parent(parent: Rc<RefCell<Env<'a>>>) -> Self {
        Env::Local {
            parent: parent.clone(),
            indexed_entries: vec![Object::Null, Object::Null, Object::Null, Object::Null],
        }
    }
    pub fn get_global(&self, key: Sym) -> Option<Value<'a>> {
        match self {
            Env::Global { globals } => globals.get(&key).cloned(),
            Env::Local { parent, .. } => parent.borrow().get_global(key),
        }
    }
    pub fn get_local(&self, loc: (usize, usize)) -> Option<Value<'a>> {
        match self {
            Env::Local { parent, indexed_entries } => {
                let (depth, index) = loc;
                if depth == 0 {
                    Some(indexed_entries[index].clone())
                } else {
                    parent
                        .borrow()
                        .get_local((depth - 1, index))
                }
            }
            Env::Global { .. } => unsafe { unreachable_unchecked() }
        }
    }
    // parent should be None
    pub fn insert_global(&mut self, key: Sym, obj: Value<'a>) -> Value<'a> {
        if let Env::Global { globals } = self {
            globals.insert(key, obj.clone());
            obj
        } else {
            unsafe { unreachable_unchecked() }
        }
    }
    // loc.0 must be 0 since there is no assignment operator and we
    // insert only with `let` statements
    pub fn insert_local(&mut self, loc: (usize, usize), obj: Value<'a>) -> Value<'a> {
        if let Env::Local { indexed_entries, .. } = self {
            let (_, index) = loc;

            if index >= indexed_entries.len() {
                indexed_entries.resize(index * 2, Object::Null);
            }
            indexed_entries[index] = obj.clone();
            obj
        } else {
            unsafe { unreachable_unchecked() }
        }
    }
}

use crate::{Object, Value};
use fnv::FnvHashMap;
use std::cell::RefCell;
use std::rc::Rc;
use string_interner::Sym;

#[derive(Default)]
pub struct Env<'a> {
    globals: FnvHashMap<Sym, Value<'a>>,
    parent: Option<Rc<RefCell<Env<'a>>>>,
    indexed_entries: Vec<Value<'a>>,
}

impl<'a> Env<'a> {
    pub fn with_parent(parent: Rc<RefCell<Env<'a>>>) -> Self {
        Env {
            globals: FnvHashMap::default(),
            parent: Some(parent.clone()),
            indexed_entries: vec![Object::Null, Object::Null, Object::Null, Object::Null],
        }
    }
    pub fn get_global(&self, key: Sym) -> Option<Value<'a>> {
        match &self.parent {
            Some(p) => p.borrow().get_global(key),
            None => self.globals.get(&key).cloned(),
        }
    }
    pub fn get_local(&self, loc: (usize, usize)) -> Option<Value<'a>> {
        let (depth, index) = loc;
        if depth == 0 {
            Some(self.indexed_entries[index].clone())
        } else {
            self.parent
                .as_ref()
                .unwrap()
                .borrow()
                .get_local((depth - 1, index))
        }
    }
    // parent should be None
    pub fn insert_global(&mut self, key: Sym, obj: Value<'a>) -> Value<'a> {
        self.globals.insert(key, obj.clone());
        obj
    }
    // loc.0 must be 0 since there is no assignment operator and we
    // insert only with `let` statements
    pub fn insert_local(&mut self, loc: (usize, usize), obj: Value<'a>) -> Value<'a> {
        let (_, index) = loc;

        if index >= self.indexed_entries.len() {
            self.indexed_entries.resize(index * 2, Object::Null);
        }
        self.indexed_entries[index] = obj.clone();
        obj
    }
}

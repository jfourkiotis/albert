use std::collections::HashMap;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum SymbolScope {
    Global = 0,
    Local = 1,
    Free = 2,
    Function = 3,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Symbol<'a> {
    pub name: &'a str,
    pub scope: SymbolScope,
    pub index: usize,
}

#[derive(Default)]
pub struct SymbolTable<'a> {
    store: HashMap<&'a str, Symbol<'a>>,
    pub num_definitions: usize,
    outer: Option<*mut SymbolTable<'a>>,
    // contains original symbols from the enclosing scope
    pub free_symbols: Vec<Symbol<'a>>,
}

impl<'a> SymbolTable<'a> {
    pub fn new_enclosed(outer: *mut SymbolTable<'a>) -> Self {
        SymbolTable {
            outer: Some(outer),
            ..Default::default()
        }
    }
    pub fn define(&mut self, name: &'a str) -> Symbol<'a> {
        let symbol = Symbol {
            name,
            scope: if self.outer.is_none() {
                SymbolScope::Global
            } else {
                SymbolScope::Local
            },
            index: self.num_definitions,
        };
        self.num_definitions += 1;
        self.store.insert(name, symbol);
        self.store.get(name).cloned().unwrap()
    }
    pub fn define_function(&mut self, name: &'a str) -> Symbol<'a> {
        let symbol = Symbol {
            name,
            index: 0, // arbitrary choice, the index doesn't matter
            scope: SymbolScope::Function,
        };
        self.store.insert(name, symbol);
        symbol
    }
    fn resolve_local(&self, name: &'a str) -> Option<Symbol<'a>> {
        self.store.get(name).cloned()
    }
    fn resolve_outer(&mut self, name: &'a str) -> Option<Symbol<'a>> {
        if let Some(parent) = self.outer {
            unsafe { (&mut *parent).resolve_or_define_free(name) }
        } else {
            None
        }
    }
    fn define_free(&mut self, mut original: Symbol<'a>) -> Symbol<'a> {
        self.free_symbols.push(original);

        original.index = self.free_symbols.len() - 1;
        original.scope = SymbolScope::Free;
        self.store.insert(original.name, original);
        original
    }
    pub fn resolve_or_define_free(&mut self, name: &'a str) -> Option<Symbol<'a>> {
        let local = self.resolve_local(name);
        if local.is_some() {
            return local;
        }
        let nonlocal = self.resolve_outer(name);
        if nonlocal.is_none() {
            return nonlocal;
        }
        if nonlocal.unwrap().scope == SymbolScope::Global {
            return nonlocal;
        }

        let nonlocal = nonlocal.unwrap();
        Some(self.define_free(nonlocal))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn define_global_symbol_works() {
        let mut expected = HashMap::new();
        expected.insert(
            "a",
            Symbol {
                name: "a",
                scope: SymbolScope::Global,
                index: 0,
            },
        );
        expected.insert(
            "b",
            Symbol {
                name: "b",
                scope: SymbolScope::Global,
                index: 1,
            },
        );

        let mut global: SymbolTable = Default::default();
        let a = global.define("a");
        assert_eq!(a, *expected.get("a").unwrap());
        let b = global.define("b");
        assert_eq!(b, *expected.get("b").unwrap());
    }

    #[test]
    fn resolve_global_symbol_works() {
        let mut global: SymbolTable = Default::default();
        global.define("a");
        global.define("b");

        let expected = [
            Symbol {
                name: "a",
                scope: SymbolScope::Global,
                index: 0,
            },
            Symbol {
                name: "b",
                scope: SymbolScope::Global,
                index: 1,
            },
        ];

        for sym in expected.iter() {
            match global.resolve_or_define_free(sym.name) {
                Some(r) => assert_eq!(r, *sym),
                None => panic!("name '{:?}' could not be resolved", sym.name),
            }
        }
    }

    #[test]
    fn resolve_local_symbol_works() {
        let mut global: SymbolTable = Default::default();
        global.define("a");
        global.define("b");

        let mut local = SymbolTable::new_enclosed(&mut global);
        local.define("c");
        local.define("d");

        let expected = [
            Symbol {
                name: "a",
                scope: SymbolScope::Global,
                index: 0,
            },
            Symbol {
                name: "b",
                scope: SymbolScope::Global,
                index: 1,
            },
            Symbol {
                name: "c",
                scope: SymbolScope::Local,
                index: 0,
            },
            Symbol {
                name: "d",
                scope: SymbolScope::Local,
                index: 1,
            },
        ];

        for sym in expected.iter() {
            if let Some(resolved) = local.resolve_or_define_free(sym.name) {
                assert_eq!(resolved, *sym);
            } else {
                panic!("name {} not resolved", sym.name);
            }
        }
    }

    #[test]
    fn resolve_free_symbols_works() {
        let mut global: SymbolTable = Default::default();
        global.define("a");
        global.define("b");

        let mut flocal = SymbolTable::new_enclosed(&mut global);
        flocal.define("c");
        flocal.define("d");

        let mut slocal = SymbolTable::new_enclosed(&mut flocal);
        slocal.define("e");
        slocal.define("f");

        struct Test<'a> {
            table: &'a mut SymbolTable<'a>,
            expected_symbols: Vec<Symbol<'a>>,
            expected_free_symbols: Vec<Symbol<'a>>,
        };

        let mut tests = [
            Test {
                table: &mut flocal,
                expected_symbols: vec![
                    Symbol {
                        name: "a",
                        scope: SymbolScope::Global,
                        index: 0,
                    },
                    Symbol {
                        name: "b",
                        scope: SymbolScope::Global,
                        index: 1,
                    },
                    Symbol {
                        name: "c",
                        scope: SymbolScope::Local,
                        index: 0,
                    },
                    Symbol {
                        name: "d",
                        scope: SymbolScope::Local,
                        index: 1,
                    },
                ],
                expected_free_symbols: vec![],
            },
            Test {
                table: &mut slocal,
                expected_symbols: vec![
                    Symbol {
                        name: "a",
                        scope: SymbolScope::Global,
                        index: 0,
                    },
                    Symbol {
                        name: "b",
                        scope: SymbolScope::Global,
                        index: 1,
                    },
                    Symbol {
                        name: "c",
                        scope: SymbolScope::Free,
                        index: 0,
                    },
                    Symbol {
                        name: "d",
                        scope: SymbolScope::Free,
                        index: 1,
                    },
                    Symbol {
                        name: "e",
                        scope: SymbolScope::Local,
                        index: 0,
                    },
                    Symbol {
                        name: "f",
                        scope: SymbolScope::Local,
                        index: 1,
                    },
                ],
                expected_free_symbols: vec![
                    Symbol {
                        name: "c",
                        scope: SymbolScope::Local,
                        index: 0,
                    },
                    Symbol {
                        name: "d",
                        scope: SymbolScope::Local,
                        index: 1,
                    },
                ],
            },
        ];

        for test in tests.iter_mut() {
            for sym in test.expected_symbols.iter() {
                let resolved = test.table.resolve_or_define_free(sym.name);
                assert!(
                    resolved.is_some(),
                    "name {} could not be resolved",
                    sym.name
                );
                assert_eq!(*sym, resolved.unwrap());
            }

            assert_eq!(test.expected_free_symbols, test.table.free_symbols);
        }
    }

    #[test]
    fn define_and_resolve_function_names_works() {
        let mut global: SymbolTable = Default::default();
        global.define_function("a");

        let expected = Symbol {
            name: "a",
            scope: SymbolScope::Function,
            index: 0,
        };
        if let Some(result) = global.resolve_or_define_free(expected.name) {
            assert_eq!(expected, result);
        } else {
            panic!("could not resolve {}", expected.name);
        }
    }

    #[test]
    fn shadowing_function_names_works() {
        let mut global: SymbolTable = Default::default();
        global.define_function("a");
        global.define("a");

        let expected = Symbol {
            name: "a",
            scope: SymbolScope::Global,
            index: 0,
        };
        if let Some(result) = global.resolve_or_define_free(expected.name) {
            assert_eq!(expected, result);
        } else {
            panic!("could not resolve {}", expected.name);
        }
    }

    #[test]
    fn resolve_nested_local_symbol_works() {
        let mut global: SymbolTable = Default::default();
        global.define("a");
        global.define("b");

        let mut first_local = SymbolTable::new_enclosed(&mut global);
        first_local.define("c");
        first_local.define("d");

        let mut second_local = SymbolTable::new_enclosed(&mut global);
        second_local.define("e");
        second_local.define("f");

        struct Test<'a> {
            symbol_table: &'a mut SymbolTable<'a>,
            expected_symbols: Vec<Symbol<'a>>,
        };

        let mut tests = [
            Test {
                symbol_table: &mut first_local,
                expected_symbols: vec![
                    Symbol {
                        name: "a",
                        scope: SymbolScope::Global,
                        index: 0,
                    },
                    Symbol {
                        name: "b",
                        scope: SymbolScope::Global,
                        index: 1,
                    },
                    Symbol {
                        name: "c",
                        scope: SymbolScope::Local,
                        index: 0,
                    },
                    Symbol {
                        name: "d",
                        scope: SymbolScope::Local,
                        index: 1,
                    },
                ],
            },
            Test {
                symbol_table: &mut second_local,
                expected_symbols: vec![
                    Symbol {
                        name: "a",
                        scope: SymbolScope::Global,
                        index: 0,
                    },
                    Symbol {
                        name: "b",
                        scope: SymbolScope::Global,
                        index: 1,
                    },
                    Symbol {
                        name: "e",
                        scope: SymbolScope::Local,
                        index: 0,
                    },
                    Symbol {
                        name: "f",
                        scope: SymbolScope::Local,
                        index: 1,
                    },
                ],
            },
        ];

        for test in tests.iter_mut() {
            for sym in test.expected_symbols.iter() {
                if let Some(resolved) = test.symbol_table.resolve_or_define_free(sym.name) {
                    assert_eq!(resolved, *sym);
                } else {
                    panic!("name {} not resolved", sym.name);
                }
            }
        }
    }
}

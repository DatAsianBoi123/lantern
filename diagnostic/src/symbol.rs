use std::{collections::HashMap, rc::Rc};

pub trait SymbolDisplay {
    fn display(&self, symbol_table: &SymbolTable) -> String;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(usize);

impl SymbolDisplay for Symbol {
    fn display(&self, symbol_table: &SymbolTable) -> String {
        symbol_table.resolve(*self).to_string()
    }
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct SymbolTable {
    map: HashMap<Rc<str>, Symbol>,
    strings: Vec<Rc<str>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { map: HashMap::new(), strings: Vec::new() }
    }

    pub fn store(&mut self, str: &str) -> Symbol {
        if let Some(symbol) = self.map.get(str) {
            return *symbol;
        }
        let str = Rc::from(str);
        let symbol = Symbol(self.strings.len());
        self.strings.push(Rc::clone(&str));
        self.map.insert(str, symbol);
        symbol
    }

    pub fn get(&self, str: &str) -> Option<Symbol> {
        self.map.get(str).copied()
    }

    pub fn resolve(&self, symbol: Symbol) -> &str {
        &self.strings[symbol.0]
    }
}


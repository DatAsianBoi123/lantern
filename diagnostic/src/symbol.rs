use std::collections::HashMap;

pub trait SymbolDisplay {
    fn display(&self, symbol_table: &SymbolTable) -> String;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolTable<'a> {
    map: HashMap<&'a str, Symbol>,
    strings: Vec<&'a str>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> Self {
        Self { map: HashMap::new(), strings: Vec::new() }
    }

    pub fn store(&mut self, str: &'a str) -> Symbol {
        let id = self.strings.len();
        self.strings.push(str);
        *self.map.entry(str).or_insert(Symbol(id))
    }

    pub fn get(&self, str: &'a str) -> Option<Symbol> {
        self.map.get(str).copied()
    }

    pub fn resolve(&self, symbol: Symbol) -> &'a str {
        self.strings[symbol.0]
    }
}

impl Default for SymbolTable<'_> {
    fn default() -> Self {
        Self::new()
    }
}


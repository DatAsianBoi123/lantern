use proc_macro::TokenStream;
use syn::{DeriveInput, parse_macro_input};

mod parse;

#[proc_macro_derive(Parse, attributes(parse))]
pub fn derive_parse(stream: TokenStream) -> TokenStream {
    let input = parse_macro_input!(stream as DeriveInput);
    parse::expand_derive_parse(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}


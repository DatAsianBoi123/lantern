use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, Data, DataStruct, DeriveInput, Field, Fields, FieldsNamed, FieldsUnnamed};

#[proc_macro_derive(Parse)]
pub fn derive_parse(stream: TokenStream) -> TokenStream {
    let DeriveInput { ident, data, .. } = parse_macro_input!(stream as DeriveInput);

    match data {
        Data::Struct(DataStruct { fields: Fields::Unit, .. }) => {
            quote! {
                impl crate::Parse<crate::lex::TokenStree> for #ident {
                    fn parse<I>(_: &mut ::std::iter::Peekable<I>) -> crate::Result<Self>
                    where I: ::std::iter::Iterator<Item = crate::lex::TokenTree> + ::std::clone::Clone
                    {
                        Ok(Self)
                    }
                }
            }.into()
        },
        Data::Struct(DataStruct { fields: Fields::Named(FieldsNamed { named, .. }), .. }) => {
            let names = named.iter()
                .map(|Field { ident, .. }| ident.as_ref().expect("named field"));
            let assignments = named.iter()
                .map(|Field { ident, ty, .. }| {
                    let ident = ident.as_ref().expect("named field");
                    quote! { let #ident = <#ty as crate::Parse<crate::lex::TokenTree>>::parse(iter)?; }
                });

            quote! {
                impl crate::Parse<crate::lex::TokenTree> for #ident {
                    fn parse<I>(iter: &mut ::std::iter::Peekable<I>) -> crate::Result<Self>
                    where I: ::std::iter::Iterator<Item = crate::lex::TokenTree> + ::std::clone::Clone
                    {
                        #(#assignments)*
                        Ok(Self { #(#names),* })
                    }
                }
                impl crate::Parse<crate::lex::TokenTree> for ::std::vec::Vec<#ident> {
                    fn parse<I>(iter: &mut ::std::iter::Peekable<I>) -> crate::Result<Self>
                    where I: ::std::iter::Iterator<Item = crate::lex::TokenTree> + ::std::clone::Clone
                    {
                        let mut vec = ::std::vec::Vec::new();
                        while let Ok(token) = <#ident as crate::Parse<crate::lex::TokenTree>>::parse(&mut iter.clone()) {
                            let _ = <#ident as crate::Parse<crate::lex::TokenTree>>::parse(iter);
                            vec.push(token);
                        }
                        Ok(vec)
                    }
                }
            }.into()
        },
        Data::Struct(DataStruct { fields: Fields::Unnamed(FieldsUnnamed { unnamed, .. }),  .. }) => {
            syn::Error::new(unnamed.span(), "unnamed struct fields are not supported").into_compile_error().into()
        },
        Data::Enum(enum_data) => syn::Error::new(enum_data.enum_token.span, "enums are not supported").into_compile_error().into(),
        Data::Union(union_data) => syn::Error::new(union_data.union_token.span, "unions are not supported").into_compile_error().into(),
    }
}


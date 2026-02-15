use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DataEnum, DataStruct, DeriveInput, Expr, Field, Fields, FieldsNamed, FieldsUnnamed, LitStr, Variant, parse_macro_input};

#[proc_macro_derive(Parse, attributes(parse))]
pub fn derive_parse(stream: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(stream as DeriveInput);

    gen_full(derive_input)
}

fn gen_full(DeriveInput { ident, data, .. }: DeriveInput) -> TokenStream {
    match data {
        Data::Struct(DataStruct { fields, .. }) => {
            let can_parse = fields.iter().next()
                .map(|Field { ty, .. }| quote!(<#ty as crate::ParseTokens>::can_parse(peek)))
                .unwrap_or(quote!(true));
            let r#impl = impl_for_fields(&fields, quote! { Self });

            quote! {
                impl crate::ParseTokens for #ident {
                    fn parse(stream: &mut crate::stream::TokenStream) -> crate::Result<Self> {
                        #r#impl
                    }

                    fn can_parse(peek: &::lex::Token) -> bool {
                        #can_parse
                    }
                }
            }.into()
        },
        Data::Enum(DataEnum { variants, .. }) => {
            let tries = variants.iter()
                .map(|Variant { ident, fields, .. }| {
                    let can_parse = fields.iter().next()
                        .map(|Field { ty, .. }| quote!(<#ty as crate::ParseTokens>::can_parse(peek)))
                        .unwrap_or(quote!(true));
                    let r#impl = impl_for_fields(fields, quote! { Self::#ident });
                    quote! {
                        if #can_parse {
                            return #r#impl;
                        }
                    }
                });
            let can_parse = variants.iter()
                .map(|Variant { fields, .. }| {
                    fields.iter().next()
                        .map(|Field { ty, .. }| quote!(<#ty as crate::ParseTokens>::can_parse(peek)))
                        .unwrap_or(quote!(true))
                });
            let ident_name = LitStr::new(&ident.to_string(), ident.span());

            quote! {
                impl crate::ParseTokens for #ident {
                    fn parse(stream: &mut crate::stream::TokenStream) -> crate::Result<Self> {
                        let peek = stream.peek()?;
                        #(#tries)*
                        let span = peek.span();
                        Err(::diagnostic::error!(span => "expected `{}`", #ident_name))
                    }

                    fn can_parse(peek: &::lex::Token) -> bool {
                        #(#can_parse ||)* false
                    }
                }
            }.into()
        },
        Data::Union(union_data) => syn::Error::new(union_data.union_token.span, "unions are not supported").into_compile_error().into(),
    }
}

fn impl_for_fields(fields: &Fields, this: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    match fields {
        Fields::Unit => {
            quote! { Ok::<_, ::diagnostic::Diagnostic>(#this) }
        },
        Fields::Named(FieldsNamed { named, .. }) => {
            let assignments = named.iter()
                .map(|Field { attrs, ident, ty, .. }| {
                    let ident = ident.as_ref().expect("named field");

                    if let Some(parse) = attrs.iter().find(|attr| attr.path().is_ident("parse")) {
                        parse.parse_args::<Expr>()
                            .map(|parse| quote!(#ident: #parse))
                            .unwrap_or_else(|err| err.to_compile_error())
                    } else {
                        quote!(#ident: <#ty as crate::ParseTokens>::parse(stream)?)
                    }
                });

            quote! {
                Ok::<_, ::diagnostic::Diagnostic>(#this { #(#assignments),* })
            }
        },
        Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
            let args = unnamed.iter()
                .map(|Field { attrs, ty, .. }| {
                    if let Some(parse) = attrs.iter().find(|attr| attr.path().is_ident("parse")) {
                        parse.parse_args::<Expr>()
                            .map(|parse| quote!(#parse))
                            .unwrap_or_else(|err| err.to_compile_error())
                    } else {
                        quote!(<#ty as crate::ParseTokens>::parse(stream)?)
                    }
                });

            quote! { Ok::<_, ::diagnostic::Diagnostic>(#this(#(#args),*)) }
        },
    }
}


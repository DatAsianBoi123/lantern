use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DataEnum, DataStruct, DeriveInput, Field, Fields, FieldsNamed, FieldsUnnamed, Type, Variant};

#[proc_macro_derive(Parse, attributes(from))]
pub fn derive_parse(stream: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(stream as DeriveInput);

    match derive_input.attrs.iter().find(|attr| attr.path().is_ident("from")) {
        Some(parse_attr) => {
            match parse_attr.parse_args() {
                Ok(from) => gen_from(derive_input, from),
                Err(err) => err.to_compile_error().into(),
            }
        },
        None => gen_full(derive_input),
    }
}

fn gen_from(DeriveInput { ident, .. }: DeriveInput, from: Type) -> TokenStream {
    quote! {
        impl crate::ParseTokens for #ident {
            fn parse(stream: &mut crate::stream::StreamBranch) -> crate::Result<Self> {
                <#from as crate::ParseTokens>::parse(stream).map(::std::convert::Into::into)
            }
        }
    }.into()
}

fn gen_full(DeriveInput { ident, data, .. }: DeriveInput) -> TokenStream {
    match data {
        Data::Struct(DataStruct { fields, .. }) => {
            let r#impl = impl_for_fields(&fields, quote! { Self });

            quote! {
                impl crate::ParseTokens for #ident {
                    fn parse(stream: &mut crate::stream::StreamBranch) -> crate::Result<Self> {
                        #r#impl
                    }
                }
            }.into()
        },
        Data::Enum(DataEnum { variants, .. }) => {
            let tries = variants.iter()
                .map(|Variant { ident, fields, .. }| {
                    let r#impl = impl_for_fields(fields, quote! { Self::#ident });
                    quote! {
                        match (|| { #r#impl })() {
                            Ok(variant) => return Ok(variant),
                            Err(err) if stream.cursor() == farthest.cursor() => {
                                diagnostics.extend(err);
                            },
                            Err(err) if stream.cursor() > farthest.cursor() => {
                                farthest = stream.location();
                                diagnostics = err;
                            },
                            Err(err) => {},
                        }
                        stream.goto(mark.clone());
                    }
                });

            quote! {
                impl crate::ParseTokens for #ident {
                    fn parse(stream: &mut crate::stream::StreamBranch) -> crate::Result<Self> {
                        let mut diagnostics = crate::error::Diagnostics::default();
                        let mut farthest = stream.location();
                        let mut mark = stream.location();
                        #(#tries)*
                        stream.goto(farthest);

                        Err(diagnostics)
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
            quote! { Ok::<_, crate::error::Diagnostics>(#this) }
        },
        Fields::Named(FieldsNamed { named, .. }) => {
            let names = named.iter()
                .map(|Field { ident, .. }| ident.as_ref().expect("named field"));
            let assignments = named.iter()
                .map(|Field { ident, ty, .. }| {
                    let ident = ident.as_ref().expect("named field");
                    quote! { let #ident = <#ty as crate::ParseTokens>::parse(stream)?; }
                });

            quote! {
                #(#assignments)*
                Ok::<_, crate::error::Diagnostics>(#this { #(#names),* })
            }
        },
        Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
            let args = unnamed.iter()
                .map(|Field { ty, .. }| quote! { <#ty as crate::ParseTokens>::parse(stream)? });

            quote! { Ok::<_, crate::error::Diagnostics>(#this(#(#args),*)) }
        },
    }
}


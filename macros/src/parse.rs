use proc_macro2::TokenStream;
use quote::quote;
use syn::{Attribute, Data, DataEnum, DataStruct, DeriveInput, Expr, Field, Fields, LitStr, Token, Type, Variant, parenthesized, punctuated::Punctuated, token::{Comma, Paren}};

pub fn expand_derive_parse(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let DeriveInput { ident, data, .. } = input;

    match data {
        Data::Struct(DataStruct { fields, .. }) => {
            let parse_fields = expand_for_fields(&fields);
            Ok(quote! {
                impl crate::ParseTokens for #ident {
                    fn parse(stream: &mut crate::stream::TokenStream) -> crate::Result<Self> {
                        Ok(Self {
                            #(#parse_fields),*
                        })
                    }
                }
            })
        }
        Data::Enum(DataEnum { variants, .. }) => {
            let variant_checks = variants.iter()
                .take(variants.len() - 1)
                .map(|variant @ Variant { attrs, ident, fields, .. }| {
                    let VariantAttributes { using } = match VariantAttributes::from_attrs(attrs) {
                        Ok(attrs) => attrs,
                        Err(err) => return err.to_compile_error(),
                    };

                    let parse_fields = expand_for_fields(fields);
                    match using {
                        Some(using) => {
                            let using_types = using.into_iter();
                            quote! {
                                if false #(|| <#using_types as ::lex::TokenKind>::is_token(peek))* {
                                    return Ok(Self::#ident { #(#parse_fields),* });
                                }
                            }
                        }
                        None => {
                            let Some(first) = fields.iter().next() else {
                                return syn::Error::new_spanned(variant, "missing `using` attribute").into_compile_error();
                            };
                            quote! {
                                if <#first as ::lex::TokenKind>::is_token(peek) {
                                    return Ok(Self::#ident { #(#parse_fields),* })
                                }
                            }
                        },
                    }
                });
            let ident_name = LitStr::new(&ident.to_string(), ident.span());
            let else_variant = match variants.last() {
                Some(Variant { attrs, ident, fields, .. }) => {
                    match VariantAttributes::from_attrs(attrs) {
                        Ok(VariantAttributes { using: Some(using) }) => {
                            let using_types = using.into_iter();
                            let parse_fields = expand_for_fields(fields);
                            quote! {
                                if false #(|| <#using_types as ::lex::TokenKind>::is_token(peek))* {
                                    return Ok(Self::#ident { #(#parse_fields),* });
                                }
                                Err(::diagnostic::error!(peek.span() => "expected `{}`", #ident_name))
                            }
                        }
                        Ok(_) => {
                            let parse_fields = expand_for_fields(fields);
                            quote! {
                                Ok(Self::#ident { #(#parse_fields),* })
                            }
                        }
                        Err(err) => err.to_compile_error(),
                    }
                }
                None => quote! {
                    Err(::diagnostic::error!(peek.span() => "expected `{}`", #ident_name))
                },
            };

            Ok(quote! {
                impl crate::ParseTokens for #ident {
                    fn parse(stream: &mut crate::stream::TokenStream) -> crate::Result<Self> {
                        let peek = stream.peek()?;
                        #(#variant_checks)*
                        #else_variant
                    }
                }
            })
        }
        Data::Union(union) => Err(syn::Error::new(union.union_token.span, "unions are not supported")),
    }
}

fn expand_for_fields(fields: &Fields) -> impl Iterator<Item = TokenStream> {
    fields.iter().zip(fields.members())
        .map(|(Field { attrs, ty, .. }, member)| {
            match FieldAttributes::from_attrs(attrs) {
                Ok(FieldAttributes { with: Some(expr), .. }) => quote! {
                    #member: #expr(stream)?
                },
                Ok(FieldAttributes { with_try: Some((left, right)), boxed, .. }) => {
                    let right_side = if boxed.is_some() {
                        quote!(::std::boxed::Box::new(<#right as crate::ParseTokens>::parse(stream)?))
                    } else {
                        quote!(<#right as crate::ParseTokens>::parse(stream)?)
                    };
                    quote! {
                        #member: if <#left as ::lex::TokenKind>::is_token(stream.peek()?) {
                            Some((<#left as crate::ParseTokens>::parse(stream)?, #right_side))
                        } else {
                            None
                        }
                    }
                }
                Ok(FieldAttributes { boxed: Some(Some(ty)), .. }) => quote! {
                    #member: ::std::boxed::Box::new(<#ty as crate::ParseTokens>::parse(stream)?)
                },
                Ok(_) => quote! {
                    #member: <#ty as crate::ParseTokens>::parse(stream)?
                },
                Err(err) => err.into_compile_error(),
            }
        })
}

#[derive(Clone)]
struct FieldAttributes {
    with: Option<Expr>,
    with_try: Option<(Type, Type)>,
    boxed: Option<Option<Type>>,
}

impl FieldAttributes {
    fn from_attrs(attrs: &[Attribute]) -> syn::Result<Self> {
        let mut with = None;
        let mut with_try = None;
        let mut boxed = None;

        for attr in attrs {
            if !attr.path().is_ident("parse") {
                continue;
            }

            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("with") {
                    let block;
                    parenthesized!(block in meta.input);
                    with = Some(block.parse()?);
                    Ok(())
                } else if meta.path.is_ident("with_try") {
                    let types;
                    parenthesized!(types in meta.input);
                    let first = types.parse()?;
                    types.parse::<Token![,]>()?;
                    let second = types.parse()?;
                    if !types.is_empty() {
                        Err(types.error("unexpected token"))
                    } else {
                        with_try = Some((first, second));
                        Ok(())
                    }
                } else if meta.path.is_ident("boxed") {
                    if meta.input.peek(Paren) {
                        let r#type;
                        parenthesized!(r#type in meta.input);
                        boxed = Some(Some(r#type.parse()?));
                    } else {
                        boxed = Some(None);
                    }
                    Ok(())
                } else {
                    Err(meta.error("unsupported parse property"))
                }
            })?;
        }

        Ok(Self { with, with_try, boxed })
    }
}

#[derive(Clone)]
pub struct VariantAttributes {
    using: Option<Punctuated<Type, Comma>>,
}

impl VariantAttributes {
    fn from_attrs(attrs: &[Attribute]) -> syn::Result<Self> {
        let mut using = None;

        for attr in attrs {
            if !attr.path().is_ident("parse") {
                continue;
            }

            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("using") {
                    let types;
                    parenthesized!(types in meta.input);
                    using = Some(Punctuated::parse_terminated(&types)?);
                    Ok(())
                } else {
                    Err(meta.error("unsupported parse property"))
                }
            })?;
        }

        Ok(Self { using })
    }
}


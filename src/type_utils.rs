use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{
    punctuated::Punctuated,
    token::{Gt, Lt},
    AngleBracketedGenericArguments, Path, PathArguments, PathSegment, Type, TypePath,
};

pub fn optional(typ: Type) -> Type {
    let tokens: TokenStream = quote! {
        Option<#typ>
    }
    .into();
    syn::parse(tokens).unwrap()
}

pub fn vec_of(typ: Type) -> Type {
    let tokens: TokenStream = quote! {
        Vec<#typ>
    }
    .into();
    syn::parse(tokens).unwrap()
}

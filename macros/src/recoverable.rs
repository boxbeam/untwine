use proc_macro2::TokenStream;
use quote::quote;
use syn::{DataEnum, DataStruct, Fields, GenericParam, Generics, Ident};

pub fn recover_enum(
    data: &DataEnum,
    name: &Ident,
    generics: &Generics,
) -> syn::Result<TokenStream> {
    let variant = data.variants.iter().find(|variant| {
        variant.attrs.iter().any(|attr| {
            attr.meta
                .path()
                .get_ident()
                .is_some_and(|i| i.to_string() == "recover")
        })
    });
    let Some(variant) = variant else {
        return Err(syn::Error::new_spanned(
            variant,
            "Enum has no #[recover] variant",
        ));
    };

    let variant_name = &variant.ident;
    let value = build_recovered_value(&variant.fields);
    let original_generics = generics;
    let generics = generic_bounds(generics);

    Ok(quote! {
        impl #original_generics untwine::Recoverable for #name #generics {
            fn error_value(range: std::ops::Range<usize>) -> Self {
                Self::#variant_name #value
            }
        }
    })
}

pub fn recover_struct(data: &DataStruct, name: &Ident, generics: &Generics) -> TokenStream {
    let value = build_recovered_value(&data.fields);
    let original_generics = generics;
    let generics = generic_bounds(generics);
    quote! {
        impl #original_generics untwine::Recoverable for #name #generics {
            fn error_value(range: std::ops::Range<usize>) -> Self {
                Self #value
            }
        }
    }
}

fn generic_bounds(generics: &Generics) -> TokenStream {
    let generics = generics.clone();
    let params = generics.params.clone();
    let mut where_clause = vec![];
    for generic in params {
        if let GenericParam::Type(t) = generic {
            let name = &t.ident;
            where_clause.push(quote! {
                #name: untwine::Recoverable
            });
        }
    }
    let where_clause = (!where_clause.is_empty()).then(|| quote! { where #(#where_clause),* });
    quote! {
        #generics #where_clause
    }
}

fn build_recovered_value(data: &Fields) -> TokenStream {
    match data {
        syn::Fields::Named(fields) => {
            let mut names = vec![];

            for field in &fields.named {
                names.push(field.ident.clone());
            }

            quote! {
                {
                    #(
                        #names: untwine::Recoverable::error_value(range.clone())
                    ),*
                }
            }
        }
        syn::Fields::Unnamed(fields) => {
            let mut default_fields = vec![];
            for _ in 0..fields.unnamed.len() {
                default_fields.push(quote! { untwine::Recoverable::error_value(range.clone()) });
            }

            quote! {
                ( #(#default_fields),* )
            }
        }
        syn::Fields::Unit => quote! {},
    }
}

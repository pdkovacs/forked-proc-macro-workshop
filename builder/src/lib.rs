use proc_macro::{TokenStream};
use proc_macro2::{Span};
use syn::{DeriveInput,parse_macro_input,Ident};
use quote::*;

#[proc_macro_derive(Builder)]
pub fn derive_builder(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let builder_ident = Ident::new(&format!("{}Builder", name), Span::call_site());

    let expanded = quote! {

        // struct #builder_ident;

        impl #impl_generics builder_trait::Builder for #name #ty_generics #where_clause {

            type BuilderType = (); // #builder_ident;

            fn builder() {}
        }
    };

    return TokenStream::from(expanded);
}

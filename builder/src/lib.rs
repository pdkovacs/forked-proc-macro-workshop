use proc_macro::{TokenStream};
use proc_macro2::{Span};
use syn::{DeriveInput,parse_macro_input,Ident,Data,Fields};
use quote::*;

#[proc_macro_derive(Builder)]
pub fn derive_builder(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let builder_ident = Ident::new(&format!("{}Builder", name), Span::call_site());

    let builder_field_definitions = create_builder_field_definitions(&input.data);
    let initial_builder_fields = create_initial_builder_fields(&input.data);

    let expanded = quote! {

        pub struct #builder_ident {
            #builder_field_definitions
        }

        impl #impl_generics builder_trait::Builder for #name #ty_generics #where_clause {

            type BuilderType = #builder_ident;

            fn builder() -> #builder_ident {
                #builder_ident {
                    #initial_builder_fields
                }
            }
        }
    };

    return TokenStream::from(expanded);
}

fn create_builder_field_definitions(data: &Data) -> proc_macro2::TokenStream {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    let recurse = fields.named.iter().map(|f| {
                        let name = &f.ident;
                        let ty = &f.ty;
                        quote! {
                            #name: Option<#ty>
                        }
                    });
                    quote! {
                        #(#recurse),*
                    }
                }
                Fields::Unnamed(_) | Fields::Unit => unimplemented!()
            }
        }
        Data::Enum(_) | Data::Union(_) => unimplemented!()
    }
}

fn create_initial_builder_fields(data: &Data) -> proc_macro2::TokenStream {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    let recurse = fields.named.iter().map(|f| {
                        let name = &f.ident;
                        quote! {
                            #name: None
                        }
                    });
                    quote! {
                        #(#recurse),*
                    }
                }
                Fields::Unnamed(_) | Fields::Unit => unimplemented!()
            }
        }
        Data::Enum(_) | Data::Union(_) => unimplemented!()
    }
}

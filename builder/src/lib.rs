use proc_macro::{TokenStream};
use proc_macro2::{Span};
use syn::{DeriveInput,parse_macro_input,Ident,Data,Fields};
use quote::*;

#[proc_macro_derive(Builder)]
pub fn derive_builder(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let to_be_built_type = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let builder_ident = Ident::new(&format!("{}Builder", to_be_built_type), Span::call_site());

    let builder_field_definitions = create_builder_field_definitions(&input.data);
    let initial_builder_fields = create_initial_builder_fields(&input.data);
    let builder_setter_methods = create_builder_setter_methods(&input.data);
    let build_assignments = create_build_assignments(&input.data);

    let expanded = quote! {
        use std::error::Error;

        pub struct #builder_ident {
            #builder_field_definitions
        }

        impl #builder_ident {
            #builder_setter_methods
        }

        impl #builder_ident {
            pub fn build(&mut self) -> Result<#to_be_built_type, Box<dyn std::error::Error>> {
                Ok(Command{
                    #build_assignments
                })
            }
        }    

        impl #impl_generics builder_trait::Builder for #to_be_built_type #ty_generics #where_clause {

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
                        let field_ident = &f.ident;
                        quote! {
                            #field_ident: None
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

fn create_builder_setter_methods(data: &Data) -> proc_macro2::TokenStream {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    let recurse = fields.named.iter().map(|f| {
                        let field_ident = &f.ident;
                        let field_type = &f.ty;
                        quote! {
                            fn #field_ident(&mut self, #field_ident: #field_type) -> &mut Self {
                                self.#field_ident = Some(#field_ident);
                                self
                            }
                        }
                    });
                    quote! {
                        #(#recurse)*
                    }
                }
                Fields::Unnamed(_) | Fields::Unit => unimplemented!()
            }
        }
        Data::Enum(_) | Data::Union(_) => unimplemented!()
    }
}

fn create_build_assignments(data: &Data) -> proc_macro2::TokenStream {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    let recurse = fields.named.iter().map(|f| {
                        let field_ident = &f.ident;
                        let field_name = stringify!(field_ident);
                        quote! {
                            #field_ident: self.#field_ident.take().ok_or_else(|| Box::<dyn Error>::from(format!("{} isn't set", #field_name)))?,
                        }
                    });
                    quote! {
                        #(#recurse)*
                    }
                }
                Fields::Unnamed(_) | Fields::Unit => unimplemented!()
            }
        }
        Data::Enum(_) | Data::Union(_) => unimplemented!()
    }
}

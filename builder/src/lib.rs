use proc_macro::{TokenStream};
use proc_macro2::{Span};
use syn::{DeriveInput,parse_macro_input,Ident,Data,Fields, Type, TypePath, Path, PathArguments, AngleBracketedGenericArguments, GenericArgument};
use quote::*;
use std::error::Error;
use syn::spanned::Spanned;

struct BuilderField {
    ident: syn::Ident,
    ty: syn::Type,
}

struct ErrorWithSpan {
    error: Box<dyn Error>,
    span: proc_macro2::Span
}

fn handle_error_with_span(err_with_span: ErrorWithSpan) -> TokenStream {
    let err_message = format!("{}", err_with_span.error);
    let tokstr2 = quote_spanned! {
        err_with_span.span =>
        compile_error!(#err_message);
    };
    TokenStream::from(tokstr2)
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive_builder(input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);

    let to_be_built_type = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let builder_ident = Ident::new(&format!("{}Builder", to_be_built_type), Span::call_site());

    let field_definitions = match get_field_definitions(&mut input.data)  {
        Ok(field_defs) => field_defs,
        Err(err) => return handle_error_with_span(err),
    };

    let builder_field_definitions = create_builder_field_definitions(&field_definitions);
    let initial_builder_fields = create_initial_builder_fields(&field_definitions);
    let builder_setter_methods = create_builder_setter_methods(&field_definitions);
    let build_assignments = create_build_assignments(&field_definitions);

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

fn get_field_definitions(data: &mut Data) -> Result<Vec<BuilderField>, ErrorWithSpan> {
    match *data {
        Data::Struct(ref mut data) => {
            match data.fields {
                Fields::Named(ref mut fields) => {
                    let mut field_list: Vec<BuilderField> = Vec::new();
                    for field in &mut fields.named {
                        let ident: syn::Ident = field.ident.take().ok_or_else(|| ErrorWithSpan {
                            error: Box::<dyn Error>::from("field without ident"),
                            span: field.span()
                        })?;
                        let ty: syn::Type = field.ty.clone();
                        field_list.push(BuilderField {
                            ident,
                            ty,
                        });
                    }
                    return Ok(field_list);
                }
                Fields::Unnamed(_) | Fields::Unit => unimplemented!()
            }
        }
        Data::Enum(_) | Data::Union(_) => unimplemented!()
    }
}

fn create_builder_field_definitions(fdefs: &Vec<BuilderField>) -> proc_macro2::TokenStream {
    let recurse = fdefs.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if let Some(_) = unwrap_optional(ty) {
            quote! {
                #name: #ty
            }
        } else {
            quote! {
                #name: Option<#ty>
            }
        }
    });
    quote! {
        #(#recurse),*
    }
}

fn create_initial_builder_fields(fdefs: &Vec<BuilderField>) -> proc_macro2::TokenStream {
    let recurse = fdefs.iter().map(|f| {
        let field_ident = &f.ident;
        quote! {
            #field_ident: None
        }
    });
    quote! {
        #(#recurse),*
    }
}

fn create_builder_setter_methods(fdefs: &Vec<BuilderField>) -> proc_macro2::TokenStream {
    let recurse = fdefs.iter().map(|f| {
        let field_ident = &f.ident;
        let field_type = &f.ty;
        let opt_unwrapped_optional = unwrap_optional(field_type);
        if let Some(unwrapped_optional) = opt_unwrapped_optional {
            quote! {
                fn #field_ident(&mut self, #field_ident: #unwrapped_optional) -> &mut Self {
                    self.#field_ident = Some(#field_ident);
                    self
                }
            }
        } else {
            quote! {
                fn #field_ident(&mut self, #field_ident: #field_type) -> &mut Self {
                    self.#field_ident = Some(#field_ident);
                    self
                }
            }
        }
    });
    quote! {
        #(#recurse)*
    }
}

fn create_build_assignments(fdefs: &Vec<BuilderField>) -> proc_macro2::TokenStream {
    let recurse = fdefs.iter().map(|f| {
        let field_ident = &f.ident;
        let field_name = stringify!(field_ident);
        if let Some(_) = unwrap_optional(&f.ty) {
            quote! {
                #field_ident: match self.#field_ident {
                    Some(_) => self.#field_ident.take(),
                    None => None
                }
            }
        } else {
            quote! {
                #field_ident: self.#field_ident.take().ok_or_else(|| Box::<dyn Error>::from(format!("{} isn't set", #field_name)))?,
            }    
        }
    });
    quote! {
        #(#recurse)*
    }
}

fn unwrap_optional(ty: &Type) -> Option<&Type> {
    if let Type::Path(TypePath{
        qself: None,
        path: Path{
            leading_colon: _,
            segments: path_segments
        }
    }) = ty {
        for item in path_segments {
            if item.ident.to_string() == String::from("Option") {
                if let PathArguments::AngleBracketed(
                    AngleBracketedGenericArguments {
                        colon2_token: _,
                        lt_token: _,
                        args: generic_args,
                        gt_token: _
                    },
                ) = &item.arguments {
                    for arg in generic_args {
                        if let GenericArgument::Type(
                            wrapped_type
                        ) = arg {
                            return Some(wrapped_type);
                        }
                    }
                }
            }
        }
    }
    None
}

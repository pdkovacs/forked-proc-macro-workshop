use proc_macro::{TokenStream};
use proc_macro2::{Span};
use syn::{DeriveInput,parse_macro_input,Ident,Data,Fields, Type, TypePath, Path, PathArguments, AngleBracketedGenericArguments, GenericArgument};
use quote::*;
use std::error::Error;
use syn::spanned::Spanned;

struct SetterMethod {
    name: String,
    param_type: Type,
}

struct BuilderField {
    ident: syn::Ident,
    ty: syn::Type,
    custom_setter_method: Option<SetterMethod>,
}

struct ErrorWithSpan {
    error: Box<dyn Error>,
    span: proc_macro2::Span,
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

fn parse_builder_attribute(field_type: &Type, list_attrib: &syn::NestedMeta) -> Result<Option<SetterMethod>, ErrorWithSpan> {
    if let syn::NestedMeta::Meta(nested_meta) = &list_attrib {
        if let syn::Meta::NameValue(name_value) = nested_meta {
            if let syn::Lit::Str(single_item_method_name) = &name_value.lit {
                if name_value.path.segments[0].ident == "each" {
                    let method_name = single_item_method_name.value();
                    return Ok(Some(SetterMethod{ name: method_name, param_type: unwrap_type(field_type, &String::from("Vec")).unwrap().clone() }));
                }
            }
        }
    }
    return Err(ErrorWithSpan {
        error: Box::<dyn Error>::from("invalid builder attribute"),
        span:  list_attrib.span(),
    })
}

fn get_builder_method(field: &syn::Field) -> Result<Option<SetterMethod>, ErrorWithSpan> {
    let attrs = field.attrs.clone();
    for attr in attrs.iter() {
        if let Ok(meta) = attr.parse_meta() {
            if let syn::Meta::List(list_attrib) = meta {
                if list_attrib.path.segments[0].ident.to_string() == "builder" {
                    return parse_builder_attribute(&field.ty, &list_attrib.nested[0]);
                }
            }
        }
    }
    return Ok(None);
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
                            custom_setter_method: get_builder_method(field)?,
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
        let field_name = &f.ident;

        let ty = &f.ty;
        if let Some(_) = unwrap_optional(ty) {
            quote! {
                #field_name: #ty
            }
        } else if let Some(_) = unwrap_type(ty, &String::from("Vec")) {
            quote! {
                #field_name: #ty
            }
        } else {
            quote! {
                #field_name: Option<#ty>
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
        let vec_item_type = unwrap_type(&f.ty, &String::from("Vec"));
        if let Some(_) = vec_item_type {
            quote! {
                #field_ident: Vec::new()
            }
        } else {
            quote! {
                #field_ident: None
            }
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
        } else if let Some(ref setter_method) = f.custom_setter_method {
            let setter_name = syn::Ident::new(&setter_method.name, Span::call_site());
            let setter_param_type = &setter_method.param_type;
            quote! {
                fn #setter_name(&mut self, #field_ident: #setter_param_type) -> &mut Self {
                    self.#field_ident.push(#field_ident);
                    self
                }
            }    
        } else if let Some(_) = unwrap_type(&f.ty, &String::from("Vec")) {
            quote! {
                fn #field_ident(&mut self, #field_ident: #field_type) -> &mut Self {
                    self.#field_ident = #field_ident.clone();
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
        if let Some(_) = unwrap_optional(&f.ty) {
            quote! {
                #field_ident: match self.#field_ident {
                    Some(_) => self.#field_ident.take(),
                    None => None
                }
            }
        } else if let Some(_) = unwrap_type(&f.ty, &String::from("Vec")) {
            quote! {
                #field_ident: self.#field_ident.clone(),
            }
        } else {
            let missing_field_error_message = format!("{} isn't set", field_ident);
            quote! {
                #field_ident: self.#field_ident.take().ok_or_else(|| Box::<dyn Error>::from(#missing_field_error_message))?,
            }    
        }
    });
    quote! {
        #(#recurse)*
    }
}

fn unwrap_type<'a>(ty: &'a Type, from: &String) -> Option<&'a Type> {
    if let Type::Path(TypePath{
        qself: None,
        path: Path{
            leading_colon: _,
            segments: path_segments
        }
    }) = ty {
        for item in path_segments {
            if item.ident.to_string() == String::from(from) {
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

fn unwrap_optional(ty: &Type) -> Option<&Type> {
    return unwrap_type(ty, &String::from("Option"));
}

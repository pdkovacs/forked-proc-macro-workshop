use proc_macro::TokenStream;

trait Builder {
    type BuilderType;

    fn builder() -> Self::BuilderType; 
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let _ = input;
    return TokenStream::new();
}

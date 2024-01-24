use proc_macro::TokenStream;

#[proc_macro]
pub fn parser(input: TokenStream) -> TokenStream {
    let input = proc_macro2::TokenStream::from(input);
    input.into()
}

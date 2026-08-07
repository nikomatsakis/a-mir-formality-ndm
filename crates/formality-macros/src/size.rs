use proc_macro2::TokenStream;
use quote::quote;

pub(crate) fn derive_size(mut s: synstructure::Structure) -> TokenStream {
    s.underscore_const(true);
    s.bind_with(|_| synstructure::BindStyle::Move);

    let size_body = s.each(|field| {
        quote! {
            __sum = __sum.saturating_add(
                <_ as formality_core::Size>::size(#field)
            )
        }
    });

    s.gen_impl(quote! {
        gen impl formality_core::Size for @Self {
            fn size(&self) -> usize {
                let mut __sum = 1usize;
                match self {
                    #size_body
                }
                __sum
            }
        }
    })
}

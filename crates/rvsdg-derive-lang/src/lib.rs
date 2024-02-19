#![feature(proc_macro_diagnostic)]
//! # RVSDG Derive Lang macro
//!
//!
//! Provides two macros, that implement the `LangNode` and `LangEdge` trait for you.
//!

use proc_macro::TokenStream;
use syn::{spanned::Spanned, Data, Field, Fields, Ident};

#[proc_macro_derive(LangNode, attributes(inputs, outputs, input, output, expose))]
pub fn derive_lang_node(item: TokenStream) -> TokenStream {
    let ast = syn::parse(item).expect("failed to parse input stream");

    impl_lang_node(&ast)
}

fn impl_lang_node(ast: &syn::DeriveInput) -> TokenStream {
    let mut expose_field = None;
    let mut inputs_field = None;
    let mut outputs_field = None;

    //First try to find which fields we need to implement.

    match &ast.data {
        Data::Enum(_) | Data::Union(_) => {
            ast.span()
                .unwrap()
                .error("Cannot implement LangNode on this, only structs are supported!")
                .emit();
            return TokenStream::new();
        }
        Data::Struct(st) => {
            match &st.fields {
                Fields::Named(named) => {
                    //Iterate all fields
                    for field in &named.named {
                        //iterate the fields attributes
                        for attr in field.attrs.iter() {
                            //Check all fields for inputs/outputs and expose attribute

                            //multiple inputs
                            if attr.path().is_ident("inputs") {
                                if inputs_field.is_some() {
                                    attr.span()
                                        .unwrap()
                                        .error("There can only be one input collection")
                                        .emit();
                                    return TokenStream::new();
                                }
                                inputs_field = Some((field.ident.clone().unwrap(), true));
                            }

                            //Multiple outputs
                            if attr.path().is_ident("outputs") {
                                if outputs_field.is_some() {
                                    attr.span()
                                        .unwrap()
                                        .error("There can only be one outputs collection")
                                        .emit();
                                    return TokenStream::new();
                                }
                                outputs_field = Some((field.ident.clone().unwrap(), true));
                            }

                            if attr.path().is_ident("input") {
                                if inputs_field.is_some() {
                                    attr.span()
                                        .unwrap()
                                        .error("There can only be one input collection")
                                        .emit();
                                    return TokenStream::new();
                                }
                                inputs_field = Some((field.ident.clone().unwrap(), false));
                            }

                            //Multiple outputs
                            if attr.path().is_ident("output") {
                                if outputs_field.is_some() {
                                    attr.span()
                                        .unwrap()
                                        .error("There can only be one outputs collection")
                                        .emit();
                                    return TokenStream::new();
                                }
                                outputs_field = Some((field.ident.clone().unwrap(), false));
                            }

                            if attr.path().is_ident("expose") {
                                if expose_field.is_some() {
                                    attr.span()
                                        .unwrap()
                                        .error("There can only be one field that exposes the inner LangNode definition")
                                        .emit();
                                    return TokenStream::new();
                                }
                                expose_field = Some(field.ident.clone().unwrap());
                            }
                        }
                    }
                }
                Fields::Unnamed(unnamed) => {
                    unnamed
                        .span()
                        .unwrap()
                        .error("Deriving unnamed fields is not supported by LangNode macro!")
                        .emit();
                    return TokenStream::new();
                }
                Fields::Unit => {}
            }
        }
    }

    if expose_field.is_some() && (inputs_field.is_some() || outputs_field.is_some()) {
        ast.span().unwrap().error("You can either \"#[expose]\" a field for LangNode implementation, or mark fields as inputs and outputs. Not both!").emit();
        return TokenStream::new();
    }

    match (&inputs_field, &outputs_field) {
        (Some((inp, _is_multiple)), None) => {
            inp.span().unwrap().error("Input is defined here, but no #[outputs] field was declared. Consider adding one!").emit();
            return TokenStream::new();
        }
        (None, Some((outp, _is_multiple))) => {
            outp.span().unwrap().error("Output is defined here, but no #[inputs] field was declared. Consider adding one!").emit();
            return TokenStream::new();
        }
        _ => {}
    }

    match (expose_field, inputs_field, outputs_field) {
        (Some(exp), None, None) => impl_lang_node_expose(ast.ident.clone(), exp),
        (None, Some(inp), Some(outp)) => impl_lang_node_inout(ast.ident.clone(), inp, outp),
        _ => {
            ast.span()
                .unwrap()
                .error("Unexpected error in LangNode derive macro")
                .emit();
            TokenStream::new()
        }
    }
}

fn impl_lang_node_expose(struct_ident: Ident, expose_field: Ident) -> TokenStream {
    quote::quote!(
        impl rvsdg::nodes::LangNode for #struct_ident{
            fn inputs(&self) -> &[rvsdg::region::Input]{
                self.#expose_field.inputs()
            }
            fn inputs_mut(&mut self) -> &mut [rvsdg::region::Input]{
                self.#expose_field.inputs_mut()
            }
            fn outputs(&self) -> &[rvsdg::region::Output]{
                self.#expose_field.outputs()
            }
            fn outputs_mut(&mut self) -> &mut [rvsdg::region::Output]{
                self.#expose_field.outputs_mut()
            }
        }
    )
    .into()
}

fn impl_lang_node_inout(
    struct_ident: Ident,
    inputs: (Ident, bool),
    outputs: (Ident, bool),
) -> TokenStream {
    let (inputs, is_inputs_slice) = inputs;

    let (inputfrag, inputmutfrag) = if is_inputs_slice {
        (
            quote::quote!(self.#inputs.as_slice()),
            quote::quote!(self.#inputs.as_mut_slice()),
        )
    } else {
        (
            quote::quote!(std::slice::from_ref(&self.#inputs)),
            quote::quote!(std::slice::from_mut(&mut self.#inputs)),
        )
    };

    let (outputs, is_outputs_slice) = outputs;
    let (outputfrag, outputmutfrag) = if is_outputs_slice {
        (
            quote::quote!(self.#outputs.as_slice()),
            quote::quote!(self.#outputs.as_mut_slice()),
        )
    } else {
        (
            quote::quote!(std::slice::from_ref(&self.#outputs)),
            quote::quote!(std::slice::from_mut(&mut self.#outputs)),
        )
    };

    quote::quote!(
        impl rvsdg::nodes::LangNode for #struct_ident{
            fn inputs(&self) -> &[rvsdg::region::Input]{
                #inputfrag
            }
            fn inputs_mut(&mut self) -> &mut [rvsdg::region::Input]{
                #inputmutfrag
            }
            fn outputs(&self) -> &[rvsdg::region::Output]{
                #outputfrag
            }
            fn outputs_mut(&mut self) -> &mut [rvsdg::region::Output]{
                #outputmutfrag
            }
        }
    )
    .into()
}

#[proc_macro_derive(LangEdge, attributes(state_edge, value_edge))]
pub fn derive_lang_edge(_item: TokenStream) -> TokenStream {
    "fn yay() {println!(\"yay\");}".parse().unwrap()
}

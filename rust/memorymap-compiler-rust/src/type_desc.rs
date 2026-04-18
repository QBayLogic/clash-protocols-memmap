// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::collections::{BTreeSet, HashMap};

use memorymap_compiler::{
    ir::{
        monomorph::{MonomorphVariants, TypeRefVariant},
        types::{IrCtx, TypeDefinition, TypeDescription},
    },
    storage::Handle,
};
use proc_macro2::TokenStream;
use quote::quote;
use std::str::FromStr;

use crate::{
    IdentType, ident,
    type_reference::{TypeReferences, generate_type_ref, mono_variant_name, po2_type},
};

/// User provided annotations. Will be considered during code generation.
#[derive(Default)]
pub struct Annotations {
    pub type_annotation: HashMap<Handle<TypeRefVariant>, BTreeSet<String>>,
    pub type_imports: HashMap<Handle<TypeRefVariant>, BTreeSet<String>>,
    pub type_tags: HashMap<Handle<TypeRefVariant>, BTreeSet<String>>,
}

/// Generate code for a type description, taking into account monomorp variants.
///
/// Returns the base-name of the type, the code and any types referenced by the code.
pub fn generate_type_desc<'ir>(
    ctx: &'ir IrCtx,
    varis: &MonomorphVariants,
    anns: &Annotations,
    handle: Handle<TypeDescription>,
) -> (&'ir str, proc_macro2::TokenStream, TypeReferences) {
    let mut refs = TypeReferences {
        references: BTreeSet::new(),
    };
    let desc = &ctx.type_descs[handle];
    let type_name = &ctx.type_names[desc.name];
    let variants = if let Some(variants) = varis.variants_by_type.get(&desc.name) {
        let mut all_imports = BTreeSet::new();

        let variant_toks = variants.iter().map(|var_handle| {
            generate_type_definition(ctx, varis, *var_handle, anns, &mut refs, desc)
        });

        for var in variants {
            if let Some(imports) = anns.type_imports.get(var) {
                all_imports.extend(imports)
            }
        }

        let imports = all_imports
            .into_iter()
            .map(|s| TokenStream::from_str(s).expect("invalid TokenStream for import"))
            .map(|import| quote! { use #import; });

        quote! {
            #(#imports)*
            #(#variant_toks)*
        }
    } else {
        quote! {
            compile_error!("Type `#type_name` has no monomorph variant");
        }
    };

    (&type_name.base, variants, refs)
}

fn generate_type_definition(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    var_handle: Handle<TypeRefVariant>,
    anns: &Annotations,
    refs: &mut TypeReferences,
    desc: &TypeDescription,
) -> TokenStream {
    let variant = &varis.type_ref_variants[var_handle];
    let raw_name = &ctx.type_names[desc.name];
    let (ty_name, has_mono_args) = mono_variant_name(ctx, variant);

    let args = {
        let arg_decls = desc
            .param_names
            .handles()
            .zip(&variant.argument_mono_values)
            .filter_map(|(handle, val)| {
                if val.is_none() {
                    let name = ident(IdentType::TypeVariable, &ctx.identifiers[handle]);
                    if ctx.type_param_nats.contains(&handle) {
                        Some(quote! { const #name: u128 })
                    } else {
                        Some(quote! { #name })
                    }
                } else {
                    None
                }
            });
        if variant.argument_mono_values.iter().any(|val| val.is_none()) {
            quote! { < #(#arg_decls,)* > }
        } else {
            quote! {}
        }
    };

    let attrs = {
        let repr = generate_repr(ctx, desc);
        let naming_attr = if has_mono_args {
            quote! { #[allow(non_camel_case_types)]}
        } else {
            TokenStream::new()
        };
        let annots = if let Some(code) = anns.type_annotation.get(&var_handle) {
            code
        } else {
            &BTreeSet::new()
        };
        let annots = annots
            .iter()
            .map(|s| TokenStream::from_str(s).expect("invalid TokenStream for type annotations"))
            .map(|toks| quote! { #toks });
        quote! {
            #naming_attr
            #repr
            #(#annots)*
        }
    };

    match &desc.definition {
        TypeDefinition::DataType {
            names: _,
            constructors,
        } if constructors.len == 0 => {
            quote! {
                #attrs
                pub struct #ty_name;
            }
        }
        TypeDefinition::DataType {
            names,
            constructors,
        } if constructors.len == 1 => {
            let con = &ctx.type_constructors[constructors.start];
            let con_name = &ctx.identifiers[names.start];

            if con_name != &raw_name.base {
                panic!(
                    "Single-constructor data types are required to have the constructor name match the type name. \
                    Type name: '{type_name:?}', Constructor name: '{constructor_name:?}'",
                    type_name = raw_name.base,
                    constructor_name = con_name,
                )
            }

            if let Some(names) = con.field_names {
                let fields_toks =
                    names
                        .handles()
                        .zip(con.field_types.handles())
                        .map(|(name, ty)| {
                            let id = ident(IdentType::Variable, &ctx.identifiers[name]);
                            let ty = generate_type_ref(ctx, varis, Some(variant), refs, ty);
                            quote! { pub #id: #ty }
                        });
                quote! {
                    #attrs
                    pub struct #ty_name #args {
                        #(#fields_toks,)*
                    }
                }
            } else {
                let fields_toks = con
                    .field_types
                    .handles()
                    .map(|f| generate_type_ref(ctx, varis, Some(variant), refs, f));
                let body = if con.field_types.len == 0 {
                    quote! {}
                } else {
                    quote! { ( #(pub #fields_toks,)* ) }
                };
                quote! {
                    #attrs
                    pub struct #ty_name #args #body;
                }
            }
        }
        TypeDefinition::DataType {
            names,
            constructors,
        } => {
            let cons_toks =
                names
                    .handles()
                    .zip(constructors.handles())
                    .map(|(name_handle, con_handle)| {
                        let con_name = &ctx.identifiers[name_handle];
                        let con = &ctx.type_constructors[con_handle];
                        let id = ident(IdentType::Type, con_name);

                        if let Some(field_names) = con.field_names {
                            let fields_toks = field_names
                                .handles()
                                .zip(con.field_types.handles())
                                .map(|(name, ty)| {
                                    let name = ident(IdentType::Variable, &ctx.identifiers[name]);
                                    let ty = generate_type_ref(ctx, varis, Some(variant), refs, ty);
                                    quote! { #name: #ty }
                                });
                            quote! {
                                #id {
                                    #(#fields_toks,)*
                                }
                            }
                        } else {
                            let fields_toks = con
                                .field_types
                                .handles()
                                .map(|ty| generate_type_ref(ctx, varis, Some(variant), refs, ty));
                            if con.field_types.len == 0 {
                                quote! { #id }
                            } else {
                                quote! { #id( #(#fields_toks,)* ) }
                            }
                        }
                    });
            quote! {
                #attrs
                pub enum #ty_name #args {
                    #(#cons_toks,)*
                }
            }
        }
        TypeDefinition::Newtype {
            name: _,
            constructor: _,
        } => {
            quote! { compile_error!("TODO"); }
        }
        TypeDefinition::Builtin(_builtin_type) => {
            quote! { compile_error!("TODO"); }
        }
        TypeDefinition::Synonym(handle) => {
            let ty = generate_type_ref(ctx, varis, Some(variant), refs, *handle);

            quote! { #attrs pub type #ty_name #args = #ty; }
        }
    }
}

fn generate_repr(ctx: &IrCtx, desc: &TypeDescription) -> TokenStream {
    match &desc.definition {
        TypeDefinition::DataType {
            names: _,
            constructors,
        } if constructors.len == 0 => quote! {},
        TypeDefinition::DataType {
            names: _,
            constructors,
        } if constructors.len == 1 => quote! { #[repr(C)] },
        TypeDefinition::DataType {
            names: _,
            constructors,
        } => {
            let fieldless = constructors
                .handles()
                .map(|handle| &ctx.type_constructors[handle])
                .all(|con| con.field_types.len == 0);
            let n = po2_type(constructors.len.ilog2() as u64);
            let repr = ident(IdentType::Raw, format!("u{}", n));

            if fieldless {
                quote! { #[repr(#repr) ]}
            } else {
                quote! { #[repr(C, #repr) ]}
            }
        }
        TypeDefinition::Newtype {
            name: _,
            constructor: _,
        } => {
            quote! { #[repr(transparent)] }
        }
        TypeDefinition::Builtin(_builtin_type) => quote! {},
        TypeDefinition::Synonym(_handle) => quote! {},
    }
}

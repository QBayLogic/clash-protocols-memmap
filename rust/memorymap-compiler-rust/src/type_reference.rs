use std::collections::BTreeSet;

use memorymap_compiler::{
    input_language::TypeName,
    ir::{
        monomorph::{MonomorphVariants, TypeRefVariant},
        types::{IrCtx, TypeRef},
    },
    storage::Handle,
};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{ToTokens, quote};

use crate::{IdentType, ident};

/// Types referenced during code generation.
pub struct TypeReferences {
    pub references: BTreeSet<Handle<TypeName>>,
}

pub fn generate_type_ref(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    variant: Option<&TypeRefVariant>,
    refs: &mut TypeReferences,
    ty: Handle<TypeRef>,
) -> TokenStream {
    let handle = lookup_sub(variant, ty);
    match &ctx.type_refs[handle] {
        TypeRef::BitVector(handle) => {
            let size = &ctx.type_refs[lookup_sub(variant, *handle)];

            if let TypeRef::Nat(n) = size {
                // TODO maybe special case on n == 1 like the C backend?
                let n = n.div_ceil(8) as usize;
                let n_lit = Literal::usize_unsuffixed(n);
                quote! { [u8; #n_lit] }
            } else {
                quote! { compile_error!("BitVector with length not known after monomorphisation") }
            }
        }
        TypeRef::Unsigned(handle) => {
            let size = &ctx.type_refs[lookup_sub(variant, *handle)];

            if let TypeRef::Nat(n) = size {
                let n = po2_type(*n);
                let name = format!("u{n}");
                ident(IdentType::Raw, name).into_token_stream()
            } else {
                quote! { compile_error!("Unsigned with length not known after monomorphisation") }
            }
        }
        TypeRef::Signed(handle) => {
            let size = &ctx.type_refs[lookup_sub(variant, *handle)];

            if let TypeRef::Nat(n) = size {
                let n = po2_type(*n);
                let name = format!("i{n}");
                ident(IdentType::Raw, name).into_token_stream()
            } else {
                quote! { compile_error!("Signed with length not known after monomorphisation") }
            }
        }
        TypeRef::Index(handle) => {
            let size = &ctx.type_refs[lookup_sub(variant, *handle)];

            if let TypeRef::Nat(n) = size {
                let n = po2_type(n.ilog2() as u64);
                let name = format!("u{n}");
                ident(IdentType::Raw, name).into_token_stream()
            } else {
                quote! { compile_error!("Index with length not known after monomorphisation") }
            }
        }
        TypeRef::Bool => quote! { bool },
        TypeRef::Float => quote! { f32 },
        TypeRef::Double => quote! { f64 },
        TypeRef::Vector(len, inner) => {
            let len = generate_type_ref(ctx, varis, variant, refs, *len);
            let inner = generate_type_ref(ctx, varis, variant, refs, *inner);
            quote! { [#inner; #len] }
        }
        TypeRef::Tuple(handle_range) => {
            let tys = handle_range
                .handles()
                .map(|ty| generate_type_ref(ctx, varis, variant, refs, ty));
            quote! {
                ( #(#tys,)*)
            }
        }
        TypeRef::Variable(handle) => {
            let name = &ctx.identifiers[*handle];
            let name_ident = ident(IdentType::TypeVariable, name);
            quote! { #name_ident }
        }
        TypeRef::Nat(n) => {
            let n = Literal::u64_unsuffixed(*n);
            quote! { #n }
        }
        TypeRef::Reference { name, args } => {
            refs.references.insert(*name);
            let mono_ref = if let Some(var) = variant {
                var.monomorph_type_ref_substitutions.get(&handle)
            } else {
                None
            };
            let mono_ref = mono_ref.or_else(|| varis.type_refs.get(&handle));

            if let Some(mono_ref) = mono_ref.copied() {
                let mono_variant = &varis.type_ref_variants[mono_ref];
                let (name, _has_mono_arg) = mono_variant_name(ctx, mono_variant);
                let any_args = mono_variant
                    .argument_mono_values
                    .iter()
                    .any(|s| s.is_none());
                let args = args
                    .handles()
                    .zip(&mono_variant.argument_mono_values)
                    .filter_map(|(arg, arg_val)| {
                        if let Some(_val) = arg_val {
                            None
                        } else {
                            Some(arg)
                        }
                    })
                    .map(|arg| generate_type_ref(ctx, varis, variant, refs, arg));
                let args = if any_args {
                    quote! { < #(#args, )* > }
                } else {
                    quote! {}
                };
                quote! {
                    #name #args
                }
            } else {
                let ty_ident = ident(IdentType::Type, &ctx.type_names[*name].base);
                let args = if args.len > 0 {
                    let args = args
                        .handles()
                        .map(|arg| generate_type_ref(ctx, varis, variant, refs, arg));
                    quote! {
                        < #(#args,)* >
                    }
                } else {
                    quote! {}
                };

                quote! {
                    #ty_ident #args
                }
            }
        }
    }
}

pub fn po2_type(n: u64) -> u64 {
    let n = n.max(8);
    if n.is_power_of_two() {
        n
    } else {
        n.next_power_of_two()
    }
}

pub fn mono_variant_name(ctx: &IrCtx, var: &TypeRefVariant) -> (Ident, bool) {
    let desc = &ctx.type_descs[var.original_type_desc];
    let ty_name = &ctx.type_names[desc.name];
    let args = var
        .argument_mono_values
        .iter()
        .map(|arg| arg.map(|val| type_to_ident(ctx, val)));

    let has_mono_args = args.clone().any(|arg| arg.is_some());

    let mut arg_names = String::new();
    if has_mono_args {
        for arg in args.flatten() {
            arg_names.push('_');
            arg_names.push_str(&arg);
        }
    }

    let type_name_base = ident(IdentType::Type, &ty_name.base);
    (
        ident(IdentType::Raw, format!("{type_name_base}{arg_names}")),
        has_mono_args,
    )
}

fn type_to_ident(ctx: &IrCtx, ty: Handle<TypeRef>) -> String {
    match &ctx.type_refs[ty] {
        TypeRef::BitVector(handle) => {
            let len = type_to_ident(ctx, *handle);
            format!("bv{len}")
        }
        TypeRef::Unsigned(handle) => {
            let len = type_to_ident(ctx, *handle);
            format!("u{len}")
        }
        TypeRef::Signed(handle) => {
            let len = type_to_ident(ctx, *handle);
            format!("s{len}")
        }
        TypeRef::Index(handle) => {
            let len = type_to_ident(ctx, *handle);
            format!("i{len}")
        }
        TypeRef::Bool => "bool".to_string(),
        TypeRef::Float => "f32".to_string(),
        TypeRef::Double => "f64".to_string(),
        TypeRef::Vector(size_handle, inner) => {
            let size = type_to_ident(ctx, *size_handle);
            let inner = type_to_ident(ctx, *inner);
            format!("vec_{size}_{inner}")
        }
        TypeRef::Tuple(handle_range) if handle_range.len == 0 => "unit".to_string(),
        TypeRef::Tuple(handle_range) => {
            let n = handle_range.len;
            let mut name = format!("tuple{n}");
            for handle in handle_range.handles() {
                let add = type_to_ident(ctx, handle);
                if !add.is_empty() {
                    name.push('_');
                    name.push_str(&add);
                }
            }
            name
        }
        TypeRef::Variable(handle) => {
            let name = &ctx.identifiers[*handle];
            format!("var_{name}")
        }
        TypeRef::Nat(n) => format!("{}", n),
        TypeRef::Reference { name, args } => {
            let type_name = &ctx.type_names[*name];
            let mut ident = type_name.base.clone();
            for arg in args.handles() {
                let add = type_to_ident(ctx, arg);
                if !add.is_empty() {
                    ident.push('_');
                    ident.push_str(&add);
                }
            }
            ident
        }
    }
}

fn lookup_sub(variant: Option<&TypeRefVariant>, handle: Handle<TypeRef>) -> Handle<TypeRef> {
    let mut handle = handle;
    let subs = if let Some(var) = variant {
        &var.variable_substitutions
    } else {
        return handle;
    };
    while let Some(sub) = subs.get(&handle).copied() {
        handle = sub;
    }
    handle
}

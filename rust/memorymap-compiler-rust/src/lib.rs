// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

pub mod device_desc;
pub mod device_instances;
pub mod type_desc;
pub mod type_reference;

use heck::{ToPascalCase, ToShoutySnakeCase, ToSnakeCase};
use proc_macro2::{Ident, Span};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IdentType {
    Type,
    TypeVariable,
    Device,
    Instance,
    Module,
    Method,
    Variable,
    Constant,
    // Just pass through the raw string
    Raw,
}

/// Generate a contextual identifier from a string.
pub fn ident(ident_type: IdentType, n: impl AsRef<str>) -> Ident {
    let s = n.as_ref();
    let s = s.split(".").last().unwrap();
    let s = match ident_type {
        IdentType::Type => s.to_pascal_case(),
        IdentType::TypeVariable => s.to_shouty_snake_case(),
        IdentType::Device => s.to_pascal_case(),
        IdentType::Instance => s.to_snake_case(),
        IdentType::Module => s.to_snake_case(),
        IdentType::Method => s.to_snake_case(),
        IdentType::Variable => s.to_snake_case(),
        IdentType::Constant => s.to_shouty_snake_case(),
        IdentType::Raw => s.to_string(),
    };

    Ident::new(&s, Span::call_site())
}

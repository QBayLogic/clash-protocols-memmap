// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Regression test: `testing_utils::generate_device_descs` must handle registers
//! whose type is a named (user-defined) type, not only builtin types.

use memorymap_compiler::parse;
use memorymap_compiler_rust::testing_utils as backend_rust;

#[test]
fn named_register_type_is_resolved() {
    // One register of type `Reading 8`, where `data Reading n = Reading (Unsigned n)`.
    let memmap = parse(include_str!("fixtures/named_type.json")).expect("fixture parses");

    let rendered = backend_rust::generate_device_descs(&memmap)
        .iter()
        .map(|(_name, code)| code.to_string())
        .collect::<String>();

    assert!(
        rendered.contains("fn reading (& self) -> Reading_8"),
        "expected the getter to return the monomorphized `Reading_8`; got:\n{rendered}"
    );
}

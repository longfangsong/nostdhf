//! This crate polyfills the missing methods on f32 and f64 in no_std environments
#![no_std]
#![warn(
    missing_docs,
    rust_2018_idioms,
    trivial_casts,
    trivial_numeric_casts,
    unused_qualifications
)]
#![feature(core_intrinsics)]

mod f32ext;
mod f64ext;

pub use crate::f32ext::F32Ext;
pub use crate::f64ext::F64Ext;

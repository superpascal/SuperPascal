//! SuperPascal Runtime Library
//!
//! This crate provides runtime support functions for SuperPascal language features
//! that require runtime behavior, such as Variant types, dynamic arrays, and closures.

pub mod variant;

/// Re-export variant module for convenience
pub use variant::*;


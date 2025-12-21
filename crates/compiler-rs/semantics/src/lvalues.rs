//! Lvalue analysis (left-hand side of assignments)

use ast::Node;
use types::Type;
use super::core::CoreAnalyzer;

// TODO: Extract lvalue analysis functions from lib.rs


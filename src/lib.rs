//! Dependency graphs are represented here as functions. Basically system F.
//! There will be nominal type equivalence right now because I don't want to think.
//!
//! Provided will be a way to find a path through this hypergraph using a SAT solver-y algorithm.

use std::collections::HashMap;

type TypeId = String;
type ParamId = String;
type OpId = String;

pub struct Type<Data> {
    pub name: TypeId,
    pub params: HashMap<ParamId, TypeId>,
    pub data: Data,
}

pub struct Op<Data> {
    pub name: OpId,
    pub inputs: Vec<Type<Data>>,
    pub outputs: Vec<Type<Data>>,
}

pub struct DependencyGraph<Data> {
    ops: Vec<Op<Data>>,
}

impl<Data> DependencyGraph<Data> {
    pub fn ops(&self) -> &[Op<Data>] {
        &self.ops
    }
}

pub struct Path<Data> {
    ops: Vec<Op<Data>>,
}

impl<Data> Path<Data> {
    pub fn path(&self) -> &[Op<Data>] {
        &self.ops
    }
}

pub struct PathErr {
    msg: String,
}

impl PathErr {
    pub fn error_msg(&self) -> &str {
        &self.msg
    }
}

pub fn find_path<Data>(
    start: &Type<Data>,
    end: &Type<Data>,
    ops: &DependencyGraph<Data>,
) -> Result<Path<Data>, PathErr> {
}

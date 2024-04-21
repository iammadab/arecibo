// this is supposed to build and return the LookupTrace
// operations:
//  new
//  read
//  write
//  snapshot
//  get_challenges (should this be here?)

use std::collections::BTreeMap;
use ff::PrimeField;
use crate::gadgets::lookup::lookup_table::LookupTable;
use crate::gadgets::lookup::lookup_trace::RWTrace;

// TODO: add documentation
struct LookupTraceBuilder<'a, F: PrimeField> {
    // needs a lookup table to get the actual values from and also to write them to
    // why does this need a cache??? thinking transaction related stuff so will just go with it
    // and a trace
    // TODO: add documentation for each piece
    lookup_table: &'a mut LookupTable<F>,
    trace: Vec<RWTrace<F>>,
    lookup_table_cache: BTreeMap<F, (F, F)>
}
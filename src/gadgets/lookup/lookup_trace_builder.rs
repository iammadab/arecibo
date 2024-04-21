// this is supposed to build and return the LookupTrace
// operations:
//  new
//  read
//  write
//  snapshot
//  get_challenges (should this be here?)

use crate::gadgets::lookup::lookup_table::LookupTable;
use crate::gadgets::lookup::lookup_trace::RWTrace;
use ff::PrimeField;
use std::collections::BTreeMap;

// TODO: add documentation
struct LookupTraceBuilder<'a, F: PrimeField> {
  // needs a lookup table to get the actual values from and also to write them to
  // why does this need a cache??? thinking transaction related stuff so will just go with it
  // and a trace
  // TODO: add documentation for each piece
  lookup_table: &'a mut LookupTable<F>,
  trace: Vec<RWTrace<F>>,
  lookup_table_cache: BTreeMap<F, (F, F)>,
}

impl<'a, F: PrimeField + Ord> LookupTraceBuilder<'a, F> {
  // TODO: add documentation
  fn new(lookup_table: &'a mut LookupTable<F>) -> Self {
    // TODO: does this need to do any extra checks?
    Self {
      lookup_table,
      trace: vec![],
      lookup_table_cache: BTreeMap::new(),
    }
  }

  // TODO: add documentation
  fn read(&mut self, addr: F) -> F {
    // first try to read from cache, if you cannot then read from lookup table
    let (read_value, read_ts, _, write_ts) = self.lookup_table.rw_operation(addr, None);
    self
        .trace
        .push(RWTrace::Read(addr, read_value, read_ts, write_ts));
    read_value
  }
}

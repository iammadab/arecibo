// this is supposed to build and return the LookupTrace
// operations:
//  new
//  read
//  write
//  snapshot
//  get_challenges (should this be here?)

use crate::constants::NUM_CHALLENGE_BITS;
use crate::gadgets::lookup::lookup_table::LookupTable;
use crate::gadgets::lookup::lookup_trace::{LookupTrace, RWTrace};
use crate::{scalar_as_base, CurveCycleEquipped, Dual, Engine, ROConstants, ROTrait};
use ff::PrimeField;
use std::collections::BTreeMap;
use std::mem;

// TODO: add documentation
struct LookupTraceBuilder<'a, E: CurveCycleEquipped> {
  // needs a lookup table to get the actual values from and also to write them to
  // why does this need a cache??? thinking transaction related stuff so will just go with it
  // and a trace
  // TODO: add documentation for each piece
  lookup_table: &'a mut LookupTable<E::Scalar>,
  // TODO: this could be the lookup trace itself
  trace: Vec<RWTrace<E::Scalar>>,
  lookup_table_cache: BTreeMap<E::Scalar, (E::Scalar, E::Scalar)>,
}

impl<'a, E: CurveCycleEquipped> LookupTraceBuilder<'a, E>
where
  E::Scalar: Ord,
{
  // TODO: add documentation
  fn new(lookup_table: &'a mut LookupTable<E::Scalar>) -> Self {
    // TODO: does this need to do any extra checks?
    Self {
      lookup_table,
      trace: vec![],
      lookup_table_cache: BTreeMap::new(),
    }
  }

  // TODO: add documentation
  fn read(&mut self, addr: E::Scalar) -> E::Scalar {
    let (read_value, read_ts, _, write_ts) = self.lookup_table.rw_operation(addr, None);
    self
      .trace
      .push(RWTrace::Read(addr, read_value, read_ts, write_ts));
    read_value
  }

  // TODO: add documentation
  fn write(&mut self, addr: E::Scalar, value: E::Scalar) {
    let (read_value, read_ts, write_value, write_ts) =
      self.lookup_table.rw_operation(addr, Some(value));
    self.trace.push(RWTrace::Write(
      addr,
      read_value,
      read_ts,
      write_value,
      write_ts,
    ));
  }

  // TODO: add documentation
  fn snapshot(
    &mut self,
    ro_consts: ROConstants<Dual<E>>,
    prev_intermediate_gamma: E::Scalar,
  ) -> (E::Scalar, LookupTrace<E>) {
    // the goal here is to update the intermediate gamma with every element in the trace
    // technically we could have done this during read / write
    // for this we need a hasher, the hasher seems to use values from the base field
    let mut hasher = <Dual<E> as Engine>::RO::new(
      ro_consts,
      // TODO: fix the number of absorbs, intentionally making it wrong to see how this affects things
      1,
    );

    hasher.absorb(prev_intermediate_gamma);

    // loop over every element in the read write trace and absorb
    // addr, read_value, read_ts, write_value, write_ts
    for rw_trace in &self.trace {
      let (addr, read_value, read_ts, write_value, write_ts) = match rw_trace {
        RWTrace::Read(addr, read_value, read_ts, write_ts) => {
          (addr, read_value, read_ts, read_value, write_ts)
        }
        RWTrace::Write(addr, read_value, read_ts, write_value, write_ts) => {
          (addr, read_value, read_ts, write_value, write_ts)
        }
      };
      hasher.absorb(*addr);
      hasher.absorb(*read_value);
      hasher.absorb(*read_ts);
      hasher.absorb(*write_value);
      hasher.absorb(*write_ts);
    }

    // then squeeze out the next intermediate gamma
    let next_intermediate_gamma = scalar_as_base::<Dual<E>>(hasher.squeeze(NUM_CHALLENGE_BITS));
    let trace = mem::take(&mut self.trace);

    (next_intermediate_gamma, LookupTrace::new(trace))
  }
}

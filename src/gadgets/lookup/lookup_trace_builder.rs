use crate::constants::NUM_CHALLENGE_BITS;
use crate::gadgets::lookup::lookup_table::LookupTable;
use crate::gadgets::lookup::lookup_trace::{LookupTrace, RWTrace};
use crate::{scalar_as_base, CurveCycleEquipped, Dual, Engine, ROConstants, ROTrait};
use ff::PrimeField;
use std::collections::BTreeMap;
use std::mem;

/// Performs read write operations on a `LookupTable` while keeping track
/// of each operation in a trace vector
struct LookupTraceBuilder<'a, E: CurveCycleEquipped> {
  lookup_table: &'a mut LookupTable<E::Scalar>,
  trace: Vec<RWTrace<E::Scalar>>,
}

impl<'a, E: CurveCycleEquipped> LookupTraceBuilder<'a, E>
where
  E::Scalar: Ord,
{
  /// Instantiate a new `LookupTraceBuilder` for a `LookupTable`
  fn new(lookup_table: &'a mut LookupTable<E::Scalar>) -> Self {
    Self {
      lookup_table,
      trace: vec![],
    }
  }

  /// Performs and records a read operation on the `LookupTable`
  fn read(&mut self, addr: E::Scalar) -> E::Scalar {
    let (read_value, read_ts, _, write_ts) = self.lookup_table.rw_operation(addr, None);
    self
      .trace
      .push(RWTrace::Read(addr, read_value, read_ts, write_ts));
    read_value
  }

  /// Performs and records a write operation on the `LookupTable`
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

  /// Generates the next intermediate gamma and returns the `LookupTrace`
  fn snapshot(
    &mut self,
    ro_consts: ROConstants<Dual<E>>,
    prev_intermediate_gamma: E::Scalar,
  ) -> (E::Scalar, LookupTrace<E>) {
    let mut hasher = <Dual<E> as Engine>::RO::new(ro_consts, 1 + 5 * self.trace.len());

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

    // squeeze out the next intermediate gamma
    let next_intermediate_gamma = scalar_as_base::<Dual<E>>(hasher.squeeze(NUM_CHALLENGE_BITS));
    let trace = mem::take(&mut self.trace);

    (next_intermediate_gamma, LookupTrace::new(trace))
  }

  // TODO: implement get_challenges
}

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

#[cfg(test)]
mod tests {
  use crate::gadgets::lookup::lookup_table::{LookupTable, TableType};
  use crate::gadgets::lookup::lookup_trace::{LookupTrace, RWTrace};
  use crate::gadgets::lookup::lookup_trace_builder::LookupTraceBuilder;
  use crate::provider::poseidon::PoseidonConstantsCircuit;
  use crate::provider::PallasEngine;
  use crate::{Dual, Engine, ROTrait};
  use ff::Field;

  type F = <PallasEngine as Engine>::Scalar;

  #[test]
  fn test_lookup_builder_rw_operations() {
    // table = []
    // global_ts = 0

    // write addr=0 val=15
    // table = [Write(0, 0, 0, 15, 1)]
    // global_ts = 1

    // read addr=0
    // table = [Write(0, 0, 0, 15, 1), Read(0, 15, 1, 15, 2)]
    // global_ts = 2

    // write addr=1 val=12
    // table = [Write(0, 0, 0, 15, 1), Read(0, 15, 1, 2), Write(1, 0, 0, 12, 3)]
    // global_ts = 3

    // read addr=1
    // table = [Write(0, 0, 0, 15, 1), Read(0, 15, 1, 2), Write(1, 0, 0, 12, 3), Read(1, 12, 3, 4)]
    // global_ts = 4

    // write addr=0 val=6
    // table = [Write(0, 0, 0, 15, 1), Read(0, 15, 1, 2), Write(1, 0, 0, 12, 3), Read(1, 12, 3, 4), Write(0, 15, 2, 6, 5)]
    // global_ts = 5

    let expected_final_trace = vec![
      RWTrace::Write(F::ZERO, F::ZERO, F::ZERO, F::from(15), F::ONE),
      RWTrace::Read(F::ZERO, F::from(15), F::ONE, F::from(2)),
      RWTrace::Write(F::ONE, F::ZERO, F::ZERO, F::from(12), F::from(3)),
      RWTrace::Read(F::ONE, F::from(12), F::from(3), F::from(4)),
      RWTrace::Write(F::ZERO, F::from(15), F::from(2), F::from(6), F::from(5)),
    ];

    let mut lookup_table = LookupTable::new(vec![], TableType::ReadWrite);
    let mut lookup_trace_builder = LookupTraceBuilder::<PallasEngine>::new(&mut lookup_table);

    // perform read write operations on the builder
    // write addr=0 val=15
    lookup_trace_builder.write(F::ZERO, F::from(15));

    // read addr=0
    let read_value = lookup_trace_builder.read(F::ZERO);
    assert_eq!(read_value, F::from(15));

    // write addr=1 val=12
    lookup_trace_builder.write(F::ONE, F::from(12));

    // read addr=1
    let read_value = lookup_trace_builder.read(F::ONE);
    assert_eq!(read_value, F::from(12));

    // write addr=0 val=6
    lookup_trace_builder.write(F::ZERO, F::from(6));

    let (_, lookup_trace) =
      lookup_trace_builder.snapshot(PoseidonConstantsCircuit::default(), F::ONE);

    // assert that lookup trace matches the expected lookup trace
    assert_eq!(lookup_trace, LookupTrace::new(expected_final_trace));
  }
}

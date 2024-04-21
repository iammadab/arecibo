// TODO: move pieces into separate files
use ff::PrimeField;
use std::collections::BTreeMap;

/// Specifies read write permissions for the `LookupTable`
#[derive(PartialEq)]
enum TableType {
  ReadOnly,
  ReadWrite,
}

/// ReadOnly or ReadWrite mapping from address to value
struct LookupTable<F: PrimeField> {
  table: BTreeMap<F, (F, F)>,
  global_ts: F,
  table_type: TableType,
}

impl<F: PrimeField + Ord> LookupTable<F> {
  /// Create a new `LookupTable` setting initial values and read write permissions
  fn new(initial_table: Vec<(F, F)>, table_type: TableType) -> Self {
    let table_map = initial_table
      .into_iter()
      .enumerate()
      .map(|(expected_addr, (addr, value))| {
        // assert that initial table addresses
        // are contiguous and strictly increasing, starting at 0
        assert!(F::from(expected_addr as u64) == addr);
        (addr, (value, F::ZERO))
      })
      .collect();

    Self {
      table: table_map,
      global_ts: F::ZERO,
      table_type,
    }
  }

  /// When value is set to None, performs a read operation
  /// When value is set to Some, performs a write operation
  fn rw_operation(&mut self, addr: F, value: Option<F>) -> (F, F, F, F) {
    // assumes every field element is in the addressable memory space
    let (read_value, read_ts) = self.table.get(&addr).cloned().unwrap_or((F::ZERO, F::ZERO));

    let (write_value, write_ts) = if self.table_type == TableType::ReadOnly {
      (read_value, read_ts + F::ONE)
    } else {
      debug_assert!(self.table_type == TableType::ReadWrite);
      let value_to_write = value.unwrap_or(read_value);
      self.global_ts = self.global_ts.max(read_ts) + F::ONE;
      (value_to_write, self.global_ts)
    };

    // update value and count
    self.table.insert(addr, (write_value, write_ts));

    (read_value, read_ts, write_value, write_ts)
  }

  // TODO: list out and implement other helper methods (as demand suggests)
}

#[cfg(test)]
mod tests {
  use crate::gadgets::lookup::{LookupTable, TableType};
  use crate::provider::PallasEngine;
  use crate::Engine;
  use ff::Field;

  type F = <PallasEngine as Engine>::Scalar;

  #[test]
  #[should_panic]
  fn test_initial_table_address_strictly_increasing_and_contiguous() {
    let invalid_initial_table = vec![(F::ONE, F::from(10)), (F::from(2), F::from(15))];
    LookupTable::new(invalid_initial_table, TableType::ReadOnly);
  }

  #[test]
  fn test_read_only_lookup_table_operations() {
    // initialize read only lookup table
    let initial_table = vec![
      (F::ZERO, F::from(2)),
      (F::ONE, F::from(4)),
      (F::from(2), F::from(6)),
    ];
    let mut lookup_table = LookupTable::new(initial_table, TableType::ReadOnly);

    // read addr 0
    let rw_operation_result = lookup_table.rw_operation(F::ZERO, None);
    assert_eq!(
      rw_operation_result,
      (F::from(2), F::ZERO, F::from(2), F::ONE)
    );
    assert_eq!(
      lookup_table.table.get(&F::ZERO),
      Some(&(F::from(2), F::ONE))
    );

    // read addr 1
    let rw_operation_result = lookup_table.rw_operation(F::ONE, None);
    assert_eq!(
      rw_operation_result,
      (F::from(4), F::ZERO, F::from(4), F::ONE)
    );
    assert_eq!(lookup_table.table.get(&F::ONE), Some(&(F::from(4), F::ONE)));

    // read addr 2
    let rw_operation_result = lookup_table.rw_operation(F::from(2), None);
    assert_eq!(
      rw_operation_result,
      (F::from(6), F::ZERO, F::from(6), F::ONE)
    );
    assert_eq!(
      lookup_table.table.get(&F::from(2)),
      Some(&(F::from(6), F::ONE))
    );

    // read addr 1
    let rw_operation_result = lookup_table.rw_operation(F::ONE, None);
    assert_eq!(
      rw_operation_result,
      (F::from(4), F::ONE, F::from(4), F::from(2))
    );
    assert_eq!(
      lookup_table.table.get(&F::ONE),
      Some(&(F::from(4), F::from(2)))
    );
  }

  #[test]
  fn test_read_write_lookup_table_operations() {
    // initialize read write lookup table
    let initial_table = vec![
      (F::ZERO, F::from(2)),
      (F::ONE, F::from(4)),
      (F::from(2), F::from(6)),
    ];
    let mut lookup_table = LookupTable::new(initial_table, TableType::ReadWrite);

    // series of read / write operations to ensure
    // global_ts is used correctly

    // read addr 1
    let rw_operation_result = lookup_table.rw_operation(F::ONE, None);
    assert_eq!(
      rw_operation_result,
      (F::from(4), F::ZERO, F::from(4), F::ONE)
    );
    assert_eq!(lookup_table.table.get(&F::ONE), Some(&(F::from(4), F::ONE)));

    // write addr 2
    let rw_operation_result = lookup_table.rw_operation(F::from(2), Some(F::from(46)));
    assert_eq!(
      rw_operation_result,
      (F::from(6), F::ZERO, F::from(46), F::from(2))
    );
    assert_eq!(
      lookup_table.table.get(&F::from(2)),
      Some(&(F::from(46), F::from(2)))
    );

    // read addr 0
    let rw_operation_circuit = lookup_table.rw_operation(F::from(0), None);
    assert_eq!(
      rw_operation_circuit,
      (F::from(2), F::ZERO, F::from(2), F::from(3))
    );
    assert_eq!(
      lookup_table.table.get(&F::ZERO),
      Some(&(F::from(2), F::from(3)))
    );
  }
}

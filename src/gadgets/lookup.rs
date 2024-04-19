// TODO: move pieces into separate files

// TODO:
//  create lookup table / memory
//  test this
//  add max_cap_for_global_ts (think about this)
//  test this

use ff::PrimeField;
use std::collections::BTreeMap;

// TODO: best way I have seen for ordering is, introduce first use later

// TODO: add documentation
enum TableType {
  ReadOnly,
  ReadWrite,
}

// TODO: add documentation
struct LookupTable<F: PrimeField> {
  table: BTreeMap<F, (F, F)>,
  global_ts: F,
  table_type: TableType, // TODO: add and use the max_cap_for_global_ts
}

// TODO: what methods??
//  ability to init a new table
//  ability to read, ability to write
impl<F: PrimeField> LookupTable<F> {
  // TODO: add documentation
  fn new(initial_table: Vec<(F, F)>, table_type: TableType) -> Self {
    let table_map = initial_table
      .into_iter()
      .enumerate()
      .map(|expected_addr, (addr, value)| {
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
}

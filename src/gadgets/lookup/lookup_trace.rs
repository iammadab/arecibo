use crate::CurveCycleEquipped;
use bellpepper_core::num::AllocatedNum;

#[derive(Clone, Debug, PartialEq)]
/// Represents a Read / Write operation.
/// Contains all information needed to prove table / memory consistency
pub enum RWTrace<F> {
  // addr, read_value, read_ts, write_ts
  Read(F, F, F, F),
  // addr, read_value, read_ts, write_value, write_ts
  Write(F, F, F, F, F),
}

// TODO: add documentation
pub struct LookupTrace<E: CurveCycleEquipped> {
  trace: Vec<RWTrace<E::Scalar>>,
  allocated_trace: Vec<RWTrace<AllocatedNum<E::Scalar>>>,
}

impl<E: CurveCycleEquipped> LookupTrace<E> {
  // TODO: add documentation
  pub fn new(trace: Vec<RWTrace<E::Scalar>>) -> Self {
    Self {
      trace,
      allocated_trace: vec![],
    }
  }

  // TODO: add documentation
  pub fn trace(&self) -> &[RWTrace<E::Scalar>] {
    self.trace.as_ref()
  }
}

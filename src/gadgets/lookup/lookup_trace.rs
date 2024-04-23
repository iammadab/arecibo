use crate::CurveCycleEquipped;
use bellpepper_core::num::AllocatedNum;
use bellpepper_core::{ConstraintSystem, SynthesisError};

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
  cursor: usize,
}

impl<E: CurveCycleEquipped> LookupTrace<E> {
  // TODO: add documentation
  pub fn new(trace: Vec<RWTrace<E::Scalar>>) -> Self {
    Self {
      trace,
      allocated_trace: vec![],
      cursor: 0,
    }
  }

  // TODO: still think a bit about the possibility of getting rid of this function and the
  //  write counterpart.
  //  leaving it here because it kinda makes sense to want to constrain the address somewhere
  //  else without having to use copy constraints to ensure they are the same values.
  // TODO: add documentation
  pub fn read<CS: ConstraintSystem<E::Scalar>>(
    &mut self,
    mut cs: CS,
    addr: &AllocatedNum<E::Scalar>,
  ) -> Result<AllocatedNum<E::Scalar>, SynthesisError> {
    // assert that we are not adding more trace operations than is expected
    assert!(
      self.cursor < self.trace.len(),
      "cursor {} out of range of expected len {}",
      self.cursor,
      self.trace.len()
    );

    let RWTrace::Read(expected_addr, read_value, read_ts, write_ts) = self.trace[self.cursor]
    else {
      return Err(SynthesisError::AssignmentMissing);
    };

    // verify that allocated address is the same as expected address
    if let Some(allocated_addr) = addr.get_value() {
      assert!(
        allocated_addr == expected_addr,
        "read address {:?} mismatch with expected {:?}",
        allocated_addr,
        expected_addr
      );
    } else {
      return Err(SynthesisError::AssignmentMissing);
    }

    // witness remaining trace operation values
    let read_value = AllocatedNum::alloc(cs.namespace(|| "read_value"), || Ok(read_value))?;
    let read_ts = AllocatedNum::alloc(cs.namespace(|| "read_ts"), || Ok(read_ts))?;
    let write_ts = AllocatedNum::alloc(cs.namespace(|| "write_ts"), || Ok(write_ts))?;

    self.allocated_trace.push(RWTrace::Read(
      addr.clone(),
      read_value.clone(),
      read_ts,
      write_ts,
    ));
    self.cursor += 1;

    Ok(read_value)
  }

  // TODO: add documentation
  pub fn write<CS: ConstraintSystem<E::Scalar>>(
    &mut self,
    mut cs: CS,
    addr: &AllocatedNum<E::Scalar>,
    value: &AllocatedNum<E::Scalar>,
  ) -> Result<(), SynthesisError> {
    // assert that we are not adding more trace operations than is expected
    assert!(
      self.cursor < self.trace.len(),
      "cursor {} out of range of expected len {}",
      self.cursor,
      self.trace.len()
    );

    let RWTrace::Write(expected_addr, read_value, read_ts, expected_write_value, write_ts) =
      self.trace[self.cursor]
    else {
      return Err(SynthesisError::AssignmentMissing);
    };

    // verify that the allocated address is the same as expected address
    if let Some(allocated_addr) = addr.get_value() {
      assert!(
        allocated_addr == expected_addr,
        "write address {:?} mismatch with expected {:?}",
        allocated_addr,
        expected_addr
      );
    } else {
      return Err(SynthesisError::AssignmentMissing);
    }

    // verify that the allocated value is the same as the expected value
    if let Some(allocated_value) = value.get_value() {
      assert!(
        allocated_value == expected_write_value,
        "write value {:?} mismatch with expected {:?}",
        allocated_value,
        expected_write_value
      )
    }

    // witness remaining trace operation values
    let read_value = AllocatedNum::alloc(cs.namespace(|| "read_value"), || Ok(read_value))?;
    let read_ts = AllocatedNum::alloc(cs.namespace(|| "read_ts"), || Ok(read_ts))?;
    let write_value =
      AllocatedNum::alloc(cs.namespace(|| "read_value"), || Ok(expected_write_value))?;
    let write_ts = AllocatedNum::alloc(cs.namespace(|| "write_ts"), || Ok(write_ts))?;

    self.allocated_trace.push(RWTrace::Write(
      addr.clone(),
      read_value.clone(),
      read_ts,
      write_value,
      write_ts,
    ));
    self.cursor += 1;

    Ok(())
  }

  // TODO: add documentation
  pub fn commit(&self) {
    // here it seems we are basically doing what we did during the snapshot phase
    // but now inside a circuit?
    // no we are doing more.
    // we are computing the next intermediate gamma as constraints :check
    // then we are for each operation adding and removing for the running multiset accumulator
    todo!()
  }

  // TODO: add documentation
  pub fn accumulate_rw_operation(&self) {
    // what is needed for this read write operation?
    // it takes in the following:
    // - addr
    // - challenges (r, gamma)
    // - read_value
    // - write_value
    // - prev_RW_acc
    // - read_ts
    // - global_ts
    // firstly why is the trace element destructured? why not just pass AllocatedRWTrace
    // then where is the write_ts?
    // seems they want to compute the write_ts inside of the circuit
    //  - what other things should be computed within the circuit
    //  - how does one verify the read_ts??
    //    we want them to prove correctness of the write ts, but defer correctness of the rest
    //    to the final snark??? (is that sufficient?)
    //    will be nice to do some soundness analysis with this
    todo!()
  }

  /// Return a reference to the internal trace vector
  pub fn trace(&self) -> &[RWTrace<E::Scalar>] {
    self.trace.as_ref()
  }
}

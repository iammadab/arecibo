use crate::constants::NUM_CHALLENGE_BITS;
use crate::gadgets::le_bits_to_num;
use crate::gadgets::lookup::lookup_table::TableType;
use crate::gadgets::lookup::util::{add_allocated_num, less_than};
use crate::gadgets::utils::{alloc_one, conditionally_select2};
use crate::traits::ROCircuitTrait;
use crate::{CurveCycleEquipped, Dual, Engine, ROConstantsCircuit};
use bellpepper_core::num::AllocatedNum;
use bellpepper_core::{ConstraintSystem, LinearCombination, SynthesisError};
use ff::{Field, PrimeField};

#[derive(Clone, Debug, PartialEq)]
/// Represents a Read / Write operation.
/// Contains all information needed to prove table / memory consistency
pub enum RWTrace<F> {
  // addr, read_value, read_ts, write_ts
  Read(F, F, F, F),
  // addr, read_value, read_ts, write_value, write_ts
  Write(F, F, F, F, F),
}

impl<F> RWTrace<F> {
  /// Returns Read / Write contents uniformly
  pub fn destructure(&self) -> (&F, &F, &F, &F, &F) {
    match self {
      RWTrace::Read(addr, read_value, read_ts, write_ts) => {
        (addr, read_value, read_ts, read_value, write_ts)
      }
      RWTrace::Write(addr, read_value, read_ts, write_value, write_ts) => {
        (addr, read_value, read_ts, write_value, write_ts)
      }
    }
  }
}

// TODO: add documentation
pub struct LookupTrace<E: CurveCycleEquipped> {
  trace: Vec<RWTrace<E::Scalar>>,
  allocated_trace: Vec<RWTrace<AllocatedNum<E::Scalar>>>,
  cursor: usize,
  table_type: TableType,
  max_cap_global_ts_log2: usize,
}

impl<E: CurveCycleEquipped> LookupTrace<E>
where
  E::Scalar: PartialOrd,
{
  // TODO: add documentation
  pub fn new(
    trace: Vec<RWTrace<E::Scalar>>,
    table_type: TableType,
    max_cap_global_ts_log2: usize,
  ) -> Self {
    Self {
      trace,
      allocated_trace: vec![],
      cursor: 0,
      table_type,
      max_cap_global_ts_log2,
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
  pub fn commit<CS: ConstraintSystem<E::Scalar>>(
    &self,
    mut cs: CS,
    ro_consts: ROConstantsCircuit<Dual<E>>,
    prev_intermediate_gamma: &AllocatedNum<E::Scalar>,
    challenges: &(AllocatedNum<E::Scalar>, AllocatedNum<E::Scalar>),
    prev_rw_acc: &AllocatedNum<E::Scalar>,
    prev_global_ts: &AllocatedNum<E::Scalar>,
  ) -> Result<
    (
      AllocatedNum<E::Scalar>,
      AllocatedNum<E::Scalar>,
      AllocatedNum<E::Scalar>,
    ),
    SynthesisError,
  > {
    // 1 for prev_intermediate_gamma + 5 per RWTrace element
    let no_of_absorbs = 1 + 5 * self.trace.len();

    let mut hasher_circuit = <Dual<E> as Engine>::ROCircuit::new(ro_consts, no_of_absorbs);
    hasher_circuit.absorb(prev_intermediate_gamma);

    // for every element in the allocated rw_trace,
    // we need to accumulate into prev_rw_acc
    // and absorb into the hasher circuit (to generate the next intermediate gamma)
    let (rw_acc, global_ts) = self.allocated_trace.iter().try_fold(
      (prev_rw_acc.clone(), prev_global_ts.clone()),
      |(running_rw_acc, running_global_ts), rw_trace| {
        let (addr, read_value, read_ts, write_value, write_ts) = rw_trace.destructure();
        hasher_circuit.absorb(addr);
        hasher_circuit.absorb(read_value);
        hasher_circuit.absorb(read_ts);
        hasher_circuit.absorb(write_value);
        hasher_circuit.absorb(write_ts);

        Ok::<(AllocatedNum<E::Scalar>, AllocatedNum<E::Scalar>), SynthesisError>(
          self.accumulate_rw_operation(
            // TODO: verify if this needs to be different for each iteration
            cs.namespace(|| "accumulate read write"),
            rw_trace,
            challenges,
            &running_rw_acc,
            &running_global_ts,
          )?,
        )
      },
    )?;

    let hash_bits = hasher_circuit.squeeze(cs.namespace(|| "challenge"), NUM_CHALLENGE_BITS)?;
    let next_intermediate_gamma = le_bits_to_num(cs.namespace(|| "bits to hash"), &hash_bits)?;

    Ok((rw_acc, global_ts, next_intermediate_gamma))
  }

  // TODO: add documentation
  pub fn accumulate_rw_operation<CS: ConstraintSystem<E::Scalar>>(
    &self,
    mut cs: CS,
    rw_trace: &RWTrace<AllocatedNum<E::Scalar>>,
    challenges: &(AllocatedNum<E::Scalar>, AllocatedNum<E::Scalar>),
    prev_rw_acc: &AllocatedNum<E::Scalar>,
    prev_global_ts: &AllocatedNum<E::Scalar>,
  ) -> Result<(AllocatedNum<E::Scalar>, AllocatedNum<E::Scalar>), SynthesisError> {
    let (addr, read_value, read_ts, write_value, _) = rw_trace.destructure();

    let (r, gamma) = challenges;

    let gamma_square = gamma.mul(cs.namespace(|| "gamma^2"), gamma)?;

    // accumulate read operation
    // (addr, read_value, read_ts)
    // tuple compression = addr + gamma * read_value + gamma^2 * read_ts
    // rw_acc = prev_acc + 1 / (r + (addr + gamma * value + gamma^2 * read_ts))

    let read_value_term = gamma.mul(cs.namespace(|| "read_value_term"), read_value)?;
    let read_ts_term = gamma_square.mul(cs.namespace(|| "read_ts_term"), read_ts)?;

    // compute rw_acc = prev_acc + 1 / (r + (addr + gamma * value + gamma^2 * read_ts))
    let rw_acc = AllocatedNum::alloc(cs.namespace(|| "rw_acc_added"), || {
      match (
        prev_rw_acc.get_value(),
        r.get_value(),
        addr.get_value(),
        read_value_term.get_value(),
        read_ts_term.get_value(),
      ) {
        (Some(prev_rw_acc), Some(r), Some(addr), Some(read_value_term), Some(read_ts_term)) => Ok(
          prev_rw_acc
            + (r + (addr + read_value_term + read_ts_term))
              .invert()
              .expect("cannot invert 0"),
        ),
        _ => Err(SynthesisError::AssignmentMissing),
      }
    })?;

    // compute r + (addr + gamma * read_value + gamma^2 * read_ts)
    let mut r_blc = LinearCombination::<E::Scalar>::zero();
    r_blc = r_blc
      + r.get_variable()
      + addr.get_variable()
      + read_value_term.get_variable()
      + read_ts_term.get_variable();

    // ensure that (rw_acc - prev_rw_acc) * r_blc = 1
    cs.enforce(
      || "rw_acc_update",
      |lc| lc + rw_acc.get_variable() - prev_rw_acc.get_variable(),
      |_| r_blc,
      |lc| lc + CS::one(),
    );

    // compute write_ts
    // if table_type = read_only then write_ts = read_ts + 1
    // if table_type = read_write then write_ts = max{read_ts, global_ts} + 1

    // note the computation below doesn't add +1 to the write_ts, this will be handled later
    let (write_ts, write_ts_term) = if self.table_type == TableType::ReadOnly {
      (read_ts.clone(), read_ts_term)
    } else {
      assert!(self.table_type == TableType::ReadWrite);
      let lt = less_than(
        cs.namespace(|| "read_ts < previous_global_ts"),
        read_ts,
        prev_global_ts,
        self.max_cap_global_ts_log2,
      )?;
      let write_ts = conditionally_select2(
        cs.namespace(|| "write_ts = read_ts < previous_global_ts ? previous_global_ts : read_ts"),
        prev_global_ts,
        read_ts,
        &lt,
      )?;
      let write_ts_term = gamma_square.mul(cs.namespace(|| "write_ts_term"), &write_ts)?;
      (write_ts, write_ts_term)
    };

    // enforce correct write operation

    // accumulate write operation
    // (addr, write_value, write_ts)
    // tuple compression = addr + gamma * write_value + gamma^2 * write_ts
    // rw_acc = prev_acc - 1 / (r + (addr + gamma * value + gamma^2 * read_ts))

    let write_value_term = gamma.mul(cs.namespace(|| "write_value_term"), write_value)?;

    // compute rw_acc = prev_acc - 1 / (r + (addr + gamma * value + gamma^2 * read_ts))
    let rw_acc = AllocatedNum::alloc(cs.namespace(|| "rw_acc_added"), || {
      match (
        rw_acc.get_value(),
        r.get_value(),
        addr.get_value(),
        write_value_term.get_value(),
        write_ts_term.get_value(),
      ) {
        (Some(prev_rw_acc), Some(r), Some(addr), Some(write_value_term), Some(write_ts_term)) => {
          Ok(
            // TODO: is it safe to just subtract rather than converting it to neg
            prev_rw_acc
              - (r + (addr + write_value_term + write_ts_term))
                .invert()
                .expect("cannot invert 0"),
          )
        }
        _ => Err(SynthesisError::AssignmentMissing),
      }
    })?;

    // compute r + (addr + gamma * read_value + gamma^2 * read_ts)
    let mut w_blc = LinearCombination::<E::Scalar>::zero();
    w_blc = w_blc
      + r.get_variable()
      + addr.get_variable()
      + write_value_term.get_variable()
      + write_ts_term.get_variable();

    // ensure that (rw_acc - prev_rw_acc) * r_blc = 1
    cs.enforce(
      || "rw_acc_update",
      |lc| lc + rw_acc.get_variable() - prev_rw_acc.get_variable(),
      |_| w_blc,
      |lc| lc + CS::one(),
    );

    // add one to global_ts
    let alloc_num_one = alloc_one(cs.namespace(|| "one"));
    let new_global_ts =
      add_allocated_num(cs.namespace(|| "new_global_ts"), &write_ts, &alloc_num_one)?;

    Ok((rw_acc, new_global_ts))
  }

  /// Return a reference to the internal trace vector
  pub fn trace(&self) -> &[RWTrace<E::Scalar>] {
    self.trace.as_ref()
  }
}

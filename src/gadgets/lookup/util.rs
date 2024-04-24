use crate::gadgets::Num;
use bellpepper_core::boolean::Boolean;
use bellpepper_core::num::AllocatedNum;
use bellpepper_core::{ConstraintSystem, SynthesisError};
use ff::PrimeField;

/// alloc a field as a constant
/// implemented refer from <https://github.com/lurk-lab/lurk-rs/blob/4335fbb3290ed1a1176e29428f7daacb47f8033d/src/circuit/gadgets/data.rs#L387-L402>
pub fn alloc_const<F: PrimeField, CS: ConstraintSystem<F>>(mut cs: CS, val: F) -> AllocatedNum<F> {
  let allocated = AllocatedNum::<F>::alloc_infallible(cs.namespace(|| "allocate const"), || val);

  // allocated * 1 = val
  cs.enforce(
    || "enforce constant",
    |lc| lc + allocated.get_variable(),
    |lc| lc + CS::one(),
    |_| Boolean::Constant(true).lc(CS::one(), val),
  );

  allocated
}

/// a < b ? 1 : 0
pub fn less_than<F: PrimeField + PartialOrd, CS: ConstraintSystem<F>>(
  mut cs: CS,
  a: &AllocatedNum<F>,
  b: &AllocatedNum<F>,
  n_bits: usize,
) -> Result<AllocatedNum<F>, SynthesisError> {
  assert!(n_bits < 64, "not support n_bits {n_bits} >= 64");
  let range = alloc_const(cs.namespace(|| "range"), F::from(1u64 << n_bits));
  // diff = (lhs - rhs) + (if lt { range } else { 0 });
  let diff = Num::alloc(cs.namespace(|| "diff"), || {
    a.get_value()
      .zip(b.get_value())
      .zip(range.get_value())
      .map(|((a, b), range)| {
        let lt = a < b;
        (a - b) + (if lt { range } else { F::ZERO })
      })
      .ok_or(SynthesisError::AssignmentMissing)
  })?;
  diff.fits_in_bits(cs.namespace(|| "diff fit in bits"), n_bits)?;
  let diff = diff.as_allocated_num(cs.namespace(|| "diff_alloc_num"))?;
  let lt = AllocatedNum::alloc(cs.namespace(|| "lt"), || {
    a.get_value()
      .zip(b.get_value())
      .map(|(a, b)| F::from(u64::from(a < b)))
      .ok_or(SynthesisError::AssignmentMissing)
  })?;
  cs.enforce(
    || "lt is bit",
    |lc| lc + lt.get_variable(),
    |lc| lc + CS::one() - lt.get_variable(),
    |lc| lc,
  );
  cs.enforce(
    || "lt ⋅ range == diff - lhs + rhs",
    |lc| lc + lt.get_variable(),
    |lc| lc + range.get_variable(),
    |lc| lc + diff.get_variable() - a.get_variable() + b.get_variable(),
  );
  Ok(lt)
}

/// c = a + b where a, b is AllocatedNum
pub fn add_allocated_num<F: PrimeField, CS: ConstraintSystem<F>>(
  mut cs: CS,
  a: &AllocatedNum<F>,
  b: &AllocatedNum<F>,
) -> Result<AllocatedNum<F>, SynthesisError> {
  let c = AllocatedNum::alloc(cs.namespace(|| "c"), || {
    a.get_value()
      .zip(b.get_value())
      .map(|(a, b)| a + b)
      .ok_or(SynthesisError::AssignmentMissing)
  })?;
  cs.enforce(
    || "c = a+b",
    |lc| lc + a.get_variable() + b.get_variable(),
    |lc| lc + CS::one(),
    |lc| lc + c.get_variable(),
  );
  Ok(c)
}

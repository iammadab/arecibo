/// Represents a Read / Write operation.
/// Contains all information needed to prove table / memory consistency
pub enum RWTrace<F>{
    // addr, read_value, read_ts, write_ts
    Read(F, F, F, F),
    // addr, read_value, write_value, read_ts, write_ts
    Write(F, F, F, F, F)
}
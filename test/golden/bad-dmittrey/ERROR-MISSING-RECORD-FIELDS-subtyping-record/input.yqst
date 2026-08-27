language core;

extend with #records,
            #natural-literals,
            #top-type,
            #bottom-type,
            #structural-subtyping;

fn iterate(n : Nat) -> { current : Nat } {
  return { current = n, next = succ(n) }
}

fn main(n : Nat) -> { current : Nat, next : Nat } {
  return iterate(0)
}

//ERROR_MISSING_RECORD_FIELDS

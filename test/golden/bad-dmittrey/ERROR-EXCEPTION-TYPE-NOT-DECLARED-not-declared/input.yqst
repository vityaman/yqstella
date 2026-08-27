// ERROR_AMBIGUOUS_THROW_TYPE

language core;

extend with #exceptions, #natural-literals;

fn main(n : Nat) -> Nat {
  return try { throw(1) } with { 1 }
}


//ERROR_EXCEPTION_TYPE_NOT_DECLARED

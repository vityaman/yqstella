language core;

extend with #variants, #type-ascriptions;

fn main(succeed : Bool) -> Nat {
  return match (<| a = succ(0) |>) as <| a : Nat, c : Bool |> {
        <| a = t |> => t
        | <| b = t |> => 0
    }
}


//ERROR_UNEXPECTED_PATTERN_FOR_TYPE

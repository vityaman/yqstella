language core;
extend with #exceptions, #open-variant-exceptions, #variants, #structural-patterns, #open-variant-exceptions;

exception variant bool : Bool
exception variant nat : Nat

fn fail(n : Nat) -> Bool {
	return throw(<| bool = true |>)
}

fn main(n : Nat) -> Bool {
	return try { true } catch { <| bool = true |> => true }
}
//SUCCESS

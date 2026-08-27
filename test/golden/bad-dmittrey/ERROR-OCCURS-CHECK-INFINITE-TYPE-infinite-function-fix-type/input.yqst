language core;

extend with #type-reconstruction, #fixpoint-combinator;

fn main(f : auto) -> auto {
    return fix(f(f))
}


//ERROR_OCCURS_CHECK_INFINITE_TYPE

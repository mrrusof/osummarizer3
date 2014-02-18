let test (a: int) b = assert(b < a)

let _ = check_safe(test, 2)

let f x =
  assume (x > 0);
  assert (x > 0)

let _ = check_safe(f, 1)

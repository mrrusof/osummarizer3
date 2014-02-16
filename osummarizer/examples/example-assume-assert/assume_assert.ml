let f x =
  assume (x > 0);
  assert (x > 0)
in check_safe(f, 1)

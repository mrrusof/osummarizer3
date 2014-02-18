let main () =
  let x = if nondet () then 1 else 2
  in assert (x > 0)
in check_safe(main, 1)

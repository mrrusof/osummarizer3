let main () =
  let x = if nondet () then 1 else 2
  in assert (x < 1)
in check_safe(main, 1)

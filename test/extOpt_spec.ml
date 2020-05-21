open OUnit
open TestUtil
open ExtOpt

let get_or_else_test =
  "get_or_else"
  >::: [
         ("Noneの場合" >:: fun _ -> assert_equal_int 42 (get_or_else 42 None));
         ("Someの場合" >:: fun _ -> assert_equal_int 42 (get_or_else 57 (Some 42)));
       ]

let _ = run_test_tt_main ("option.ml" >::: [get_or_else_test])

open OUnit

open TestUtil
open Option


let get_test =
  "get" >::: [
    ("Someの場合" >:: fun _ ->
        assert_equal_int (get @@ Some 42) 42)
  ]

let get_or_else_test =
  "get_or_else" >::: [
    ("Noneの場合" >:: fun _ ->
        assert_equal_int (get_or_else 42 None) 42)
    ;
    ("Someの場合" >:: fun _ ->
        assert_equal_int (get_or_else 57 (Some 42)) 42)
  ]



let _ = run_test_tt_main begin
    "option.ml" >::: [
      get_test;
      get_or_else_test
    ]
  end

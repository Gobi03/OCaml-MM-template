open OUnit

open TestUtil
open ExtOpt

let get_or_else_test =
  "get_or_else" >::: [
    ("Noneの場合" >:: fun _ ->
        assert_equal_int 42 (get_or_else 42 None))
    ;
    ("Someの場合" >:: fun _ ->
        assert_equal_int 42 (get_or_else 57 (Some 42)))
  ]

let get_or_error_test =
  "get_or_error" >::: [
    ("Someの場合" >:: fun _ ->
        assert_equal_int 42 (get_or_error "error" (Some 42)))
    ;
    (* None の場合 *)
  ]

let list_flatten_test =
  "list_flatten" >::: [
    ("None は消され、Some の中身が残る" >:: fun _ ->
        assert_equal_intlist [42; 57] (list_flatten [Some 42; None; Some 57; None]))
  ]

let show_test =
  "show" >::: [
    ("Noneの場合" >:: fun _ ->
        assert_equal_str "None" (ExtOpt.show Fun.id None))
    ;
    ("Someの場合" >:: fun _ ->
        assert_equal_str "Some 42" (ExtOpt.show string_of_int @@ Some 42))
  ]

let _ = run_test_tt_main begin
    "option.ml" >::: [
      get_or_else_test;
      get_or_error_test;
      list_flatten_test;
      show_test;
    ]
  end

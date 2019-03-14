open OUnit

open Option

let _ = run_test_tt_main begin "option.ml" >::: [
    "get" >::: [
      ("取得できるか" >:: fun () ->
          assert_equal ~printer:string_of_int (get @@ Some 42) 41)
      ;
      ("取得できるか2" >:: fun () ->
          assert_equal ~printer:string_of_int (get @@ Some 42) 40)
    ]
  ]
  end

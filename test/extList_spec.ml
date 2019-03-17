open OUnit

open TestUtil
open ExtList


let is_empty_test =
  "is_empty" >::: [
    ("空の場合" >:: fun _ ->
        assert_equal_bool (is_empty []) true)
    ;
    ("空じゃない場合" >:: fun _ ->
        assert_equal_bool (is_empty [1; 2; 3]) false)
  ]

let is_sublist_test =
  "is_sublist" >::: [
    ("サブリストの場合 true" >:: fun _ ->
        assert_equal_bool (is_sublist [2; 4] [1; 2; 3; 4]) true)
    ;
    ("サブリストでない場合 false" >:: fun _ ->
        assert_equal_bool (is_sublist [42; 57] [1; 2; 3; 4]) false)
    ;
    ("順序に依存しないこと" >:: fun _ ->
        assert_equal_bool (is_sublist [4; 2] [1; 2; 3; 4]) true)
    ;
    ("要素が重複する場合は、サブリストでなくても true になり得る" >:: fun _ ->
        assert_equal_bool (is_sublist [2; 2; 4] [1; 2; 3; 4]) true)
    ;
  ]


let contains_test =
  "contains" >::: [
    ("要素に含む場合 true" >:: fun _ ->
        assert_equal_bool (contains 2 [1; 2; 3]) true)
    ;
    ("要素に含まない場合 false" >:: fun _ ->
        assert_equal_bool (contains 42 [1; 2; 3]) false)
  ]

let hd_opt_test =
  "hd_opt" >::: [
    ("空リストでない場合先頭要素を返す" >:: fun _ ->
        assert_equal_intopt (hd_opt [1; 2; 3]) (Some 1))
    ;
    ("空リストの場合デフォルト値を返す" >:: fun _ ->
        assert_equal_intopt (hd_opt []) None)
  ]

let hd_or_else_test =
  "hd_or_else" >::: [
    ("空リストでない場合先頭要素を返す" >:: fun _ ->
        assert_equal_int (hd_or_else 42 [1; 2; 3]) 1)
    ;
    ("空リストの場合デフォルト値を返す" >:: fun _ ->
        assert_equal_int (hd_or_else 42 []) 42)
  ]


let drop_test =
  "drop" >::: [
    ("条件を満たす要素は全て除去される" >:: fun _ ->
        assert_equal_intlist (drop (fun x -> x mod 2 = 0) [1; 2; 3; 2; 2; 5; 4; 2]) [1; 3; 5])
  ]

let drop_one_test =
  "drop_one" >::: [
    ("条件を満たす最初の要素が除去される" >:: fun _ ->
        assert_equal_intlist (drop_one (fun x -> x = 2) [1; 2; 3; 2; 2; 4]) [1; 3; 2; 2; 4])
    ;
    ("一致する値が１つも無ければ何もしない" >:: fun _ ->
        assert_equal_intlist (drop_one (fun x -> x = 2) [1; 3; 4]) [1; 3; 4])
  ]

let count_test =
  "count" >::: [
    ("条件を満たす要素数を返す" >:: fun _ ->
        assert_equal_int 4 (count (fun x -> x mod 2 = 0) [1; 2; 3; 2; 2; 5; 4]))
  ]

let diff_test =
  "diff" >::: [
    ("多重集合と考えて差集合を取る" >:: fun _ ->
        assert_equal_intlist [2; 4] (diff compare [1; 3; 2; 1] [3; 1; 4; 1]))
  ]





let _ = run_test_tt_main begin
    "extList.ml" >::: [
      is_empty_test;
      is_sublist_test;
      contains_test;
      hd_opt_test;
      hd_or_else_test;
      drop_test;
      drop_one_test;
      count_test;
      diff_test;
    ]
  end

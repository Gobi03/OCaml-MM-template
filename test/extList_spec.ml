open OUnit
open TestUtil
open ExtList

let is_empty_test =
  "is_empty"
  >::: [
         ("空の場合" >:: fun _ -> assert_equal_bool true (is_empty []));
         ("空じゃない場合" >:: fun _ -> assert_equal_bool false (is_empty [1; 2; 3]));
       ]

let is_sublist_test =
  "is_sublist"
  >::: [
         ( "サブリストの場合 true" >:: fun _ ->
           assert_equal_bool true (is_sublist [2; 4] [1; 2; 3; 4]) );
         ( "サブリストでない場合 false" >:: fun _ ->
           assert_equal_bool false (is_sublist [42; 57] [1; 2; 3; 4]) );
         ( "順序に依存しないこと" >:: fun _ ->
           assert_equal_bool true (is_sublist [4; 2] [1; 2; 3; 4]) );
         ( "要素が重複する場合は、サブリストでなくても true になり得る"
         >:: fun _ -> assert_equal_bool true (is_sublist [2; 2; 4] [1; 2; 3; 4]) );
       ]

let contains_test =
  "contains"
  >::: [
         ("要素に含む場合 true" >:: fun _ -> assert_equal_bool true (contains 2 [1; 2; 3]));
         ( "要素に含まない場合 false" >:: fun _ ->
           assert_equal_bool false (contains 42 [1; 2; 3]) );
       ]

let hd_opt_test =
  "hd_opt"
  >::: [
         ( "空リストでない場合先頭要素を返す" >:: fun _ ->
           assert_equal_intopt (Some 1) (hd_opt [1; 2; 3]) );
         ( "空リストの場合デフォルト値を返す" >:: fun _ ->
           assert_equal_intopt None (hd_opt []) );
       ]

let hd_or_else_test =
  "hd_or_else"
  >::: [
         ( "空リストでない場合先頭要素を返す" >:: fun _ ->
           assert_equal_int 1 (hd_or_else 42 [1; 2; 3]) );
         ( "空リストの場合デフォルト値を返す" >:: fun _ ->
           assert_equal_int 42 (hd_or_else 42 []) );
       ]

let get_opt_test =
  "get_opt"
  >::: [
         ( "p に該当する値を返す" >:: fun _ ->
           assert_equal_intopt (Some 2) (get_opt (( = ) 2) [1; 2; 3]) );
         ( "p に該当する値が無い場合 None を返す" >:: fun _ ->
           assert_equal_intopt None (get_opt (( = ) 42) [1; 2; 3]) );
         ( "p に該当する値が複数ある場合、最も先頭の値を返す" >:: fun _ ->
           assert_equal_intopt (Some 1) (get_opt (fun n -> n >= 0) [1; 2; 3]) );
       ]

let drop_test =
  "drop"
  >::: [
         ( "条件を満たす要素は全て除去される" >:: fun _ ->
           assert_equal_intlist [1; 3; 5] (drop (fun x -> x mod 2 = 0) [1; 2; 3; 2; 2; 5; 4; 2]) );
       ]

let drop_one_test =
  "drop_one"
  >::: [
         ( "条件を満たす最初の要素が除去される" >:: fun _ ->
           assert_equal_intlist [1; 3; 2; 2; 4] (drop_one (fun x -> x = 2) [1; 2; 3; 2; 2; 4]) );
         ( "一致する値が１つも無ければ何もしない" >:: fun _ ->
           assert_equal_intlist [1; 3; 4] (drop_one (fun x -> x = 2) [1; 3; 4]) );
       ]

let take_out_test =
  "get_opt"
  >::: [
         ( "p に該当する値を取り出す" >:: fun _ ->
           assert_equal_intoptintlist (Some 2, [1; 3]) (take_out (( = ) 2) [1; 2; 3]) );
         ( "p に該当する値が無い場合 None を返す" >:: fun _ ->
           assert_equal_intoptintlist (None, [1; 2; 3]) (take_out (( = ) 42) [1; 2; 3]) );
         ( "p に該当する値が複数ある場合、最も先頭の値を返す" >:: fun _ ->
           assert_equal_intoptintlist (Some 1, [2; 3]) (take_out (fun n -> n >= 0) [1; 2; 3]) );
       ]

let count_test =
  "count"
  >::: [
         ( "条件を満たす要素数を返す" >:: fun _ ->
           assert_equal_int 4 (count (fun x -> x mod 2 = 0) [1; 2; 3; 2; 2; 5; 4]) );
       ]

let diff_test =
  "diff"
  >::: [
         ( "多重集合と考えて差集合を取る" >:: fun _ ->
           assert_equal_intlist [2; 4] (diff compare [1; 3; 2; 1] [3; 1; 4; 1]) );
       ]

let range_test =
  "range"
  >::: [
         ("[bg, ed)のリストを作る" >:: fun _ -> assert_equal_intlist [2; 3; 4] (range (2, 5)));
         ( "(bg >= ed) の時、空リストが返る" >:: fun _ ->
           assert_equal_intlist [] (range (4, 2)) );
       ]

let max_test =
  "max"
  >::: [
         ( "最大値を返す" >:: fun _ ->
           assert_equal_intopt (Some 5) (max compare [1; 2; 3; 2; 2; 5; 4]) );
         ( "空リストの場合 None を返す" >:: fun _ ->
           assert_equal_intopt None (max compare []) );
       ]

let min_test =
  "min"
  >::: [
         ( "最小値を返す" >:: fun _ ->
           assert_equal_intopt (Some 1) (min compare [1; 2; 3; 2; 2; 5; 4]) );
         ( "空リストの場合 None を返す" >:: fun _ ->
           assert_equal_intopt None (min compare []) );
       ]

let _ =
  run_test_tt_main
    ( "extList.ml"
    >::: [
           is_empty_test;
           is_sublist_test;
           contains_test;
           hd_opt_test;
           hd_or_else_test;
           get_opt_test;
           drop_test;
           drop_one_test;
           take_out_test;
           count_test;
           diff_test;
           range_test;
           max_test;
           min_test;
         ] )

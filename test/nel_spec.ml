open OUnit

open TestUtil
open Nel


let of_list_test =
  "of_list" >::: [
    ("複数要素" >:: fun _ ->
        assert_equal (Cons(1, Cons(2, Last 3))) (of_list [1; 2; 3]) )
    ;
    ("単一要素" >:: fun _ ->
        assert_equal (Last 1) (of_list [1]) )
    ;
    (* TODO: 空リストが渡ったケースのテスト *)
  ]

let hd_test =
  "hd" >::: [
    ("先頭要素を返す" >:: fun _ ->
        assert_equal_int 1 (hd @@ of_list [1; 2; 3]) )
    ;
    ("先頭要素を返す2" >:: fun _ ->
        assert_equal_int 1 (hd @@ of_list [1]) )
  ]

let tl_test =
  "tl" >::: [
    ("tail を返す" >:: fun _ ->
        assert_equal (Cons(2, Last 3)) (tl @@ of_list [1; 2; 3]) )
    ;
    (* TODO: 空リストが渡ったケースのテスト *)
  ]

let last_test =
  "last" >::: [
    ("末尾の要素を返す" >:: fun _ ->
        assert_equal_int 3 (last @@ of_list [1; 2; 3]) )
    ;
    ("末尾の要素を返す2" >:: fun _ ->
        assert_equal_int 1 (last @@ of_list [1]) )
  ]

let fold_left_test =
  "fold_left" >::: []

let length_test =
  "length" >::: [
    ("複数要素" >:: fun _ ->
        assert_equal_int 3 (length @@ of_list [1; 2; 3]) )
    ;
    ("単一要素" >:: fun _ ->
        assert_equal_int 1 (length @@ of_list [1]) )
  ]

let rev_test =
  "rev" >::: [
    ("複数要素" >:: fun _ ->
        assert_equal (Cons(3, Cons(2, Last 1))) (rev @@ of_list [1; 2; 3]) )
    ;
    ("単一要素" >:: fun _ ->
        assert_equal (Last 1) (rev @@ of_list [1]) )
  ]

let append_test =
  "append" >::: [
    ("複数要素x複数要素" >:: fun _ ->
        let expected = Cons (1, Cons(2, Cons(3, Last 4))) in
        assert_equal expected (append (of_list [1; 2]) (of_list [3; 4])) )
    ;
    ("単一要素x複数要素" >:: fun _ ->
        let expected = Cons (1, Cons(2, Last 3)) in
        assert_equal expected (append (of_list [1]) (of_list [2; 3])) )
    ;
    ("複数要素x単一要素" >:: fun _ ->
        let expected = Cons (1, Cons(2, Last 3)) in
        assert_equal expected (append (of_list [1; 2]) (of_list [3])) )
  ]

let map_test =
  "map" >::: [
    ("複数要素" >:: fun _ ->
        assert_equal (Cons(2, Cons(3, Last 4))) (map ((+) 1) @@ of_list [1; 2; 3]) )
    ;
    ("単一要素" >:: fun _ ->
        assert_equal (Last 2) (map ((+) 1) @@ of_list [1]) )
  ]

let iter_test =
  "iter" >::: []

let for_all_test =
  "for_all" >::: [
    ("全ての要素が一致した場合" >:: fun _ ->
        assert_equal_bool true (for_all (fun n -> n mod 2 = 0) @@ of_list [2; 4; 6]) )
    ;
    ("一部の要素が一致しない場合" >:: fun _ ->
        assert_equal_bool false (for_all (fun n -> n mod 2 = 0) @@ of_list [1; 2; 3]) )
    ;
    ("末尾の要素が一致しない場合" >:: fun _ ->
        assert_equal_bool false (for_all (fun n -> n <= 2) @@ of_list [2; 4; 7]) )
  ]

let exists_test =
  "exists" >::: [
    ("全ての要素が一致する場合" >:: fun _ ->
        assert_equal_bool true (exists (fun n -> n mod 2 = 0) @@ of_list [2; 4; 6]) )
    ;
    ("一部の要素が一致する場合" >:: fun _ ->
        assert_equal_bool true (exists (fun n -> n mod 2 = 0) @@ of_list [1; 2; 3]) )
    ;
    ("全ての要素が一致しない場合" >:: fun _ ->
        assert_equal_bool false (exists (fun n -> n mod 2 = 0) @@ of_list [1; 3; 5]) )
  ]

let contains_test =
  "contains" >::: [
    ("一部の要素が一致する場合" >:: fun _ ->
        assert_equal_bool true (contains 2 @@ of_list [1; 2; 3]) )
    ;
    ("一致する要素がない場合" >:: fun _ ->
        assert_equal_bool false (contains 7 @@ of_list [1; 2; 3]) )
    ;
    ("複数の要素が一致する場合" >:: fun _ ->
        assert_equal_bool true (contains 2 @@ of_list [1; 2; 3; 2]) )
  ]

let find_test =
  "find" >::: [
    ("条件にマッチする値を返す" >:: fun _ ->
        assert_equal_intopt (Some 2) (find (fun x -> x = 2) (of_list [1; 2; 3])) )
    ;
    ("条件にマッチする先頭の値を返す" >:: fun _ ->
        assert_equal_intopt (Some 1) (find (fun x -> x mod 2 = 1) (of_list [1; 2; 3])) )
    ;
    ("条件にマッチする値が無い場合 None を返す" >:: fun _ ->
        assert_equal_intopt None (find (fun x -> x = 4) (of_list [1; 2; 3])) )
  ]

let filter_test =
  "filter" >::: [
    ("一致する要素を残す" >:: fun _ ->
        assert_equal_intlist [2] (filter (fun n -> n mod 2 = 0) @@ of_list [1; 2; 3]) )
    ;
    ("全ての要素が一致する場合" >:: fun _ ->
        assert_equal_intlist [1; 2; 3] (filter (fun n -> n <= 3) @@ of_list [1; 2; 3]) )
    ;
    ("全ての要素が一致しない場合" >:: fun _ ->
        assert_equal_intlist [] (filter ((=) 0) @@ of_list [1; 2; 3]) )
  ]


let create_test =
  "create" >::: [
    ("複数要素" >:: fun _ ->
        assert_equal (Cons(1, Cons(2, Last 3))) (create [1; 2] 3) )
    ;
    ("単一要素" >:: fun _ ->
        assert_equal (Last 1) (create [] 1) )
  ]

let to_list_test =
  "to_list" >::: [
    ("複数要素" >:: fun _ ->
        assert_equal_intlist [1; 2; 3] (to_list @@ of_list [1; 2; 3]) )
    ;
    ("単一要素" >:: fun _ ->
        assert_equal_intlist [1] (to_list @@ of_list [1]) )
  ]

let sort_test =
  "sort" >::: [
    ("複数要素" >:: fun _ ->
        assert_equal (Cons(1, Cons(2, Last 3))) (sort compare @@ of_list [2; 1; 3]) )
    ;
    ("単一要素" >:: fun _ ->
        assert_equal (Last 1) (sort compare @@ of_list [1]) )
  ]

let max_test =
  "max" >::: [
    ("最大値を返す" >:: fun _ ->
        assert_equal_int 5 (max compare @@ of_list [1; 2; 3; 2; 2; 5; 4]) )
    ;
    ("単一要素の場合" >:: fun _ ->
        assert_equal_int 1 (max compare @@ of_list [1]) )
  ]

let min_test =
  "min" >::: [
    ("最大値を返す" >:: fun _ ->
        assert_equal_int 1 (min compare @@ of_list [1; 2; 3; 2; 2; 5; 4]) )
    ;
    ("単一要素の場合" >:: fun _ ->
        assert_equal_int 1 (min compare @@ of_list [1]) )
  ]



let _ = run_test_tt_main begin
    "nel.ml" >::: [
      of_list_test;
      hd_test;
      tl_test;
      last_test;
      fold_left_test;
      length_test;
      rev_test;
      append_test;
      map_test;
      for_all_test;
      exists_test;
      contains_test;
      iter_test;
      find_test;
      filter_test;
      create_test;
      to_list_test;
      sort_test;
      max_test;
      min_test;
    ]
  end

open OUnit

open TestUtil
open Nel


let nel_of_list_test =
  "nel_of_list" >::: [
    ("複数要素" >:: fun _ ->
        assert_equal (Cons(1, Cons(2, Last 3))) (nel_of_list [1; 2; 3]) )
    ;
    ("単一要素" >:: fun _ ->
        assert_equal (Last 1) (nel_of_list [1]) )
    ;
    (* TODO: 空リストが渡ったケースのテスト *)
  ]

let hd_test =
  "hd" >::: [
    ("先頭要素を返す" >:: fun _ ->
        assert_equal_int 1 (hd @@ nel_of_list [1; 2; 3]) )
    ;
    ("先頭要素を返す2" >:: fun _ ->
        assert_equal_int 1 (hd @@ nel_of_list [1]) )
  ]

let last_test =
  "last" >::: [
    ("末尾の要素を返す" >:: fun _ ->
        assert_equal_int 3 (last @@ nel_of_list [1; 2; 3]) )
    ;
    ("末尾の要素を返す2" >:: fun _ ->
        assert_equal_int 1 (last @@ nel_of_list [1]) )
  ]

let fold_left_test =
  "fold_left" >::: []

let length_test =
  "length" >::: [
    ("複数要素" >:: fun _ ->
        assert_equal_int 3 (length @@ nel_of_list [1; 2; 3]) )
    ;
    ("単一要素" >:: fun _ ->
        assert_equal_int 1 (length @@ nel_of_list [1]) )
  ]

let rev_test =
  "nel_of_list" >::: [
    ("複数要素" >:: fun _ ->
        assert_equal (Cons(3, Cons(2, Last 1))) (rev @@ nel_of_list [1; 2; 3]) )
    ;
    ("単一要素" >:: fun _ ->
        assert_equal (Last 1) (rev @@ nel_of_list [1]) )
  ]

let map_test =
  "map" >::: [
    ("複数要素" >:: fun _ ->
        assert_equal (Cons(2, Cons(3, Last 4))) (map ((+) 1) @@ nel_of_list [1; 2; 3]) )
    ;
    ("単一要素" >:: fun _ ->
        assert_equal (Last 2) (map ((+) 1) @@ nel_of_list [1]) )
  ]


let find_test =
  "nel_of_list" >::: [
    ("条件にマッチする値を返す" >:: fun _ ->
        assert_equal_intopt (Some 2) (find (fun x -> x = 2) (nel_of_list [1; 2; 3])) )
    ;
    ("条件にマッチする先頭の値を返す" >:: fun _ ->
        assert_equal_intopt (Some 1) (find (fun x -> x mod 2 = 1) (nel_of_list [1; 2; 3])) )
    ;
    ("条件にマッチする値が無い場合 None を返す" >:: fun _ ->
        assert_equal_intopt None (find (fun x -> x = 4) (nel_of_list [1; 2; 3])) )
  ]

let create_test =
  "nel_of_list" >::: [
    ("複数要素" >:: fun _ ->
        assert_equal (Cons(1, Cons(2, Last 3))) (create [1; 2] 3) )
    ;
    ("単一要素" >:: fun _ ->
        assert_equal (Last 1) (create [] 1) )
  ]


let _ = run_test_tt_main begin
    "nel.ml" >::: [
      nel_of_list_test;
      hd_test;
      last_test;
      fold_left_test;
      length_test;
      rev_test;
      map_test;
      find_test;
      create_test;
    ]
  end

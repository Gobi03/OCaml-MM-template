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


(* TODO: まだ作ってないテストがままある *)

let _ = run_test_tt_main begin
    "nel.ml" >::: [
      nel_of_list_test;
      hd_test;
      last_test;
    ]
  end

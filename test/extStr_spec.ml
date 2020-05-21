open OUnit
open TestUtil
open ExtStr

let charlist_of_string_test =
  "charlist_of_string"
  >::: [
         ( "文字列に変換できること" >:: fun _ ->
           assert_equal ['a'; 'b'; 'c'] (charlist_of_string "abc") );
       ]

let split_test =
  "split"
  >::: [
         ( "指定されたセパレータで分割できること" >:: fun _ ->
           assert_equal_strlist ["ab"; "c"; "de"] (split ";" "ab;c;de") );
         ( "セパレータが複数文字列の場合" >:: fun _ ->
           assert_equal_strlist ["ab"; "c"; "de"] (split "; " "ab; c; de") );
         ( "セパレータが橋に来た場合" >:: fun _ ->
           assert_equal_strlist [""; "ab"; "c"; "de"; ""] (split ";" ";ab;c;de;") );
       ]

let _ = run_test_tt_main ("extStr.ml" >::: [charlist_of_string_test; split_test])

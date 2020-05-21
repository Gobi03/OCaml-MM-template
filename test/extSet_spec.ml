open OUnit
open TestUtil
open ExtSet

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

module ExtSet = S (IntSet)

let set1 = ExtSet.(of_list [1; 2; 3])

let to_list_spec =
  "to_list"
  >::: [("listへの変換" >:: fun _ -> assert_equal_intlist [1; 2; 3] (ExtSet.to_list set1))]

let contains_spec =
  "contains"
  >::: [
         ("要素に含む場合 true" >:: fun _ -> assert_equal_bool true (ExtSet.contains 2 set1));
         ( "要素に含まない場合 false" >:: fun _ ->
           assert_equal_bool false (ExtSet.contains 42 set1) );
       ]

let add_list_spec =
  "add_list"
  >::: [
         (let set2 = ExtSet.(of_list [1; 2; 3; 4; 5]) in
          "listの要素の追加" >:: fun _ ->
          assert_equal_bool true (ExtSet.equal set2 @@ ExtSet.add_list [3; 4; 5] set1));
       ]

let _ = run_test_tt_main ("extList.ml" >::: [to_list_spec; contains_spec; add_list_spec])

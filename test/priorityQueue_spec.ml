open OUnit

open TestUtil

module IntPQ = PriorityQueue.Make(struct
    type t = int
    let compare = compare
  end)

open IntPQ

let pq1 = empty |> push 3 |> push 5 |> push 1
let pq2 = empty |> push 42

let pop_spec =
  "pop" >::: [
    ("最大値が取れる" >:: fun _ ->
        assert_equal_int 5 (pq1 |> pop |> fst) );
    ("準最大値が取れる" >:: fun _ ->
        assert_equal_int 3 (pq1 |> pop |> snd |> pop |> fst) )
  ]

let size_spec =
  "size" >::: [
    ("正常系" >:: fun _ ->
        assert_equal_int 3 (pq1 |> size) );
    ("空の場合" >:: fun _ ->
        assert_equal_int 0 (pq2 |> pop |> snd |> size) )
  ]

let is_empty_spec =
  "is_empty" >::: [
    ("空の場合" >:: fun _ ->
        assert_equal_bool true (pq2 |> pop |> snd |> is_empty) );
    ("非空の場合" >:: fun _ ->
        assert_equal_bool false (pq2 |> is_empty) );
  ]

let _ = run_test_tt_main begin
    "priorityQueue_spec.ml" >::: [
      pop_spec;
      size_spec;
      is_empty_spec;
    ]
  end

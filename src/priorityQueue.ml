(* TODO: 要テスト *)

module type OrderedType = sig
  type t
  val compare: t -> t -> int
end

module type PQ = sig
  type elt
  type t

  val empty: t
  val push: elt -> t -> t
  val pop: t -> elt * t

  val size: t -> int
  val is_empty: t -> bool
end

module Make(Ord : OrderedType): PQ with type elt = Ord.t = struct

  type elt = Ord.t
  type tree = Lf | Node of elt * tree * tree
  type t = { size: int; tree: tree }

  let empty = { size = 0; tree = Lf }

  exception InternalError
  exception Empty

  let rec make_path: int -> int list = fun n ->
    if n = 0 then []
    else n mod 2 :: make_path ((n-1) / 2)

  let push elm pq =
    let update: elt -> tree -> (elt -> bool) -> elt * tree = fun e child comp ->
      match child with
      | Lf -> raise InternalError
      | Node (e2, left, right) ->
        if comp e2 then (e2, Node (e, left, right))
        else (e, Node (e2, left, right)) in
    let rec func: tree -> int list -> tree = fun tree path ->
      match (path, tree) with
      | ([], Lf) -> Node (elm, Lf, Lf)
      | (dir :: rest, Node (e, left, right)) ->
        if dir = 0 then
          let sub_tree_repr = func right rest in
          let (node, sub_tree) = update e sub_tree_repr (fun x -> Ord.compare e x = -1) in
          Node (node, left, sub_tree)
        else
          let sub_tree_repr = func left rest in
          let (node, sub_tree) = update e sub_tree_repr (fun x -> Ord.compare e x = -1) in
          Node (node, sub_tree, right)
      | _  -> raise InternalError
    in { size = pq.size + 1; tree = func pq.tree (make_path pq.size) }

  let pop pq =
    let rec take: tree -> int list -> elt * tree = fun tree path ->
      match (path, tree) with
      | ([], Node (goal, _, _)) -> (goal, Lf)
      | (dir :: rest, Node (e, left, right)) ->
        if dir = 0 then
          let (last, child) = take right rest in
          (last, Node (e, left, child))
        else
          let (last, child) = take left rest in
          (last, Node (e, child, right))
      | _  -> raise InternalError in
    let rec update: tree -> tree = fun tree ->
      match tree with
      | Lf | Node (_, Lf, Lf) -> tree
      | Node (e, Node (le, llbr, lrbr), Lf) ->
        if Ord.compare e le = -1 then Node (le, Node (e, llbr, lrbr), Lf) else tree
      | Node (e, Lf, Node (re, rlbr, rrbr)) ->
        if compare e re = -1 then Node (re, Lf, Node (e, rlbr, rrbr)) else tree
      | Node (e, Node (le, llbr, lrbr), Node (re, rlbr, rrbr)) ->
        if compare le re = 1 then
          if compare e le = -1 then Node (le, update @@ Node (e, llbr, lrbr), Node (re, rlbr, rrbr))
          else tree
        else
        if compare e re = -1 then Node (re, Node (le, llbr, lrbr), update @@ Node (e, rlbr, rrbr))
        else tree
    in
    match pq.tree with
    | Lf -> raise Empty
    | Node _ ->
      let to_last_path = make_path @@ pq.size - 1 in
      let (last, last_cut_tree) = take pq.tree to_last_path in
      (match last_cut_tree with
       | Lf -> (last, { size = 0; tree = Lf })
       | Node (top, left, right) ->
         let next_tree = update (Node (last, left, right)) in
         (top, { size = pq.size - 1; tree = next_tree })
      )

  let size pq = pq.size
  let is_empty pq = pq.size = 0

end

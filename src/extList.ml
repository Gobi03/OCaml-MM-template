let is_empty: _ list -> bool = function
  | [] -> true
  | _ :: _ -> false

(* l1 が l2 のサブリストかどうかを確認します(順序未考慮).
   l1 の要素に重複がある場合、上手く動きません *)
let is_sublist: 'a list -> 'a list -> bool = fun l1 l2 ->
  List.for_all (fun ele -> List.mem ele l2) l1

let contains: 'a -> 'a list -> bool = List.mem

let hd_opt: 'a list -> 'a option = function
  | [] -> None
  | hd :: _ -> Some hd

let hd_or_else: 'a -> 'a list -> 'a = fun default -> function
  | [] -> default
  | hd :: _ -> hd

let hd_or_error: string -> 'a list -> 'a = fun errmsg -> function
  | [] -> failwith errmsg
  | hd :: _ -> hd

let hd_or_else_lazy: 'a Lazy.t -> 'a list -> 'a = fun default -> function
  | [] -> Lazy.force default
  | hd :: _ -> hd

(* p に該当する最も先頭の値を返す *)
let get_opt: ('a -> bool) -> 'a list -> 'a option = fun p lst ->
  List.fold_left (fun acc x -> if p x then Some x else acc) None (List.rev lst)

(* 第一引数と一致する値を全てリストから削除 *)
let drop: ('a -> bool) -> 'a list -> 'a list = fun p lst ->
  List.filter (fun e -> not (p e)) lst

(* 第一引数と一致する先頭の値を削除 *)
(* 一致する値が１つも無ければ何もしない *)
let drop_one: ('a -> bool) -> 'a list -> 'a list = fun p lst ->
  let rec func acc = function
    | [] -> List.rev acc
    | hd :: rest ->
      if p hd then (List.rev acc) @ rest
      else func (hd :: acc) rest
  in func [] lst

(* p に該当する最も先頭の値を取り出す *)
let take_out: ('a -> bool) -> 'a list -> ('a option * 'a list) = fun p lst ->
  (get_opt p lst, drop_one p lst)

let enumerate: 'a list -> (int * 'a) list = fun lst ->
  List.mapi (fun i e -> (i, e)) lst

let show: ('a -> string) -> 'a list -> string = fun show_elem lst ->
  lst
  |> List.map show_elem
  |> String.concat "; "
  |> Printf.sprintf "[%s]"

let count: ('a -> bool) -> 'a list -> int = fun p lst ->
  List.fold_left (fun acc ele -> acc + if p ele then 1 else 0) 0 lst


(* 多重集合と考えて差集合を取る *)
(* O(NlogN) *)
let diff: ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list = fun compare l1 l2 ->
  let rec func acc l1 l2 =
    match (l1, l2) with
    | (([], rest) | (rest, [])) -> acc @ rest
    | (h1 :: rest1, h2 :: rest2) ->
      if h1 = h2 then func acc rest1 rest2
      else if compare h1 h2 < 0
      then func (h1 :: acc) rest1 (h2::rest2)
      else func (h2 :: acc) (h1::rest1) rest2
  in func [] (List.sort compare l1) (List.sort compare l2)

(* [bg, ed) のリストを作る *)
let range: (int * int) -> int list = fun (bg, ed) ->
  let rec func i acc =
    if i < bg then acc
    else func (i-1) (i :: acc)
  in func (ed-1) []

let max: ('a -> 'a -> int) -> 'a list -> 'a option = fun op lst ->
  match lst with
  | [] -> None
  | hd :: rest -> Some (List.fold_left (fun acc e -> if op acc e >= 0 then acc else e) hd lst)

let min: ('a -> 'a -> int) -> 'a list -> 'a option = fun op lst ->
  match lst with
  | [] -> None
  | hd :: rest -> Some (List.fold_left (fun acc e -> if op e acc >= 0 then acc else e) hd lst)

let shuffle: 'a list -> 'a list = fun lst ->
  lst
  |> List.map (fun e -> (e, Random.bits ()))
  |> List.sort (fun (_, a) (_, b) -> compare a b)
  |> List.map fst

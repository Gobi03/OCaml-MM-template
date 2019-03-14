exception EmptyList

type 'a t = Last of 'a | Cons of 'a * 'a t

let nel_of_list: 'a list -> 'a t = fun lst ->
  match List.rev lst with
  | [] -> raise EmptyList
  | hd :: rest -> List.fold_left (fun acc next -> Cons (next, acc)) (Last hd) rest

(* 第二引数を末尾につける *)
let create: 'a list -> 'a -> 'a t = fun lst last ->
  List.fold_right (fun next acc -> Cons (next, acc)) lst (Last last)

let rev: 'a t -> 'a t = fun nlst ->
  let rec func: 'a t -> 'a t -> 'a t = fun acc -> function
    | Last last -> Cons (last, acc)
    | Cons (hd, rest) -> func (Cons (hd, acc)) rest
  in
  match nlst with
  | Last last -> Last last
  | Cons (hd, rest) -> func (Last hd) rest

let rec get_last: 'a t -> 'a = function
  | Last last -> last
  | Cons (_, rest) -> get_last rest

(* 述語 p を満たす先頭の要素を返す *)
let rec find: ('a -> bool) -> 'a t -> 'a option = fun p nlst ->
  match nlst with
  | Last last -> if p last then Some last else None
  | Cons (hd, rest) -> if p hd then Some hd else find p rest

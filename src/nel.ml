exception EmptyList

type 'a t = Last of 'a | Cons of 'a * 'a t

let nel_of_list: 'a list -> 'a t = fun lst ->
  match List.rev lst with
  | [] -> raise EmptyList
  | hd :: rest -> List.fold_left (fun acc next -> Cons (next, acc)) (Last hd) rest

let hd: 'a t -> 'a = function
  | Cons (h, _) -> h
  | Last last -> last

let rec last: 'a t -> 'a = function
  | Last last -> last
  | Cons (_, rest) -> last rest

let fold_left: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a = fun f ->
  let rec func: 'a -> 'b t -> 'a = fun acc nel ->
    match nel with
    | Last last -> f acc last
    | Cons (hd, rest) -> func (f acc hd) rest
  in func

let length: _ t -> int = fun nel -> fold_left (fun acc _ -> acc + 1) 0 nel

(* 第二引数を末尾につける *)
let create: 'a list -> 'a -> 'a t = fun lst last ->
  List.fold_right (fun next acc -> Cons (next, acc)) lst (Last last)

let rev: 'a t -> 'a t = function
  | Last last -> Last last
  | Cons (hd, rest) -> fold_left (fun acc e -> Cons (e, acc)) (Last hd) rest

(* 述語 p を満たす先頭の要素を返す *)
let rec find: ('a -> bool) -> 'a t -> 'a option = fun p nlst ->
  match nlst with
  | Last last -> if p last then Some last else None
  | Cons (hd, rest) -> if p hd then Some hd else find p rest

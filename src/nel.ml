exception EmptyList

type 'a t = Last of 'a | Cons of 'a * 'a t

let of_list: 'a list -> 'a t = fun lst ->
  match List.rev lst with
  | [] -> raise EmptyList
  | hd :: rest -> List.fold_left (fun acc next -> Cons (next, acc)) (Last hd) rest

let cons: 'a -> 'a t -> 'a t = fun hd tail -> Cons (hd, tail)

let hd: 'a t -> 'a = function
  | Cons (h, _) -> h
  | Last last -> last

let tl: 'a t -> 'a t = function
  | Cons (_, rest) -> rest
  | Last e -> raise EmptyList

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

let rev: 'a t -> 'a t = function
  | Last last -> Last last
  | Cons (hd, rest) -> fold_left (fun acc e -> Cons (e, acc)) (Last hd) rest

let append: 'a t -> 'a t -> 'a t = fun nel1 nel2 ->
  fold_left (fun acc e -> cons e acc) nel2 (rev nel1)

let map: ('a -> 'b) -> 'a t -> 'b t = fun f nel ->
  match nel with
  | Last e -> Last (f e)
  | Cons (hd, rest) -> rev @@ fold_left (fun acc e -> Cons (f e, acc)) (Last (f hd)) rest

let iter: ('a -> unit) -> 'a t -> unit = fun f nel -> ignore @@ map f nel


(** scanning **)
let for_all: ('a -> bool) -> 'a t -> bool = fun p nel ->
  fold_left (fun acc e -> acc && p e) true nel

let exists: ('a -> bool) -> 'a t -> bool = fun p nel ->
  let rec func: 'a t -> bool = function
    | Last e -> p e
    | Cons (hd, rest) -> if p hd then true else func rest
  in func nel

let contains: 'a -> 'a t -> bool = fun e nel ->
  exists (fun a -> e = a) nel

(** searching **)
(* 述語 p を満たす先頭の要素を返す *)
let rec find: ('a -> bool) -> 'a t -> 'a option = fun p nel ->
  match nel with
  | Last e -> if p e then Some e else None
  | Cons (hd, rest) -> if p hd then Some hd else find p rest

let filter: ('a -> bool) -> 'a t -> 'a list = fun p nel ->
  List.rev @@ fold_left (fun acc e -> if p e then e :: acc else acc) [] nel


(* 第二引数を末尾につける *)
let create: 'a list -> 'a -> 'a t = fun lst last ->
  List.fold_right (fun next acc -> Cons (next, acc)) lst (Last last)

let to_list: 'a t -> 'a list = fun nel ->
  List.rev @@ fold_left (fun acc e -> e :: acc) [] nel

let sort: ('a -> 'a -> int) -> 'a t -> 'a t = fun op nel ->
  nel
  |> to_list
  |> List.sort op
  |> of_list

let max: ('a -> 'a -> int) -> 'a t -> 'a = fun op nel ->
  match nel with
  | Last a -> a
  | Cons (hd, rest) -> fold_left (fun acc e -> if op acc e >= 0 then acc else e) hd nel

let min: ('a -> 'a -> int) -> 'a t -> 'a = fun op nel ->
  match nel with
  | Last a -> a
  | Cons (hd, rest) -> fold_left (fun acc e -> if op e acc >= 0 then acc else e) hd nel

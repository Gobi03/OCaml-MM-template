let is_empty: _ list -> bool = function
  | [] -> true
  | _ :: _ -> false

(* l1 が l2 のサブリストかどうかを確認します(順序未考慮).
   l1 の要素に重複がある場合、上手く動きません *)
let is_sublist: 'a list -> 'a list -> bool = fun l1 l2 ->
  List.for_all (fun ele -> List.mem ele l2) l1

let contains: 'a -> 'a list -> bool = fun ele lst ->
  List.exists (fun x -> x = ele) lst

let hd_opt: 'a list -> 'a option = function
  | [] -> None
  | hd :: _ -> Some hd

let hd_or_else: 'a -> 'a list -> 'a = fun default -> function
  | [] -> default
  | hd :: _ -> hd

let hd_or_error: string -> 'a list -> 'a = fun errmsg -> function
  | [] -> failwith errmsg
  | hd :: _ -> hd

(* 第一引数と一致する値を全てリストから削除 *)
let drop: 'a -> 'a list -> 'a list = fun ele lst ->
  List.filter ((<>) ele) lst

(* 第一引数と一致する先頭の値を削除 *)
(* 一致する値が１つも無ければ何もしない *)
let drop_one: 'a -> 'a list -> 'a list = fun ele lst ->
  let rec func acc = function
    | [] -> List.rev acc
    | hd :: rest ->
      if hd = ele then (List.rev acc) @ rest
      else func (hd :: acc) rest
  in func [] lst

let show: ('a -> string) -> 'a list -> string = fun show_elem lst ->
  lst
  |> List.map show_elem
  |> String.concat "; "
  |> Printf.sprintf "[%s]"

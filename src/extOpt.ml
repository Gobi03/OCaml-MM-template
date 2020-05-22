let get_or_else: 'a -> 'a option -> 'a = fun default opt -> 
  Option.value opt ~default:default

let get_or_error msg = function
  | None -> failwith msg
  | Some v -> v

let fold: 'a -> ('b -> 'a) -> 'b option -> 'a = fun default f opt ->
  Option.fold ~none:default ~some:f opt

let list_flatten: 'a option list -> 'a list = fun lst ->
  lst
  |> List.fold_left (fun acc -> function None -> acc | Some e -> e :: acc) []
  |> List.rev

let show: ('a -> string) -> 'a option -> string = fun f -> function
  | None -> "None"
  | Some a -> "Some " ^ f a

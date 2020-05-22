let get_or_else: 'a -> 'a option -> 'a = fun default opt -> 
  Option.value opt ~default:default

let get_or_error msg = function
  | None -> failwith msg
  | Some v -> v

let list_flatten: 'a option list -> 'a list = fun lst ->
  lst
  |> List.map (function None -> [] | Some x -> [x])
  |> List.concat

let show: ('a -> string) -> 'a option -> string = fun f -> function
  | None -> "None"
  | Some a -> "Some " ^ f a

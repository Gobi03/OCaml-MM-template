exception No_value

let may: ('a -> unit) -> 'a option -> unit = fun f -> function
  | None -> ()
  | Some v -> f v

let map f = function
  | None -> None
  | Some v -> Some (f v)

let is_some: _ option -> bool = function
  | None -> false
  | Some _ -> true

let is_none: _ option -> bool = function
  | None -> true
  | Some _ -> false

let get = function
  | None -> raise No_value
  | Some v -> v

let get_or_else v = function
  | None -> v
  | Some v -> v

let get_or_error msg = function
  | None -> failwith msg
  | Some v -> v

let list_flatten: 'a option list -> 'a list = fun lst ->
  lst
  |> List.map (function None -> [] | Some x -> [x])
  |> List.concat

let fold: 'a -> ('b -> 'a) -> 'b option -> 'a = fun default f -> function
  | None -> default
  | Some v -> f v

let show: ('a -> string) -> 'a option -> string = fun f -> function
  | None -> "None"
  | Some a -> "Some " ^ f a

let to_list: 'a option -> 'a list = function
  | None -> []
  | Some x -> [x]

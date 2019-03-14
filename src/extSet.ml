module S(Set: Set.S) = struct
  include Set

  let to_list: t -> elt list = elements
    
  let show: (elt -> string) -> t -> string = fun show_elt set ->
    set
    |> to_list 
    |> List.map show_elt
    |> String.concat ", "
    |> Printf.sprintf "{ %s }"

  let contains: elt -> t -> bool = fun elt set ->
    exists (fun e -> e = elt) set

  let add_list: elt list -> t -> t = fun lst set ->
    List.fold_left (fun acc e -> add e acc) set lst
      
end

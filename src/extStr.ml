open String

let charlist_of_string : string -> char list =
 fun s ->
   let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
   exp (String.length s - 1) []

let split : string -> string -> string list =
 fun sep line ->
   let line_len = length line in
   let sep_len = length sep in
   if sep_len = 0 then failwith "empty separator" else ();
   let rec func i cnt acc =
     if i = line_len - (sep_len - 1) then
       List.rev @@ (sub line (i - cnt) (line_len - (i - cnt)) :: acc)
     else if sub line i sep_len = sep then func (i + sep_len) 0 @@ (sub line (i - cnt) cnt :: acc)
     else func (i + 1) (cnt + 1) acc
   in
   func 0 0 []

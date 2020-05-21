open Common
open Entities

let () =
  let start_time = Sys.time () in
  let _ =
    Printf.sprintf "turn time: %.4fms" @@ ((Sys.time () -. start_time) *. 1000.0)
    (* milli second *)
  in
  ()

(** constants **)
let pi = 3.14159265358979


(** types **)
type coord = { x: int; y: int }

class type hascoord =
  object
    method coord: coord
  end

module IntCoord = struct

  let show: coord -> string = fun { x = x; y = y } ->
    Printf.sprintf "{x=%d; y=%d}" x y

  let get_matrix: coord -> 'a array array -> 'a = fun { x = x; y = y } matrix ->
    matrix.(y).(x)

  let set_matrix: coord -> 'a -> 'a array array -> unit = fun { x = x; y = y } ele matrix ->
    matrix.(y).(x) <- ele


  let (+): coord -> coord -> coord = fun {x=x1; y=y1} {x=x2; y=y2} ->
    { x = x1 + x2; y = y1 + y2 }

  let (-): coord -> coord -> coord = fun {x=x1; y=y1} {x=x2; y=y2} ->
    { x = x1 - x2; y = y1 - y2 }


  let make_4dir: coord -> coord list = fun coord ->
    let delta =
      [ { x = 1; y = 0 }; { x = 0; y = 1 }; { x = -1; y = 0 }; { x = 0; y = -1 } ] in
    List.map ((+) coord) delta

  let make_8dir: coord -> coord list = fun coord ->
    let delta =
      [ { x = 1; y = 0 }; { x = 1; y = 1 }; { x = 0; y = 1 }; { x = -1; y = 1 };
        { x = -1; y = 0 }; { x = -1; y = -1 }; { x = 0; y = -1 }; { x = 1; y = -1 } ] in
    List.map ((+) coord) delta

end



(** ** functions ** **)
let ($) f g = fun x -> f (g x)

let str_of_int = string_of_int
let int_of_str = int_of_string

let rec power: int -> int -> int = fun x n ->
  match n with
  | 0 -> 1
  | 1 -> x
  | n' when n' mod 2 = 1 -> x * power (x * x) (n / 2)
  | n' -> power (x * x) (n / 2)

let pow2 n = n * n

(* perTime 毎にtimeoutを検査する *)
(* @param _timeoutTime 稼働時間(milli seconds)
   @param perTimes 時間計測する頻度
*)
class timeout_checker _timeoutTime perTimes =
  object
    val mutable loopCnt = 0
    val mutable startTime = 0.0
    val timeoutTime = _timeoutTime /. 1000.0

    initializer
      startTime <- Sys.time ()

    method in_time: unit -> bool = fun () ->
      loopCnt <- loopCnt + 1;
      if loopCnt = perTimes then begin
        loopCnt <- 0;
        let nowTime = Sys.time () -. startTime in
        nowTime < timeoutTime
      end
      else true
  end

(* for coord *)
let get_distance: coord -> coord -> float = fun co1 co2 ->
  (pow2 @@ co1.x - co2.x) + (pow2 @@ co1.y - co2.y)
  |> float
  |> sqrt

let in_range: coord -> float -> < coord: coord; ..> -> bool = 
  fun center range obje ->
  get_distance center obje#coord <= range


(** debug **)
let tap: ('a -> unit) -> 'a -> 'a = fun f x ->
  f x; x

let prerr_list: string -> ('a -> string) -> 'a list -> unit = fun msg show lst ->
  prerr_endline @@ Printf.sprintf "%s: %s" msg (ExtList.show show lst)

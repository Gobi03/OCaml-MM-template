(** constants **)
let pi = 3.14159265358979


(** types **)
type coord = { x: int; y: int }

class type hascoord =
  object
    method coord: coord
  end


(** ** functions ** **)
let ($) f g = fun x -> f (g x)

let str_of_int = string_of_int
let int_of_str = int_of_string

let rec power: int -> int -> int = fun x n ->
  match n with
    1 -> x
  | n' when n' mod 2 = 1 -> x * power (x * x) (n / 2)
  | n' -> power (x * x) (n / 2)

let pow2 n = n * n

(* for coord *)
let get_distance: coord -> coord -> float = fun co1 co2 ->
  (pow2 @@ co1.x - co2.x) + (pow2 @@ co1.y - co2.y)
  |> float
  |> sqrt

let in_range: coord -> float -> < coord: coord; ..> -> bool =
  fun center range obje ->
    get_distance center obje#coord <= range

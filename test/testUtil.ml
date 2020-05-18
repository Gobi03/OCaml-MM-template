open OUnit
open Printf


let assert_equal_int = assert_equal ~printer:string_of_int
let assert_equal_bool = assert_equal ~printer:string_of_bool
let assert_equal_str = assert_equal ~printer:(fun s -> sprintf "\"%s\"" s)

let assert_equal_intlist = assert_equal ~printer:(ExtList.show string_of_int)
let assert_equal_strlist = assert_equal ~printer:(ExtList.show (fun s -> sprintf "\"%s\"" s))

let assert_equal_intopt = assert_equal ~printer:(ExtOpt.show string_of_int)

let assert_equal_intoptintlist =
  assert_equal ~printer:(fun (io, il) -> sprintf "(%s, %s)" (ExtOpt.show string_of_int io) (ExtList.show string_of_int il))

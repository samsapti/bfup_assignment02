(*
    Functional Programming - Assignment 2
    Sam Al-Sapti (sals@itu.dk)
    February 12th, 2022
*)


module Programfs

(*
    GREEN
*)

// Exercise 2.1
let rec downto1 n =
    if n <= 0 then
        []
    else
        n :: downto1 (n - 1)
let rec downto2 =
    function
    | n when n <= 0 -> []
    | n -> n :: downto2 (n - 1)

// Exercise 2.2
let rec removeOddIdx =
    function
    | [] -> []
    | x :: xs when xs = [] -> [x]
    | x :: xs -> x :: removeOddIdx xs.Tail

// Exercise 2.3
let rec combinePair =
    function
    | [] -> []
    | _ :: [] -> []
    | x1 :: x2 :: xs -> (x1, x2) :: combinePair xs

// Exercise 2.4
exception Undefined of string
type complex = float * float
let mkComplex : float -> float -> complex = fun a b -> (a, b)
let complexToPair : complex -> float * float = fun c -> c
let (|+|) : complex -> complex -> complex = fun (a, b) (c, d) -> (a + c, b + d)
let (|*|) : complex -> complex -> complex = fun (a, b) (c, d) -> (a * c - b * d, b * c + a * d)
let (|-|) : complex -> complex -> complex = fun (a, b) (c, d) -> (a, b) |+| (-c, -d)
let (|/|) : complex -> complex -> complex = fun (a, b) (c, d) ->
    if not (c = 0.0 || d = 0.0) then
        (a, b) |*| (c / (c ** 2.0 + d ** 2.0), (-d) / (c ** 2.0 + d ** 2.0))
    else
        raise (Undefined "Undefined behavior")
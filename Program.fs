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
    if not (c = 0.0 && d = 0.0) then
        (a, b) |*| (c / (c ** 2.0 + d ** 2.0), (-d) / (c ** 2.0 + d ** 2.0))
    else
        raise (Undefined "Undefined behavior")

// Exercise 2.5
let explode1 : string -> char list = fun s -> List.ofArray (s.ToCharArray())
let rec explode2 =
    function
    | "" -> []
    | s -> s.[0] :: explode2 (s.Remove(0,1))

// Exercise 2.6
let implode : char list -> string = fun cs -> List.foldBack (fun c s -> string c + s) cs ""
let implodeRev : char list -> string = fun cs -> List.fold (fun s c -> string c + s) "" cs

// Exercise 2.7
let toUpper = explode1 >> List.map System.Char.ToUpper >> implode
let toUpper2 = implode << List.map System.Char.ToUpper << explode1
let toUpper3 s = explode1 s |> List.map (System.Char.ToUpper) |> implode

// Exercise 2.8
exception AckNegative of string
let rec ack =
    function
    | (0, n) -> n + 1
    | (m, 0) when m > 0 -> ack (m - 1, 1)
    | (m, n) when m > 0 && n > 0 -> ack (m - 1, ack (m, n - 1))
    | _ -> raise (AckNegative("Negative numbers"))
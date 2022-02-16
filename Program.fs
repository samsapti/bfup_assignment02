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
let explode1 (s : string) = List.ofArray (s.ToCharArray())
let rec explode2 =
    function
    | "" -> []
    | s -> s.[0] :: explode2 (s.Remove(0,1))

// Exercise 2.6
let implode (cs : char list) = List.foldBack (fun c s -> string c + s) cs ""
let implodeRev (cs : char list) = List.fold (fun s c -> string c + s) "" cs

// Exercise 2.7
let toUpper = explode1 >> List.map System.Char.ToUpper >> implode
let toUpper2 = implode << List.map System.Char.ToUpper << explode1 // alternative implementation
let toUpper3 s = explode1 s |> List.map System.Char.ToUpper |> implode // another alternative implementation

// Exercise 2.8
exception AckNegative of string
let rec ack =
    function
    | (0, n) -> n + 1
    | (m, 0) when m > 0 -> ack (m - 1, 1)
    | (m, n) when m > 0 && n > 0 -> ack (m - 1, ack (m, n - 1))
    | _ -> raise (AckNegative("Negative numbers"))


(*
    YELLOW
*)

// Exercise 2.9
let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)

let timeArg1 f a = time (fun () -> f a)

// Exercise 2.10
let downto3 f n e =
    match n with
    | n when n > 0 -> List.foldBack f [1..(n-1)] (f n e)
    | _ -> e

let fac n = downto3 (*) n 1
let range g n = downto3 (fun m e -> g m :: e) n []

// Exercise 2.11
type word = (char * int) list
let hello : word = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)]

// Exercise 2.12
type squareFun = word -> int -> int -> int

let singleLetterScore : squareFun = fun w pos acc -> snd w.[pos] + acc
let doubleLetterScore : squareFun = fun w pos acc -> snd w.[pos] * 2 + acc
let tripleLetterScore : squareFun = fun w pos acc -> snd w.[pos] * 3 + acc

// Exercise 2.13
let doubleWordScore : squareFun = fun _ _ acc -> acc * 2
let tripleWordScore : squareFun = fun _ _ acc -> acc * 3

// Exercise 2.14
let oddConsonants : squareFun = fun w pos acc ->
    let consonants = ['B'; 'C'; 'D'; 'F'; 'G'; 'H'; 'J'; 'K'; 'L'; 'M'; 'N'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'V'; 'W'; 'X'; 'Z']
    let checkOdd = List.fold (fun s l -> if List.contains ((fun (a, b) -> a) l) consonants then not s else s) false
    if checkOdd w then -acc else acc


(*
    RED
*)

// Exercise 2.15
type square = (int * squareFun) list

let SLS : square = [(0, singleLetterScore)]
let DLS : square = [(0, doubleLetterScore)]
let TLS : square = [(0, tripleLetterScore)]

let DWS : square = SLS @ [(1, doubleWordScore)]
let TWS : square = SLS @ [(1, tripleWordScore)]

let calculatePoints : square list -> word -> int = fun squares w ->
    let applyWord i (priority, f) = (priority, f w i)
    let calculate =
        ((List.mapi (fun i sq -> List.map (applyWord i) sq))
        >> (List.fold (fun s sq -> sq @ s) [])
        >> (List.sortBy fst)
        >> (List.map snd)
        >> (List.fold (fun s f -> f s)) 0)
    calculate squares

let calculatePoints2 : square list -> word -> int = fun squares w -> // alternative implementation
    squares
    |> List.mapi (fun i sq -> List.map (fun (priority, f) -> (priority, f w i)) sq)
    |> List.fold (fun s sq -> sq @ s) []
    |> List.sortBy (fun (a, b) -> a)
    |> List.map (fun (a, b) -> b)
    |> List.fold (fun s f -> f s) 0

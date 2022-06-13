module Regex : sig

    type regex = EmptySet
                |EmptyString
                |Character of char
                |Union of regex * regex
                |Concat of regex * regex
                |Star of regex

    val union : regex -> regex -> regex 
    val concat : regex -> regex -> regex
    val evaluate : regex -> bool
    val derive : regex -> char -> regex
    val explode: string -> char list
    val regexmatch: string -> regex -> bool
end = struct
    
    type regex = EmptySet
                |EmptyString
                |Character of char
                |Union of regex * regex
                |Concat of regex * regex
                |Star of regex


    let union r1 r2 =
        match r1, r2 with
            |EmptySet, _ -> r2
            |_, EmptySet -> r1
            |_, _ -> Union(r1, r2)


    let concat r1 r2 = 
        match r1, r2 with
            EmptySet, _ -> r1
            |_, EmptySet -> r2
            |EmptyString, _ -> r2
            |_, EmptyString -> r1
            |_, _ -> Concat(r1, r2)

    let rec evaluate = function
        EmptySet -> false
        |EmptyString -> true
        |Character(_) -> false
        |Union(r1, r2) -> (evaluate r1) || (evaluate r2)
        |Concat(r1, r2) -> (evaluate r1) && (evaluate r2)
        |Star(r) -> true

    let rec derive r c = 
        match r with
            EmptySet -> EmptySet
            |EmptyString -> EmptySet
            |Character(c') -> if c = c' then EmptyString else EmptySet
            |Union(r1, r2) -> union (derive r1 c) (derive r2 c)
            |Concat(r1, r2) -> let x = if evaluate r1 then EmptyString else EmptySet in
                    union (concat (derive r1 c) r2) (concat x (derive r2 c))
            |Star(r) -> concat (derive r c)  (Star(r))


    let explode s =
      let rec exp i l =
        if i < 0 then l else exp (i - 1) (s.[i] :: l) in
      exp (String.length s - 1) []

    let regexmatch s r =
        explode s
        |> List.fold_left derive r
        |> evaluate
end

let r = Regex.Union (Regex.Character('c'), Regex.Concat(Regex.Character('a'), Regex.Star(Regex.Character('b'))))

let z = Regex.regexmatch "a" r
let a = Regex.regexmatch "ab" r
let b = Regex.regexmatch "abb" r
let c = Regex.regexmatch "abbcb" r

let timing_regex = Regex.Concat(Regex.Concat(Regex.Character('a'), Regex.Star(Regex.Union(Regex.Character('b'), Regex.Character('c')))), Regex.Character('c'))
let time s r =
    let t = Sys.time() in
    let result = Regex.regexmatch s r in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    result

let ts = "abbbbbbbbbbbcccccccbbbcbbbbbbbbc"
let y = time ts timing_regex

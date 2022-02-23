module Regex : sig

    type regex

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
            EmptyString, _ -> r1
            |_, EmptyString -> r2
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
        let clist = explode s in
        evaluate (List.fold_left derive r clist)
end

let r = Union (Character('c'), Concat(Character('a'), Star(Character('b'))))

let z = regexmatch "a" r
let a = regexmatch "ab" r
let b = regexmatch "abb" r
let c = regexmatch "abbcb" r

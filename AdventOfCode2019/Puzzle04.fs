module Puzzle04

let private isProperLength (password:string) =
    password.Length = 6



let private hasAdjacentSameDigits (password:string) =
    let rec hasAdjacentSameDigitRecursive  i =
        if i = password.Length - 1 then
            false
        else if password.[i] = password.[i+1] then
            true
        else
            hasAdjacentSameDigitRecursive (i+1)

    hasAdjacentSameDigitRecursive 0


let private hasDecreasingDigits (password: string) =
    let rec helper i =
        if i = password.Length - 1 then
            false
        else if password.[i+1] < password.[i] then
            true
        else
            helper (i+1)

    helper 0

let isValidPassword (password: string) =
    isProperLength password && hasAdjacentSameDigits password && not (hasDecreasingDigits password)

let tap (action : 'T -> unit) (value : 'T) : 'T =
    action value
    value

let countValidPasswordsInRange start stop =
    seq { start..stop }
    |> Seq.map string
    |> Seq.map (fun password -> if isValidPassword password then 1 else 0)
    |> Seq.sum

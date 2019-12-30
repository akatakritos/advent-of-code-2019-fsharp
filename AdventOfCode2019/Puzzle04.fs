module Puzzle04

let private isProperLength (password:string) =
    password.Length = 6



let private hasAdjacentSameDigits (password:string) =
    password
    |> Seq.scan (fun (prev, count) item -> if item = prev then (item, count + 1) else (item, 1)) ('_', 0)
    |> Seq.exists (fun (_, count) -> count > 1)

let private hasDecreasingDigits (password: string) =
    let rec helper i =
        if i = password.Length - 1 then
            false
        else if password.[i+1] < password.[i] then
            true
        else
            helper (i+1)

    helper 0


let sequenceLengths password =
    let mutable previous = '_';
    let mutable count = 0
    seq {
        for c in password do
            if c = previous then
                count <- count + 1
            else
                if (previous <> '_') then yield count
                count <- 1

            previous <- c
        yield count
    }

let hasDuplicateSequenceOfLengthTwo (password: string) =
    sequenceLengths password
    |> Seq.contains 2


let isValidPassword (password: string) =
    isProperLength password && hasAdjacentSameDigits password && not (hasDecreasingDigits password)

let isValidPassword2 (password:string) =
    isProperLength password && hasDuplicateSequenceOfLengthTwo password && not (hasDecreasingDigits password)

let countValidPasswords start stop comparer =
    seq { start..stop }
    |> Seq.sumBy (string >> (fun password -> if comparer password then 1 else 0))



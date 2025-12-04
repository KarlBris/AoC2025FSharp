namespace AoC2025

open Utils

module Day02 =

    let parseInput (input: string) : (int64 * int64) array =
        input |> commas |> Array.map (hyphens >> (Array.map int64) >> twoArrayToTuple)

    let isInvalidId1 (id: string) : bool =
        let length = id.Length

        if length % 2 = 0 then
            let a = id.Substring(0, length / 2)
            let b = id.Substring(length / 2)
            a = b
        else
            false

    let isRepeating (length: int) (id: string) : bool =
        id
        |> Array.ofSeq
        |> Array.splitInto (id.Length / length)
        |> (fun chunk -> Array.forall (fun arr -> arr = Array.head chunk) chunk)

    let isInvalidId2 (id: string) : bool =
        id.Length
        |> divisors
        |> Array.removeLastElement
        |> Array.fold (fun invalid lengthToCheck -> if invalid then invalid else isRepeating lengthToCheck id) false

    let doThing (input: string) (invalidFun: string -> bool) : string =
        input
        |> parseInput
        |> Array.fold (fun sum (a, b) -> sum + ([| a..b |] |> Array.filter (string >> invalidFun) |> Array.sum)) 0L
        |> string

    let part1 (input: string) : string = doThing input isInvalidId1

    let part2 (input: string) : string = doThing input isInvalidId2

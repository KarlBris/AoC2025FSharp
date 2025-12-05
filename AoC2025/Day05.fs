namespace AoC2025

open Utils

module Day05 =

    let parseInput (input: string) : (int64 * int64) array * int64 array =
        let rangeString, idString = input |> splitAtDoubleLines |> twoArrayToTuple

        let ranges =
            rangeString
            |> lines
            |> Array.map (hyphens >> Array.map int64 >> twoArrayToTuple)

        let ids = idString |> lines |> Array.map int64

        (ranges, ids)

    let rec isIdInAnyRange (ranges: (int64 * int64) list) (id: int64) : bool =
        match ranges with
        | [] -> false
        | (l, h) :: rs -> if id >= l && id <= h then true else isIdInAnyRange rs id

    let part1 (input: string) : string =
        let ranges, ids = input |> parseInput

        ids
        |> Array.map (isIdInAnyRange (List.ofArray ranges))
        |> Array.filter id
        |> Array.length
        |> string

    let rec mergeRanges (ranges: (int64 * int64) list) : (int64 * int64) list =
        match ranges with
        | (lowLeft, highLeft) :: (lowRight, highRight) :: rs ->
            if highLeft >= lowRight then
                mergeRanges ((lowLeft, (max highLeft highRight)) :: rs)
            else
                (lowLeft, highLeft) :: (mergeRanges ((lowRight, highRight) :: rs))
        | _ -> ranges

    let part2 (input: string) : string =
        let ranges, _ = input |> parseInput

        ranges
        |> Array.sortBy fst
        |> List.ofArray
        |> mergeRanges
        |> List.map (fun (a, b) -> (b - a) + 1L)
        |> List.sum
        |> string

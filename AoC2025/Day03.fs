namespace AoC2025

open System
open Utils

module Day03 =

    let parseInput (input: string) : int array array =
        input |> lines |> Array.map (Array.ofSeq >> Array.map charToInt)

    let rec selectBatteries (batteriesLeft: int) (bank: int array) : int64 =
        if batteriesLeft = 0 then
            0L
        else
            let multiplier =
                ('1' :: (List.replicate (batteriesLeft - 1) '0')) |> String.Concat |> int64

            let candidateArray = bank |> Array.take ((bank.Length - batteriesLeft) + 1)
            let greatest = candidateArray |> Array.max
            let greatestIndex = bank |> Array.findIndex (fun a -> a = greatest)
            let restOfArray = bank[(greatestIndex + 1) ..]

            ((int64 greatest) * multiplier)
            + selectBatteries (batteriesLeft - 1) restOfArray

    let part1 (input: string) : string =
        input |> parseInput |> Array.map (selectBatteries 2) |> Array.sum |> string

    let part2 (input: string) : string =
        input |> parseInput |> Array.map (selectBatteries 12) |> Array.sum |> string

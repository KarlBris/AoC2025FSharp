namespace AoC2025

open Utils
open System

module Day01 =

    let parseLine (line: string) : int =
        let a = line.Substring(0, 1)
        let b = line.Substring(1)
        let multiplier = if a = "L" then -1 else 1

        (Int32.Parse b) * multiplier

    let parseInput (input: string) : int array = input |> lines |> Array.map parseLine

    let part1 (input: string) : string =
        input
        |> parseInput
        |> Array.fold
            (fun (pos, zeroes) instruction ->
                let newPos = eMod (pos + instruction) 100
                let newZeroes = if newPos = 0 then zeroes + 1 else zeroes
                (newPos, newZeroes))
            (50, 0)
        |> snd
        |> string


    let part2 (input: string) : string =
        input
        |> parseInput
        |> Array.fold
            (fun (pos, zeroes) instruction ->
                let newPos = eMod (pos + instruction) 100

                let newZeroes =
                    if instruction > 0 then
                        (pos + instruction) / 100
                    else
                        ((eMod (100 - pos) 100) - instruction) / 100

                (newPos, zeroes + newZeroes))
            (50, 0)
        |> snd
        |> string

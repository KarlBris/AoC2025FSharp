namespace AoC2025

open System
open Utils

module Day08 =
    type Pos3 = { X: int; Y: int; Z: int }

    let parseInput (input: string) : Pos3 array =
        input
        |> lines
        |> Array.map (commas >> (Array.map int) >> threeArrayToTriple)
        |> Array.map (fun (a, b, c) -> { X = a; Y = b; Z = c })

    let distance (a: Pos3) (b: Pos3) : float =
        Math.Sqrt(
            (((float a.X) - (float b.X)) ** 2)
            + (((float a.Y) - (float b.Y)) ** 2)
            + (((float a.Z) - (float b.Z)) ** 2)
        )

    let rec connectPairs (acc: Pos3 list list) (pairs: (Pos3 * Pos3) list) : Pos3 list list * int64 =
        match pairs with
        | [] -> (acc, 0)
        | (a, b) :: rest ->
            let newAcc =
                match acc |> List.partition (fun l -> List.contains a l || List.contains b l) with
                | [], _ -> ([ a; b ] :: acc)
                | [ listA ], others ->
                    if List.contains a listA && List.contains b listA then acc
                    elif List.contains a listA then ((b :: listA) :: others)
                    else ((a :: listA) :: others)
                | [ listA; listB ], others -> ((List.append listA listB) :: others)
                | _ -> failwith "something went very wrong"

            if newAcc.Length = 1 then
                ([], ((int64 a.X) * (int64 b.X)))
            else
                connectPairs newAcc rest

    let part1 (input: string) : string =
        let boxes = input |> parseInput |> List.ofArray

        boxes
        |> makePairs
        |> List.sortBy (fun (a, b) -> distance a b)
        |> List.take (if boxes.Length > 20 then 1000 else 10)
        |> connectPairs (boxes |> List.map List.singleton)
        |> fst
        |> List.map _.Length
        |> List.sortDescending
        |> List.take 3
        |> List.fold (fun a b -> a * b) 1
        |> string

    let part2 (input: string) : string =
        let boxes = input |> parseInput |> List.ofArray

        boxes
        |> makePairs
        |> List.sortBy (fun (a, b) -> distance a b)
        |> connectPairs (boxes |> List.map List.singleton)
        |> snd
        |> string

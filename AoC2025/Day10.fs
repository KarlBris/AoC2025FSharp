namespace AoC2025

open System
open AoC2025
open Utils

module Day10 =

    let parseInput (input: string) : (int array * int array array * int array) array =
        input
        |> lines
        |> Array.map (fun line ->
            let split = words line
            let (lightStuff, rest) = Array.splitAt 1 split
            let (buttons, joltages) = Array.splitAt (rest.Length - 1) rest

            let lightDiagram =
                lightStuff[0]
                |> (fun s -> s.Substring(1, s.Length - 2))
                |> Seq.mapi (fun i c -> if c = '#' then Some i else None)
                |> Seq.choose id
                |> Array.ofSeq

            let buttonWiringSchematics =
                buttons
                |> Array.map (fun bs -> bs.Substring(1, bs.Length - 2) |> commas |> Array.map int)

            let joltageRequirements =
                joltages[0].Substring(1, joltages[0].Length - 2) |> commas |> Array.map int

            (lightDiagram, buttonWiringSchematics, joltageRequirements))

    let toggleLamps (currentLamps: int array) (toToggle: int array) : int array =
        toToggle
        |> Array.fold
            (fun lamps lamp ->
                match Array.tryFindIndex (fun e -> e = lamp) lamps with
                | None -> Array.append [| lamp |] lamps
                | Some value -> Array.removeAt value lamps)
            currentLamps
        |> Array.sort

    let rec howFewPresses
        (acc: int)
        (targetDiagram: int array)
        (currentLamps: int array)
        (presses: int array list)
        : int option =
        if targetDiagram = currentLamps then
            Some acc
        else
            match presses with
            | [] -> None
            | lights :: rest ->
                let newLamps = toggleLamps currentLamps lights
                howFewPresses (acc + 1) targetDiagram newLamps rest

    let fewestButtonPresses ((lightDiagram, buttonWirings, _): (int array * int array array * int array)) : int =
        let hmm = buttonWirings |> List.ofArray |> permutations |> Array.ofSeq

        hmm |> Array.choose (fun l -> howFewPresses 0 lightDiagram [||] l) |> Array.min

    let part1 (input: string) : string =
        input |> parseInput |> Array.map fewestButtonPresses |> Array.sum |> string

    let part2 (input: string) : string = input

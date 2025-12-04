namespace AoC2025

open Utils

module Day04 =

    let parseInput (input: string) : Map<Position, Unit> =
        input
        |> lines
        |> Array.mapi (fun y line ->
            line
            |> Array.ofSeq
            |> Array.mapi (fun x c -> if c = '@' then Some(x, y) else None)
            |> Array.choose id)
        |> Array.concat
        |> Array.map (fun p -> p, ())
        |> Map.ofArray

    let hasLessThan4Neighbors (allRolls: Map<Position, Unit>) (pos: Position) : bool =
        (allNeighborPositions
         |> Array.map (fun relativeNeighborPos ->
             let neighborPos = addPos pos relativeNeighborPos
             Map.tryFind neighborPos allRolls)
         |> Array.choose id
         |> Array.length) < 4

    let part1 (input: string) : string =
        input
        |> parseInput
        |> (fun ps -> Array.filter (hasLessThan4Neighbors ps) (ps |> Map.keys |> Array.ofSeq))
        |> Array.length
        |> string

    let rec findAndRemoveRolls (acc: int) (rolls: Map<Position, Unit>) : int =
        let rollsToRemove =
            Array.filter (hasLessThan4Neighbors rolls) (rolls |> Map.keys |> Array.ofSeq)

        let numberOfRollsToRemove = Array.length rollsToRemove

        if numberOfRollsToRemove = 0 then
            acc
        else
            let newRollMap = rolls |> Map.removeMany rollsToRemove

            findAndRemoveRolls (acc + numberOfRollsToRemove) newRollMap

    let part2 (input: string) : string =
        input |> parseInput |> findAndRemoveRolls 0 |> string

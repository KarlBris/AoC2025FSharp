namespace AoC2025

open Utils

module Day07 =
    let findPositions (line: string) : int array =
        line
        |> Seq.mapi (fun i c -> if c = '^' then Some i else None)
        |> Array.ofSeq
        |> Array.choose id

    let parseInput (input: string) : int * int array array =
        let a = input |> lines
        let startPos = a[0] |> Seq.findIndex ((=) 'S')
        let layers = a |> Array.tail |> Array.map findPositions
        (startPos, layers)

    let rec splitBeams (beams: int Set) (acc: int) (splitters: int list list) : int =
        match splitters with
        | [] -> acc
        | splitterLine :: rest ->
            let willBeSplit, willNotBeSplit =
                beams
                |> Set.partition (fun p -> splitterLine |> List.contains p)

            let hasBeenSplit =
                willBeSplit
                |> Set.map (fun i -> Set.ofArray [| i - 1; i + 1 |])
                |> Set.unionMany

            splitBeams (Set.union hasBeenSplit willNotBeSplit) (acc + willBeSplit.Count) rest

    let rec splitBeams2 (beams: Map<int, int64>) (splitters: int list list) : int64 array =
        match splitters with
        | [] -> beams |> Map.values |> Seq.toArray
        | splitterLine :: rest ->
            let willBeSplit, willNotBeSplit =
                beams
                |> Map.toArray
                |> Array.partition (fun (i, _) -> splitterLine |> List.contains i)

            let hasBeenSplit =
                willBeSplit
                |> Array.collect (fun (i, value) -> [| (i - 1, value); (i + 1, value) |])

            let combined =
                willNotBeSplit
                |> Array.append hasBeenSplit
                |> Array.fold
                    (fun m (i, v) ->
                        m
                        |> Map.change i (fun a ->
                            match a with
                            | None -> Some v
                            | Some value -> Some(value + v)))
                    (Map.empty: Map<int, int64>)

            splitBeams2 combined rest

    let part1 (input: string) : string =
        let startPos, layers = input |> parseInput

        splitBeams (Set.singleton startPos) 0 (layers |> Array.map Array.toList |> Array.toList)
        |> string

    let part2 (input: string) : string =
        let startPos, layers = input |> parseInput

        splitBeams2 ([ startPos, 1L ] |> Map.ofList) (layers |> Array.map Array.toList |> Array.toList)
        |> Array.map int64
        |> Array.sum
        |> string

namespace AoC2025

open Utils

module Day06 =

    let parseInput (input: string) : int64 array array * char array =
        let stringLines = input |> lines
        let operators = stringLines |> Array.last |> words |> Array.map char

        let numbers =
            stringLines |> Array.removeLastElement |> Array.map (words >> Array.map int64)

        numbers, operators

    let mathTheNumbers (numbers: int64 array) (operator: char) : int64 =
        let op, start = if operator = '+' then (+), 0L else (*), 1L
        Array.fold op start numbers

    let part1 (input: string) : string =
        let numbers, operators = parseInput input

        numbers
        |> Array.transpose
        |> Array.zip operators
        |> Array.map (fun (o, ns) -> mathTheNumbers ns o)
        |> Array.sum
        |> string

    let parseInput2 (input: string) : int64 array array * char array =
        let stringLines = input |> lines
        let operators = stringLines |> Array.last |> words |> Array.map char

        let numbers =
            stringLines
            |> Array.removeLastElement
            |> Array.map Array.ofSeq
            |> Array.transpose
            |> Array.map (fun s -> s |> Seq.ofArray |> Seq.map string |> String.concat "")
            |> Array.map _.Trim()
            |> String.concat "\n"
            |> splitAtDoubleLines
            |> Array.map (lines >> Array.map int64)

        numbers, operators

    let part2 (input: string) : string =
        let numbers, operators = parseInput2 input

        Array.zip numbers operators
        |> Array.map (fun (ns, o) -> mathTheNumbers ns o)
        |> Array.sum
        |> string

namespace AoC2025

open System.IO
open System.Diagnostics
open System

module TestRunner =

    let getInput day =
        let filename = $"..\..\..\inputs\input_day{day}.txt"

        if File.Exists filename then
            filename
            |> File.ReadAllText
            |> String.filter (fun c -> c <> '\r')
        else
            failwith $"Input file {filename} not found"

    let makeComparison (expectedResults: string []) (results: string []) =
        Array.zip results expectedResults
        |> Array.map (fun (r, e) -> (r, e, r = e))

    let printStatus ((res, expectedRes, success): string * string * bool) =
        printfn "%s! Got %s, expected %s." (if success then "Success" else "Failure") res expectedRes

    let formatTime (span: TimeSpan) : string =
        $"{span.Hours} Hours, {span.Minutes} Minutes, {span.Seconds} Seconds, {span.Milliseconds}.{span.Microseconds} Milliseconds"

    let run (examples: string []) expectedResults realInput (func: string -> string) title =
        printfn title

        if examples.Length = 0 then
            printfn "No examples found, running the real input..."
        else
            printfn "Running and verifying examples before the real input..."

        let resultList =
            examples
            |> Array.map func
            |> makeComparison expectedResults

        resultList |> Array.map printStatus |> ignore

        let examplesSuccessful =
            resultList
            |> Array.fold (fun b1 (_, _, b2) -> b1 && b2) true

        if examplesSuccessful then
            printfn "All examples were successful, running the real input..."
            let timer = new Stopwatch()
            timer.Start()
            printfn "Result from real input: %s" (func realInput)
            timer.Stop()
            printfn $"Time elapsed: {(formatTime timer.Elapsed)}"
            printfn "Time elapsed: %A" timer.Elapsed
        else
            printfn "Some examples were not successful. PLEASE DO BETTER"

        printfn ""

    // Day1
    let input1() = getInput 1

    let examples1_1 = [| "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82" |]

    let exampleResults1_1 = [| "3" |]

    let examples1_2 = examples1_1

    let exampleResults1_2 = [| "6" |]

    // Day2
    let input2() = getInput 2

    let examples2_1 = [| "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124" |]

    let exampleResults2_1 = [| "1227775554" |]

    let examples2_2 = examples2_1

    let exampleResults2_2 = [| "4174379265" |]

    // Day3
    let input3() = getInput 3

    let examples3_1 = [| "" |]

    let exampleResults3_1 = [| "" |]

    let examples3_2 = [| "" |]

    let exampleResults3_2 = [| "" |]

    // Day4
    let input4() = getInput 4

    let examples4_1 = [| "" |]

    let exampleResults4_1 = [| "" |]

    let examples4_2 = [| "" |]

    let exampleResults4_2 = [| "" |]

    // Day5
    let input5() = getInput 5

    let examples5_1 = [| "" |]
    
    let exampleResults5_1 = [| "" |]
    
    let examples5_2 = [| "" |]

    let exampleResults5_2 = [| "" |]

    // Day6
    let input6() = getInput 6

    let examples6_1 = [| "" |]

    let exampleResults6_1 = [| "" |]

    let examples6_2 = [| "" |]

    let exampleResults6_2 = [| "" |]

    // Day7
    let input7() = getInput 7

    let examples7_1 = [| "" |]

    let exampleResults7_1 = [| "" |]

    let examples7_2 = [| "" |]

    let exampleResults7_2 = [| "" |]

    // Day8
    let input8() = getInput 8

    let examples8_1 = [| "" |]

    let exampleResults8_1 = [| "" |]

    let examples8_2 = [| "" |]

    let exampleResults8_2 = [| "" |]

    // Day9
    let input9() = getInput 9

    let examples9_1 = [| "" |]

    let exampleResults9_1 = [| "" |]

    let examples9_2 = [| "" |]

    let exampleResults9_2 = [| "" |]

    // Day10
    let input10() = getInput 10

    let examples10_1 = [| "" |]

    let exampleResults10_1 = [| "" |]

    let examples10_2 = [| "" |]

    let exampleResults10_2 = [| "" |]

    // Day11
    let input11() = getInput 11

    let examples11_1 = [| "" |]

    let exampleResults11_1 = [| "" |]

    let examples11_2 = [| "" |]

    let exampleResults11_2 = [| "" |]

    // Day12
    let input12() = getInput 12

    let examples12_1 = [| "" |]

    let exampleResults12_1 = [| "" |]

    let examples12_2 = [| "" |]

    let exampleResults12_2 = [| "" |]
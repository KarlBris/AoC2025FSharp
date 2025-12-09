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
            let timer = Stopwatch()
            timer.Start()
            printfn $"Result from real input: %s{func realInput}"
            timer.Stop()
            printfn $"Time elapsed: {formatTime timer.Elapsed}"
            printfn $"Time elapsed: %A{timer.Elapsed}"
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

    let examples3_1 = [| "987654321111111\n811111111111119\n234234234234278\n818181911112111" |]

    let exampleResults3_1 = [| "357" |]

    let examples3_2 = examples3_1

    let exampleResults3_2 = [| "3121910778619" |]

    // Day4
    let input4() = getInput 4

    let examples4_1 = [| "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@." |]

    let exampleResults4_1 = [| "13" |]

    let examples4_2 = examples4_1

    let exampleResults4_2 = [| "43" |]

    // Day5
    let input5() = getInput 5

    let examples5_1 = [| "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32" |]
    
    let exampleResults5_1 = [| "3" |]
    
    let examples5_2 = examples5_1

    let exampleResults5_2 = [| "14" |]

    // Day6
    let input6() = getInput 6

    let examples6_1 = [| "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  " |]

    let exampleResults6_1 = [| "4277556" |]

    let examples6_2 = examples6_1

    let exampleResults6_2 = [| "3263827" |]

    // Day7
    let input7() = getInput 7

    let examples7_1 = [| ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n..............." |]

    let exampleResults7_1 = [| "21" |]

    let examples7_2 = examples7_1

    let exampleResults7_2 = [| "40" |]

    // Day8
    let input8() = getInput 8

    let examples8_1 = [| "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689" |]

    let exampleResults8_1 = [| "40" |]

    let examples8_2 = examples8_1

    let exampleResults8_2 = [| "25272" |]

    // Day9
    let input9() = getInput 9

    let examples9_1 = [| "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3" |]

    let exampleResults9_1 = [| "50" |]

    let examples9_2 = examples9_1

    let exampleResults9_2 = [| "24" |]

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
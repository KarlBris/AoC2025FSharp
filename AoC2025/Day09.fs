namespace AoC2025

open System
open Utils

module Day09 =
    let parseInput (input: string) : Position array =
        input |> lines |> Array.map (commas >> Array.map int >> twoArrayToTuple)

    let area ((aX, aY): Position, (bX, bY): Position) : int64 =
        (abs ((int64 aX) - (int64 bX)) + 1L) * (abs ((int64 aY) - (int64 bY)) + 1L)

    let part1 (input: string) : string =
        input
        |> parseInput
        |> List.ofArray
        |> makePairs
        |> List.map area
        |> List.sortDescending
        |> List.head
        |> string

    let rec fourWise (elements: 'a list) : ('a * 'a * 'a * 'a) list =
        match elements with
        | a :: b :: c :: d :: rest -> (a, b, c, d) :: (fourWise (b :: c :: d :: rest))
        | _ -> []

    let isSaddleOrVertical (((aX, aY), (bX, bY), (cX, cY), (dX, dY)): Position * Position * Position * Position) : bool =
        if bY = cY then
            (aY < bY && dY > bY) || (aY > bY && dY < bY)
        else
            true

    let rec isPointOnALine (lines: (Position * Position) list) (point as (pX, pY): Position) : (Position * Position) option =
        match lines with
        | [] -> None
        | ((aX, aY), (bX, bY)) :: ls ->
            let isOnThisLine =
                if pX = aX then pY >= (min aY bY) && pY <= (max aY bY)
                elif pY = aY then pX >= (min aX bX) && pX <= (max aX bX)
                else false

            if isOnThisLine then Some ((aX, aY), (bX, bY)) else isPointOnALine ls point

    let rec lineIntersections (start : int) (lines: (Position * Position) list) (point as (pX, pY): Position) : int =
        if start >= pX
            then 0
            else
                let onLine = isPointOnALine lines (start,pY)
                match onLine with
                | None -> lineIntersections (start+1) lines point
                | Some ((aX, _), (bX, _)) ->
                    let lineWidth =
                        if aX = bX then 1
                        else abs (aX-bX)
                    1 + lineIntersections (start + lineWidth+1) lines point
    
    let shortenLineIfVertical (line as ((aX, aY), (bX, bY)):(Position * Position)) : (Position * Position) option =
        if aY = bY then
           Some line
        else
            if aX < bX then // aX above
                let aY' = aY + 1
                let bY' = bY - 1
                if aY' > bY' then None else Some ((aX, aY'), (bX, bY'))
            else // bX above
                let aY' = aY - 1
                let bY' = bY + 1
                if aY' < bY' then None else Some ((aX, aY'), (bX, bY'))

    let isRectangleLegal (positions: Position array) (allLines: (Position * Position) list) (shortenedLines: (Position * Position) list) ((a,b) as ((aX, aY): Position, (bX, bY): Position)) : bool =
        let (c,d) = (aX, bY), (bX, aY)
        
        printfn $"Rect {(a,b)}"
        
        if aX = 9 && aY = 5 && bX = 2 && bY = 3 then
            ()
         
        let outline = [(makeLineBetween a c); (makeLineBetween c b); (makeLineBetween b d); (makeLineBetween d a); ] |> List.concat
        
        let areAllOutlinePositionsInside =
            outline
            |> List.map (fun pos ->
                if fst pos = 2 && snd pos = 7 then
                    ()
                let isOnLine = isPointOnALine allLines pos |> Option.isSome
                if isOnLine then true
                    else
                        let intersections = lineIntersections 0 shortenedLines pos
                        (intersections % 2) = 1
                )
            |> List.forall (fun b -> b = true)
        printfn $"areAllOutlinePositionsInside {areAllOutlinePositionsInside}"
        
        // let isOneProspectiveCornerInTheList =
        //     (prospectiveCorners
        //      |> Array.filter (fun pP -> Array.contains pP positions)
        //      |> Array.length) > 1

        let isAnyPositionInsideTheRectangle =
            positions
            |> Array.map (fun (pX, pY) -> pX > (min aX bX) && pX < (max aX bX) && pY > (min aY bY) && pY < (max aY bY))
            |> Array.contains true

        printfn $"isAnyPositionInsideTheRectangle {isAnyPositionInsideTheRectangle}"
        areAllOutlinePositionsInside && (not isAnyPositionInsideTheRectangle)
                
    let part2 (input: string) : string =
        let positions = input |> parseInput

        let lastTwoPositions = positions |> Array.rev |> Array.take 2 |> Array.rev
        let firstPosition = Array.head positions

        let allLines = [|[|Array.last positions|];positions|] |> Array.concat |> Array.pairwise |> List.ofArray
        
        let saddleLines =
            [| lastTwoPositions; positions; [| firstPosition |] |]
            |> Array.concat
            |> List.ofArray
            |> fourWise
            |> List.choose (fun (a, b, c, d) -> if isSaddleOrVertical (a, b, c, d) then Some(b, c) else None)

        let shortenedLines = saddleLines |> List.choose shortenLineIfVertical
        
        positions
        |> List.ofArray
        |> makePairs
        |> List.filter (isRectangleLegal positions allLines shortenedLines)
        |> List.map (fun ps -> area ps, ps)
        |> List.sortByDescending fst
        |> List.map fst
        |> List.head
        |> string

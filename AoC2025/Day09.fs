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

    let rec isPointOnALine (lines: (Position * Position) list) (point as (pX, pY): Position) : bool =
        match lines with
        | [] -> false
        | ((aX, aY), (bX, bY)) :: ls ->
            let isOnThisLine =
                pX >= (min aX bX) && pX <= (max aX bX) && pY >= (min aY bY) && pY <= (max aY bY)
                // if pX = aX then pY >= (min aY bY) && pY <= (max aY bY)
                // elif pY = aY then pX >= (min aX bX) && pX <= (max aX bX)
                // else false

            if isOnThisLine then true else isPointOnALine ls point

    let crossProduct ((aX, aY): Position, (bX, bY): Position, (cX, cY): Position) : int =
        ((bX - aX) * (cY - aY)) - ((bY - aY) * (cX - aX))
    
    let isPointInsidePolygon (lines: (Position * Position) list) ((pX,pY): Position) : bool =
        let windingNumber =
            lines
            |> List.fold (fun wn ((aX,aY),(bX,bY)) ->
            let cross = -crossProduct ((aX,aY),(bX,bY),(pX,pY))
            
            if aY <= pY then
                if aY > pY && cross > 0 then (wn + 1) else wn
            else
                if bY <= pY && cross < 0 then (wn - 1) else wn
            ) 0
        
        windingNumber <> 0
    
    // let isPointInsidePolygonRev (lines: (Position * Position) list) ((pX,pY): Position) : bool =
    //     let windingNumber =
    //         lines
    //         |> List.fold (fun wn ((aX,aY),(bX,bY)) ->
    //         let cross = crossProduct ((aX,aY),(bX,bY),(pX,pY))
    //         
    //         if aY <= pY then
    //             if aY > pY && cross > 0 then (wn + 1) else wn
    //         else
    //             if bY <= pY && cross < 0 then (wn - 1) else wn
    //         ) 0
    //     
    //     windingNumber <> 0
    
    let rec arePositionsInside(allLines: (Position * Position) list) (positions: Position list) :  bool=
        match positions with
        | [] ->true
        | pos::ps ->
                let isOnLine = isPointOnALine allLines pos
                //let isOnLine = false
                if isOnLine then
                    arePositionsInside allLines ps
                else
                    let isInside = isPointInsidePolygon allLines pos
                    if isInside then
                        arePositionsInside allLines ps else false
        
    let isRectangleLegal (allLines: (Position * Position) list)((a,b) as ((aX, aY): Position, (bX, bY): Position)) :bool =
        let (c,d) = (aX, bY), (bX, aY)
        
        let topLeft = (min aX bX, min aY bY)
        let topRight = (max aX bX, min aY bY)
        let bottomLeft = (min aX bX, max aY bY)
        let bottomRight = (max aX bX, max aY bY)
        
        let topLeftDirs = manhattanNeighborPositions |> Array.map (fun (p,d) -> addPos topLeft p, d) |> Array.filter (fun (_,d) -> d = E || d = S) |> Array.map fst
        let topRightDirs = manhattanNeighborPositions |> Array.map (fun (p,d) -> addPos topRight p, d) |> Array.filter (fun (_,d) -> d = W || d = S) |> Array.map fst
        let bottomLeftDirs = manhattanNeighborPositions |> Array.map (fun (p,d) -> addPos bottomLeft p, d) |> Array.filter (fun (_,d) -> d = E || d = N) |> Array.map fst
        let bottomRightDirs = manhattanNeighborPositions |> Array.map (fun (p,d) -> addPos bottomRight p, d) |> Array.filter (fun (_,d) -> d = W || d = N) |> Array.map fst
        let isPrelimValid = [| topLeftDirs; topRightDirs; bottomLeftDirs; bottomRightDirs |] |> Array.concat |> List.ofArray |> arePositionsInside allLines
        if not isPrelimValid then false
            else 
        
                let area = area (a,b)
                
                let timer = System.Diagnostics.Stopwatch.StartNew()
                
                
                
                let outline = [(makeLineBetween a c); (makeLineBetween c b); (makeLineBetween b d); (makeLineBetween d a); ] |> List.concat
                let areAllOutlinePositionsInside = arePositionsInside allLines outline
                
                
                
                timer.Stop()
                // printfn $"Rect {(a,b)}"
                // printfn $"memoMap: {memoMap.Count}"
                // printfn $"area: {area}"
                // printfn $"outline: {outline.Length}"
                // printfn $"{timer.Elapsed}"
                areAllOutlinePositionsInside
                
    let part2 (input: string) : string =
        let positions = input |> parseInput
        let revPositions = input |> parseInput |> Array.rev

        let crossTest1 = crossProduct ((0,0),(5,0),(3,1))
        let crossTest2 = crossProduct ((5,0),(0,0),(3,1))
        
        let allLines = [|[|Array.last positions|];positions|] |> Array.concat |> Array.pairwise |> List.ofArray
        let revAllLines = [|[|Array.last revPositions|];revPositions|] |> Array.concat |> Array.pairwise |> List.ofArray
        
        let allPointsInSpace =
            [ for a in 0 .. 13 do
                for b in 0 .. 8 do
                 yield (a, b) ]
        
        let allPositions  = allLines |> List.map (fun (a,b) -> makeLineBetween a b) |> List.concat
        
        let drawMap = allPositions |> List.map (fun p -> p, '#') |> Map.ofList
        printMap (19,8) drawMap
        printfn "\n"
        let insideMap = allPointsInSpace |> List.filter
                                                (fun p ->
            let p' = addPos (-1,0) p
            isPointInsidePolygon allLines p'
            //|| isPointOnALine allLines p
                        )
                        |> List.map (fun p -> p, 'o') |> Map.ofList
        printMap (19,8) insideMap
        
        "12"
        
        // let pairs =
        //     positions
        //     |> List.ofArray
        //     |> makePairs
        // printfn $"{pairs.Length}"
        //
        // pairs
        // |> List.fold (fun (legalRects, counter) rect ->
        //     if counter % 100 = 0 then printfn $"{counter}/{pairs.Length}"
        //     let (isLegal) = isRectangleLegal allLines rect
        //     if isLegal then (rect::legalRects, counter+1) else (legalRects, counter+1)) ([], 0)
        // |> fst
        // |> List.map (fun ps -> area ps, ps)
        // |> List.sortByDescending fst
        // |> List.map fst
        // |> List.head
        // |> string
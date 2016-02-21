module ToyGame.Maze


type T = {
    width : int
    height : int
    adj : Map<int*int, (int*int) list>
}


let cells w h =
    seq { for x in 0..w-1 do
          for y in 0..h-1 do
          yield (x, y) }


let adj maze cell = Map.find cell maze.adj

let adjacentCells w h (x, y) = seq {
    if y > 0 then yield (x, y-1)
    if x > 0 then yield (x-1, y)
    if x < w-1 then yield (x+1, y)
    if y < h-1 then yield (x, y+1) } |> List.ofSeq


let empty w h = {
    width = w
    height = h
    adj = cells w h |> Seq.map (fun c -> (c, adjacentCells w h c)) |> Map.ofSeq
    }


let cellsAdjacent (x1, y1) (x2, y2) =
    abs (x1 - x2) + abs (y1 - y2) = 1


let assertValidAdj adjmap c1 c2 =
    if not (cellsAdjacent c1 c2) then
        failwith <| sprintf "%s and %s are not adjacent" (string c1) (string c2)
    if not (Map.containsKey c1 adjmap) then
        failwith <| sprintf "%s is not a valid cell" (string c1)
    if not (Map.containsKey c2 adjmap) then
        failwith <| sprintf "%s is not a valid cell" (string c2)

let addAdj adjmap c1 c2 =
    let c1v = c2 :: (Map.find c1 adjmap) |> Seq.distinct |> List.ofSeq
    let c2v = c1 :: (Map.find c2 adjmap) |> Seq.distinct |> List.ofSeq
    adjmap |> Map.add c1 c1v |> Map.add c2 c2v

let delAdj adjmap c1 c2 =
    let c1v = Map.find c1 adjmap |> List.filter ((=) c2)
    let c2v = Map.find c2 adjmap |> List.filter ((=) c1)
    adjmap |> Map.add c1 c1v |> Map.add c2 c2v


let addWall maze (c1, c2) = { maze with adj = delAdj maze.adj c1 c2 }



let rec buildAdjFromList adjmap = function
    | [] -> adjmap
    | (c1, c2) :: adjlist ->
        assertValidAdj adjmap c1 c2
        buildAdjFromList (addAdj adjmap c1 c2) adjlist


let load w h adjlist =
    let adjmap = cells w h |> Seq.map (fun a -> (a, [])) |> Map.ofSeq
    {
        width = w
        height = h
        adj = buildAdjFromList adjmap adjlist
    }


let notIn set item = not <| Set.contains item set

let checkPath maze cell1 cell2 =
    let rec checkPath' seen = function
        | cell when cell = cell2 -> true
        | cell -> adj maze cell |> Seq.filter (notIn seen) |>
                  Seq.exists (checkPath' (Set.add cell seen))
    checkPath' Set.empty cell1

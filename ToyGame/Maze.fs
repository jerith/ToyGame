module ToyGame.Maze

type Tile =
    | Empty
    | Wall
    | Border

type T = {
    width : int
    height : int
    cells : Map<int*int, Tile>
}


// Coordinates are 1-based and there is a border of closed cells around the
// maze.


let cellKeys w h =
    seq { for x in 1..w do
          for y in 1..h do
          yield (x, y) }

let borderKeys w h =
    seq { yield! seq { for x in 0..(w+1) -> (x, 0) }
          yield! seq { for y in 1..h do yield (0, y); yield (w+1, y) }
          yield! seq { for x in 0..(w+1) -> (x, h+1) } }

let cells maze = cellKeys maze.width maze.height
let border maze = borderKeys maze.width maze.height


let getCell maze cell =
    Map.find cell maze.cells

let neighbours (x, y) = [(x-1, y); (x+1, y); (x, y-1); (x, y+1)]


let empty w h = {
    width = w
    height = h
    cells = seq {
    yield! cellKeys w h |> Seq.map (fun cell -> cell, Empty)
    yield! borderKeys w h |> Seq.map (fun cell -> cell, Border)
    } |> Map.ofSeq
}


let notIn set item = not <| Set.contains item set

let checkPath cell1 cell2 maze =
    let rec checkPath' seen = function
        | cell when getCell maze cell <> Empty -> false
        | cell when cell = cell2 -> true
        | cell ->
            neighbours cell |> Seq.filter (notIn seen) |>
            Seq.exists (checkPath' (Set.add cell seen))
    checkPath' Set.empty cell1

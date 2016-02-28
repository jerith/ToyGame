module ToyGame.Maze


type T = {
    width : int
    height : int
    adj : Map<int*int, Set<int*int>>
}


let cells w h =
    seq { for x in 0..w-1 do
          for y in 0..h-1 do
          yield (x, y) }

let allowedBridges w h (x, y) = seq {
    if y > 0 then yield (x, y-1)
    if x > 0 then yield (x-1, y)
    if x < w-1 then yield (x+1, y)
    if y < h-1 then yield (x, y+1) } |> Set.ofSeq

let collectEdge edges = function
    | (c1, c2) when Set.contains (c2, c1) edges -> edges
    | edge -> Set.add edge edges

let edges w h = Seq.fold collectEdge Set.empty <| seq {
    for c in cells w h do
    yield! Seq.map (fun x -> (c, x)) (allowedBridges w h c) }

let validBridge maze (c1, c2) =
    allowedBridges maze.width maze.height c1 |> Set.contains c2

let adj maze cell = Map.find cell maze.adj

let empty w h =
    { width = w; height = h;
      adj = cells w h |> Seq.map (fun x -> (x, Set.empty)) |> Map.ofSeq }


let addBridge maze (c1, c2) =
    let addBridge' c1 c2 = Map.add c1 (Map.find c1 maze.adj |> Set.add c2)
    { maze with adj = maze.adj |> addBridge' c1 c2 |> addBridge' c2 c1 }

let getBridges maze =
    let collectAdj c1 bs c2 = collectEdge bs (c1, c2)
    let cellBridges bs (c, adj) = Set.fold (collectAdj c) bs adj
    Map.toSeq maze.adj |> Seq.fold cellBridges Set.empty

let getWalls maze =
    let collectAdj c1 bs c2 = collectEdge bs (c1, c2)
    let cellWalls ws (c, adj) =
        allowedBridges maze.width maze.height c
        |> Seq.filter (fun c2 -> not <| Set.contains c2 adj)
        |> Seq.fold (collectAdj c) ws
    Map.toSeq maze.adj |> Seq.fold cellWalls Set.empty

let create w h bridges =
    Seq.fold addBridge (empty w h) bridges

let fullyOpen w h = create w h <| edges w h



// let checkPath maze cell1 cell2 =
//     let rec checkPath' seen = function
//         | cell when cell = cell2 -> true
//         | cell -> adj maze cell |> Seq.filter (notIn seen) |>
//                   Seq.exists (checkPath' (Set.add cell seen))
//     checkPath' Set.empty cell1


module PQ =
    let empty = Set.empty
    let singleton = Set.singleton

    let isEmpty = Set.isEmpty
    let count = Set.count

    let min pq = Set.minElement pq |> snd
    let max pq = Set.maxElement pq |> snd

    let isIn pq item = Set.exists (fun (_, a) -> a = item) pq

    let rm item pq = Set.filter (fun (_, a) -> a <> item) pq
    let add (cost, item) pq = Set.add (cost, item) pq
    let adds citems pq = Seq.fold (fun pq citem -> add citem pq) pq citems


let checkPath maze cell1 cell2 =
    let dist (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))
    let withdist = Seq.map (fun c -> dist c cell2, c)
    let notIn set = Seq.filter (fun c -> not <| Set.contains c set)
    let rec checkPath' seen = function
        | cells when PQ.isEmpty cells -> false
        | cells ->
            match PQ.min cells with
                | cell when cell = cell2 -> true
                | cell ->
                    cells |> PQ.rm cell
                    |> PQ.adds (adj maze cell |> notIn seen |> withdist)
                    |> checkPath' (Set.add cell seen)
    checkPath' Set.empty (PQ.singleton (dist cell1 cell2, cell1))


module GrowingTree =

    let rec grow addActive (rnd: System.Random) (maze: T) seen = function
        | active when PQ.isEmpty active -> maze
        | active ->
            let cell = PQ.min active
            let cs =
                allowedBridges maze.width maze.height cell |> List.ofSeq
                |> List.filter (fun c -> not <| Set.contains c seen)
            match List.length cs with
                | 0 -> grow addActive rnd maze seen (PQ.rm cell active)
                | _ ->
                    let cell2 = List.nth cs (rnd.Next(List.length cs))
                    grow addActive rnd
                        (addBridge maze (cell, cell2))
                        (Set.add cell2 seen)
                        (addActive cell2 active)

    let gen w h =
        let cf rnd active = - (PQ.count active)
        let rnd = System.Random()
        let addActive cell pq = PQ.add (cf rnd pq, cell) pq
        let startCell = (rnd.Next(w), rnd.Next(h))
        grow addActive rnd
            (empty w h)
            (Set.singleton startCell)
            (addActive startCell PQ.empty)

module ToyGameTests.TestMaze

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open ToyGame

type MazeSize = MazeSize of int*int
type Maze = Maze of Maze.T
type FullyOpenMaze = FullyOpenMaze of Maze.T
type FullyClosedMaze = FullyClosedMaze of Maze.T
type OpenMazeWithPath = OpenMazeWithPath of Maze.T*(int*int)*(int*int)
type ClosedMazeWithPath = ClosedMazeWithPath of Maze.T*(int*int)*(int*int)


module MazeGen =

    let isqrt = float >> sqrt >> int

    let genSize = Gen.sized <| fun s -> Gen.choose (2, isqrt s + 2)

    let genCell (maze: Maze.T) =
        let w, h = maze.width, maze.height
        Gen.map2 (fun a b -> (a, b)) (Gen.choose (0, w-1)) (Gen.choose (0, h-1))

    let genMazeSize = Gen.map MazeSize (Gen.two genSize)

    let genMaze = gen {
        let! MazeSize (w, h) = genMazeSize
        let! bridges = Gen.listOf <| Gen.elements (Maze.edges w h)
        return Maze (Maze.create w h bridges) }

    let genFullyOpenMaze = gen {
        let! MazeSize (w, h) = genMazeSize
        return FullyOpenMaze (Maze.fullyOpen w h) }

    let genFullyClosedMaze = gen {
        let! MazeSize (w, h) = genMazeSize
        return FullyClosedMaze (Maze.empty w h) }

    let genOpenMazeWithPath = gen {
        let! FullyOpenMaze m = genFullyOpenMaze
        let! c1, c2 = Gen.two (genCell m)
        return OpenMazeWithPath (m, c1, c2) }

    let genClosedMazeWithPath = gen {
        let! FullyClosedMaze m = genFullyClosedMaze
        let! c1, c2 = Gen.two (genCell m)
        return ClosedMazeWithPath (m , c1, c2) }

    // Some boilerplate so we can use this in attributes.
    type Arb =
        static member MazeSize = Arb.fromGen genMazeSize
        static member Maze = Arb.fromGen genMaze
        static member FullyOpenMaze = Arb.fromGen genFullyOpenMaze
        static member FullyClosedMaze = Arb.fromGen genFullyClosedMaze
        static member OpenMazeWithPath = Arb.fromGen genOpenMazeWithPath
        static member ClosedMazeWithPath = Arb.fromGen genClosedMazeWithPath


let cellsAdjacent (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2) = 1

let bridgeSymmetric maze c1 c2 = Maze.adj maze c2 |> Set.contains c1

let adjListIsValid maze cell =
    let adj = Maze.adj maze cell
    "adj cells touching" @| (Seq.forall (cellsAdjacent cell) adj) .&.
    "bridge symmetric"   @| (Seq.forall (bridgeSymmetric maze cell) adj)

let cellIsValid (maze:Maze.T) (x, y) =
    let w, h = maze.width, maze.height
    sprintf "cell (%d, %d)" x y @| [
        "cell within bounds" @| (x >= 0 && x < w && y >= 0 && y < h)
        "adj cells valid"    @| (adjListIsValid maze (x, y))]

let checkCellsValid (maze:Maze.T) =
    let cells = Map.toList maze.adj |> List.map (fun (c, _) -> c)
    "cells valid"       @| [for c in cells -> cellIsValid maze c] .&.
    "all cells present" @| (maze.width * maze.height = List.length cells)


let cornerCells w h = [0, 0; w-1, 0; 0, h-1; w-1, h-1]

let edgeCells w h = seq {
    yield! [for x in 1..w-2 do yield x, 0; yield x, h-1]
    yield! [for y in 1..h-2 do yield 0, y; yield w-1, y]}

let insideCells w h = seq {
    for x in 1..w-2 do for y in 1..h-2 do yield (x, y)}


[<Property(Arbitrary=[|typeof<MazeGen.Arb>|])>]
let testFullyOpenMaze (FullyOpenMaze m) =
    let adjCount' n cell = (Maze.adj m cell |> Seq.length) = n
    let adjCount n cells = Seq.forall (adjCount' n) cells
    "a fully open maze has all cells connected to their neighbours" @| [
        "maze valid" @| (checkCellsValid m)
        "all adj cells present" @| [
            "corner" @| (cornerCells m.width m.height |> adjCount 2)
            "edge"   @| (edgeCells m.width m.height |> adjCount 3)
            "inside" @| (insideCells m.width m.height |> adjCount 4)]]


[<Property(Arbitrary=[|typeof<MazeGen.Arb>|])>]
let testMaze (Maze m) =
    let adjCount' n cell = (Maze.adj m cell |> Seq.length) = n
    let adjCount n cells = Seq.forall (adjCount' n) cells
    "a maze contains valid bidirectional bridges" @| [
        "maze valid" @| (checkCellsValid m)]


[<Property(Arbitrary=[|typeof<MazeGen.Arb>|])>]
let testFullyOpenMazePath (OpenMazeWithPath (m, c1, c2)) =
    "all paths are open in a fully open maze" @|
    (Maze.checkPath m c1 c2)


[<Property(Arbitrary=[|typeof<MazeGen.Arb>|])>]
let testFullyClosedMazePath (ClosedMazeWithPath (m, c1, c2)) =
    "only single-cell paths are open in a fully closed maze" @|
    (Maze.checkPath m c1 c2 = (c1 = c2))

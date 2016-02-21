module ToyGameTests.TestMaze

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open ToyGame

type MazeSize = MazeSize of int*int
type MazeSizeWithTwoCells = MazeSizeWithTwoCells of (int*int)*(int*int)*(int*int)

let isqrt = float >> sqrt >> int

let genSize = Gen.sized <| fun s -> Gen.choose (2, isqrt s + 2)

let genCell (w, h) =
    Gen.map2 (fun a b -> (a, b)) (Gen.choose (0, w-1)) (Gen.choose (0, h-1))

type MazeGenerators =
    static member MazeSize = Arb.fromGen (Gen.map MazeSize (Gen.two genSize))
    static member MazeSizeWithTwoCells = Arb.fromGen (gen {
        let! MazeSize (w, h) = Arb.generate<MazeSize>
        let! c1 = genCell (w, h)
        let! c2 = genCell (w, h)
        return MazeSizeWithTwoCells ((w, h), c1, c2)
    })


let adjListIsValid maze cell =
    let adj = Maze.adj maze cell
    "adj cells unique"   @| (Set.count <| Set.ofList adj = List.length adj) .&.
    "adj cells touching" @| (List.forall (Maze.cellsAdjacent cell) adj)

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


[<Property(Arbitrary=[|typeof<MazeGenerators>|])>]
let testEmptyMaze (MazeSize (w, h)) =
    let m = Maze.empty w h
    let adjCount' n cell = (Maze.adj m cell |> List.length) = n
    let adjCount n cells = Seq.forall (adjCount' n) cells
    "empty maze has all cells connected to their neighbours" @| [
        "maze valid" @| (checkCellsValid m)
        "all adj cells present" @| [
            "corner" @| (cornerCells w h |> adjCount 2)
            "edge"   @| (edgeCells w h |> adjCount 3)
            "inside" @| (insideCells w h |> adjCount 4)]]


[<Property(Arbitrary=[|typeof<MazeGenerators>|])>]
let testEmptyMazePath (MazeSizeWithTwoCells ((w, h), c1, c2)) =
    Maze.checkPath (Maze.empty w h) c1 c2


let assertPath w h c1 c2 walls result =
    not result


[<Test>]
let testPathExamples001()  =
    let maze = Maze.load 1 1 []
    Assert.That(Maze.checkPath maze (0, 0) (0, 0))

[<Test>]
let testPathExamples002()  =
    let maze = Maze.load 2 1 []
    Assert.That(not (Maze.checkPath maze (0, 0) (1, 0)))

[<Test>]
let testPathExamples003()  =
    let maze = Maze.load 2 1 [(0, 0), (1, 0)]
    Assert.That(Maze.checkPath maze (0, 0) (1, 0))



// [<TestCase(0,0, 0,0, Result=false, Description="border to itself")>]
// [<TestCase(0,0, 0,1, Result=false, Description="border to border")>]
// [<TestCase(0,1, 1,1, Result=false, Description="border to adjacent empty")>]
// [<TestCase(1,1, 0,1, Result=false, Description="empty to adjacent border")>]
// [<TestCase(1,1, 1,1, Result=true, Description="empty to itself")>]
// [<TestCase(1,1, 2,1, Result=true, Description="empty to adjacent empty")>]
// let testPathExamples x1 y1 x2 y2 =
//     let maze = Maze.empty 2 1
//     Maze.checkPath (x1, y1) (x2, y2) maze

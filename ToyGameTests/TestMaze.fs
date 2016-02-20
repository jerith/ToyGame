module ToyGameTests.TestMaze

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open ToyGame


let forallCells maze f cells =
    Seq.forall (fun cell -> f (Maze.getCell maze cell)) cells


let checkShape w h (maze:Maze.T) =
    "width"  @| (maze.width = w) .&.
    "height" @| (maze.height = h) .&.
    "count"  @| (Map.toList maze.cells |> List.length = ((w+2) * (h+2)))


[<Property>]
let testEmptyMaze (PositiveInt w, PositiveInt h) =
    let m = Maze.empty w h
    "empty maze has only Empty/Border cells" @| [
        "shape"  @| (checkShape w h m)
        "cells"  @| (forallCells m ((=) Maze.Empty) (Maze.cells m))
        "border" @| (forallCells m ((=) Maze.Border) (Maze.border m))]


let xIn xs (x, _) = Seq.contains x xs
let yIn ys (_, y) = Seq.contains y ys

let isBorderCell w h cell =
    (xIn (seq {0..w+1}) cell && yIn [0; h+1] cell) ||
    (xIn [0; w+1] cell && yIn (seq {0..h+1}) cell)


[<Property>]
let testCells (PositiveInt w, PositiveInt h) =
    let cells = Maze.empty w h |> Maze.cells |> Set.ofSeq
    let xs = seq { 1..w } |> Set.ofSeq
    let ys = seq { 1..h } |> Set.ofSeq
    "Maze.cells returns a seq of all non-border cells" @| [
        "count"  @| (Set.count cells = w * h)
        "x vals" @| (cells |> Set.forall (xIn <| seq { 1..w }))
        "y vals" @| (cells |> Set.forall (yIn <| seq { 1..h }))]


[<Property>]
let testBorder (PositiveInt w, PositiveInt h) =
    let cells = Maze.empty w h |> Maze.border |> Set.ofSeq
    "Maze.border returns a seq of all border cells" @| [
        "count" @| (Set.count cells = 2 * (w + h + 2))
        "vals"  @| (cells |> Set.forall (isBorderCell w h))]


[<TestCase(0,0, 0,0, Result=false, Description="border to itself")>]
[<TestCase(0,0, 0,1, Result=false, Description="border to border")>]
[<TestCase(0,1, 1,1, Result=false, Description="border to adjacent empty")>]
[<TestCase(1,1, 0,1, Result=false, Description="empty to adjacent border")>]
[<TestCase(1,1, 1,1, Result=true, Description="empty to itself")>]
[<TestCase(1,1, 2,1, Result=true, Description="empty to adjacent empty")>]
let testPathExamples x1 y1 x2 y2 =
    let maze = Maze.empty 2 1
    Maze.checkPath (x1, y1) (x2, y2) maze

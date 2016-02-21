namespace ToyGame

open UnityEngine

module MS =
    let V3 (x, y) z = new Vector3(float32 x, float32 y, float32 z)

    let spawnCube size pos =
        let cube = GameObject.CreatePrimitive(PrimitiveType.Cube)
        cube.transform.localScale <- (V3 size 1)
        cube.transform.position <- (V3 pos 0)
        cube

    let offset gap n =
        let n = float n
        ((n - 1.0) * (gap + 1.0)) / 2.0

    let pos (maze: Maze.T) gap (x, y) =
        let l = 1.0 + gap
        (x * l - (offset gap maze.width),
         y * l - (offset gap maze.height))

    let bridgePos maze gap ((x1, y1), (x2, y2)) =
        pos maze gap (float (x1 + x2) / 2.0, float (y1 + y2) / 2.0)

    let spawnCell maze gap (x, y) =
        let c = spawnCube (1.0, 1.0) (pos maze gap (float x, float y))
        if (x, y) = (0, 0) then
            c.GetComponent<Renderer>().material.color <- Color.red
        if (x, y) = (maze.width-1, maze.height-1) then
            c.GetComponent<Renderer>().material.color <- Color.blue

    let spawnBridge maze gap ((x1, y1), (x2, y2) as bridge) =
        let size = match x1 = x2, y1 = y2 with
                   | true, false -> (1.0, gap)
                   | false, true -> (gap, 1.0)
                   | _ -> failwith "Bad bridge."
        spawnCube size (bridgePos maze gap bridge) |> ignore

    let spawnMaze (maze: Maze.T) gap =
        Maze.cells maze.width maze.height |> Seq.iter (spawnCell maze gap)
        Maze.getBridges maze |> Seq.iter (spawnBridge maze gap)


type MainScript() =
    inherit MonoBehaviour()

    // Inspector pane property
    [<SerializeField>]
    let mutable mazeWidth = 10

    [<SerializeField>]
    let mutable mazeHeight = 10

    [<SerializeField>]
    let mutable gap = 0.1

    member this.genMaze () =
        Maze.GrowingTree.gen mazeWidth mazeHeight

    member this.Start() =
        let m = this.genMaze ()
        MS.spawnMaze m gap

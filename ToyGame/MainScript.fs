namespace ToyGame

open UnityEngine

module MS =
    type Params = {
        maze : Maze.T
        gap : float
        go : GameObject
    }

    let V3 (x, y) z = new Vector3(float32 x, float32 y, float32 z)

    let spawnCube p pos =
        let cube = GameObject.CreatePrimitive(PrimitiveType.Cube)
        cube.transform.SetParent(p.go.transform, false)
        cube.transform.localPosition <- (V3 pos 0)
        cube

    let pos p (x, y) = (x * (1.0 + p.gap), y * (1.0 + p.gap))

    let bridgePos p ((x1, y1), (x2, y2)) =
        pos p (float (x1 + x2) / 2.0, float (y1 + y2) / 2.0)

    let spawnCell p (x, y) =
        let cube = spawnCube p (pos p (float x, float y))
        if (x, y) = (0, 0) then
            cube.GetComponent<Renderer>().material.color <- Color.red
        if (x, y) = (p.maze.width-1, p.maze.height-1) then
            cube.GetComponent<Renderer>().material.color <- Color.blue

    let spawnBridge p ((x1, y1), (x2, y2) as bridge) =
        let size = match x1 = x2, y1 = y2 with
                   | true, false -> (1.0, p.gap)
                   | false, true -> (p.gap, 1.0)
                   | _ -> failwith "Bad bridge."
        let cube = spawnCube p (bridgePos p bridge)
        cube.transform.localScale <- (V3 size 1)

    let transformMaze p =
        let w = float p.maze.width * (p.gap + 1.0) - p.gap
        let h = float p.maze.height * (p.gap + 1.0) - p.gap
        let c = Array.get Camera.allCameras 0 // Assume we only have one camera.
        let csize = float c.orthographicSize * 0.95
        let scalex = csize * 2.0 * (float c.aspect) / w
        let scaley = csize * 2.0 / h
        let scale = min scalex scaley
        p.go.transform.localScale <- (V3 (scale, scale) 1)

        let offset l = -(l - 1.0) * scale / 2.0
        p.go.transform.localPosition <- (V3 (offset w, offset h) 0)

    let spawnMaze (maze: Maze.T) gap =
        let p = { maze = maze; gap = gap; go = new GameObject("maze") }
        transformMaze p
        Maze.cells maze.width maze.height |> Seq.iter (spawnCell p)
        Maze.getBridges maze |> Seq.iter (spawnBridge p)


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

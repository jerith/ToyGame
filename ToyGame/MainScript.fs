﻿namespace ToyGame

open UnityEngine


type IMainScript =
    abstract genMaze: unit -> unit
    abstract clearMaze: unit -> unit
    abstract getWidth: unit -> int
    abstract setWidth: int -> unit
    abstract getHeight: unit -> int
    abstract setHeight: int -> unit


module MazeUtil =
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
        cube.name <- sprintf "Cell (%d, %d)" x y
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
        cube.name <- sprintf "Bridge (%d, %d) <-> (%d, %d)" x1 y1 x2 y2
        cube.transform.localScale <- (V3 size 1)

    let transformMaze p =
        let w = float p.maze.width * (p.gap + 1.0) - p.gap
        let h = float p.maze.height * (p.gap + 1.0) - p.gap
        let c = Array.get Camera.allCameras 0 // Assume we only have one.
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
        p.go


module UIUtil =

    let mutable res = new UI.DefaultControls.Resources()
    let _gbers s = UnityEditor.AssetDatabase.GetBuiltinExtraResource<Sprite>(s)
    res.standard <- _gbers "UI/Skin/UISprite.psd"
    res.background <- _gbers "UI/Skin/Background.psd"
    res.inputField <- _gbers "UI/Skin/InputFieldBackground.psd"
    res.knob <- _gbers "UI/Skin/Knob.psd"

    let mkButton text handler =
        let button = UI.DefaultControls.CreateButton(res)
        button.name <- sprintf "Button: %s" text
        (button.GetComponent<UI.Button>()).onClick.AddListener(
            new Events.UnityAction(handler))
        let btext = button.GetComponentInChildren<UI.Text>()
        btext.font <- Font.CreateDynamicFontFromOSFont([|"Arial"|], 14)
        btext.text <- text
        button

    let mkSlider text min max value handler =
        let slider = UI.DefaultControls.CreateSlider(res)
        slider.name <- sprintf "Slider: %s" text
        let sslider = slider.GetComponent<UI.Slider>()
        sslider.wholeNumbers <- true
        sslider.minValue <- float32 min
        sslider.maxValue <- float32 max
        sslider.value <- float32 value
        sslider.onValueChanged.AddListener(
            new Events.UnityAction<float32>(handler))
        slider

    let addPanel (parent: GameObject) =
        let panel = new GameObject("Panel")
        panel.transform.SetParent(parent.transform, false)
        let rect = panel.AddComponent<RectTransform>()
        rect.anchorMin <- new Vector2(0.0f, 0.0f)
        rect.anchorMax <- new Vector2(0.0f, 1.0f)
        rect.sizeDelta <- new Vector2(70.0f, 1.0f)
        rect.anchoredPosition <- new Vector2(rect.sizeDelta.x / 2.0f, 0.0f)
        panel

    let rec addToPanel (panel: GameObject) yoffset = function
        | [] -> ()
        | (widget: GameObject) :: widgets ->
            widget.transform.SetParent(panel.transform, false)
            let rect = widget.GetComponent<RectTransform>()
            rect.anchorMin <- new Vector2(0.0f, 1.0f)
            rect.anchorMax <- new Vector2(1.0f, 1.0f)
            rect.sizeDelta <- new Vector2(0.0f, rect.sizeDelta.y)
            rect.anchoredPosition <- new Vector2(
                0.0f, -rect.sizeDelta.y / 2.0f - yoffset)
            addToPanel panel (yoffset + rect.sizeDelta.y + 5.0f) widgets


    let buildUI (main: IMainScript) =
        let canvas = (GameObject.FindObjectOfType<Canvas>()).gameObject
        let panel = addPanel canvas

        let bGenerate = mkButton "generate" main.genMaze
        let bClear = mkButton "clear" main.clearMaze
        let sWidth =
            mkSlider "width" 2 30 (main.getWidth()) (int >> main.setWidth)
        let sHeight =
            mkSlider "height" 2 30 (main.getHeight()) (int >> main.setHeight)

        addToPanel panel 0.0f [bGenerate; bClear; sWidth; sHeight]

        canvas


type GameMaze = GameMaze of Maze.T*GameObject


type MainScript() =
    inherit MonoBehaviour()

    // Inspector pane property
    [<SerializeField>]
    let mutable mazeWidth = 10

    [<SerializeField>]
    let mutable mazeHeight = 10

    [<SerializeField>]
    let mutable gap = 0.1

    let mutable maze: GameMaze option = None

    interface IMainScript with
        member this.genMaze () =
            match maze with
                | Some _ -> Debug.Log("Maze already exists!")
                | None ->
                    let m = Maze.GrowingTree.gen mazeWidth mazeHeight
                    let go = MazeUtil.spawnMaze m gap
                    maze <- Some (GameMaze (m, go))

        member this.clearMaze () =
            match maze with
                | None -> ()
                | Some (GameMaze (m, go)) ->
                    GameObject.Destroy(go)
                    maze <- None

        member this.getWidth () = mazeWidth
        member this.setWidth w = mazeWidth <- w
        member this.getHeight () = mazeHeight
        member this.setHeight h = mazeHeight <- h

    member this.Start() =
        let canvas = UIUtil.buildUI this
        let cam = Array.get Camera.allCameras 0 // Assume we only have one.
        cam.orthographicSize <- 240.0f

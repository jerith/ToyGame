namespace ToyGame

open UnityEngine

module P = Primitives


type GameMaze = GameMaze of Maze.T*GameObject
type GamePlayer = {
    mutable pos: int*int
    go: GameObject
}

type IMainScript =
    abstract GenMaze: unit -> unit
    abstract ClearMaze: unit -> unit
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

    let mazeColor = Color.white
    let startColor = (Color.red / 2.0f) + (Color.white / 2.0f)
    let endColor = (Color.blue / 2.0f) + (Color.white / 2.0f)

    let spawnCube p pos =
        let cube = GameObject.CreatePrimitive(PrimitiveType.Cube)
        cube.transform.SetParent(p.go.transform, false)
        cube.transform.localPosition <- (P.V3 pos 0)
        P.setMaterial cube mazeColor
        cube

    let pos p (x, y) = (x * (1.0 + p.gap), y * (1.0 + p.gap))

    let bridgePos p ((x1, y1), (x2, y2)) =
        pos p (float (x1 + x2) / 2.0, float (y1 + y2) / 2.0)

    let spawnCell p (x, y) =
        let cube = spawnCube p (pos p (float x, float y))
        cube.name <- sprintf "Cell (%d, %d)" x y
        if (x, y) = (0, 0) then P.setMaterial cube startColor
        if (x, y) = (p.maze.width-1, p.maze.height-1) then
            P.setMaterial cube endColor

    let spawnBridge p ((x1, y1), (x2, y2) as bridge) =
        let size = match x1 = x2, y1 = y2 with
                   | true, false -> (1.0, p.gap)
                   | false, true -> (p.gap, 1.0)
                   | _ -> failwith "Bad bridge."
        let cube = spawnCube p (bridgePos p bridge)
        cube.name <- sprintf "Bridge (%d, %d) <-> (%d, %d)" x1 y1 x2 y2
        cube.transform.localScale <- P.V3 size 1

    let transformMaze p uiWidth =
        let w = float p.maze.width * (p.gap + 1.0) - p.gap
        let h = float p.maze.height * (p.gap + 1.0) - p.gap
        let c = Array.get Camera.allCameras 0 // Assume we only have one.
        let csize = float c.orthographicSize * 0.95
        let scalex = (csize * 2.0 * (float c.aspect) - uiWidth) / w
        let scaley = csize * 2.0 / h
        let scale = min scalex scaley
        p.go.transform.localScale <- P.V3 (scale, scale) 1

        let offset l = -(l - 1.0) * scale / 2.0
        p.go.transform.localPosition <-
            P.V3 (offset w + (uiWidth / 2.0), offset h) 0

    let spawnMaze (maze: Maze.T) gap uiWidth =
        let p = { maze = maze; gap = gap; go = new GameObject("maze") }
        transformMaze p uiWidth
        Maze.cells maze.width maze.height |> Seq.iter (spawnCell p)
        Maze.getBridges maze |> Seq.iter (spawnBridge p)
        p.go


module Player =
    let playerColor = Color.green / 2.0f

    let move (mp: MazeUtil.Params) pl dir =
        let x, y = pl.pos
        let coords (x, y) = float x * (1.0 + mp.gap), float y * (1.0 + mp.gap)
        let move' dx dy =
            pl.pos <- (x + dx, y + dy)
            pl.go.transform.localPosition <- P.V3 (coords pl.pos) -1
        match dir with
            | "up" -> move' 0 1
            | "down" -> move' 0 -1
            | "left" -> move' -1 0
            | "right" -> move' 1 0
            | _ -> failwith <| "Bad direction: " + dir

    let spawnPlayer (maze: GameObject) =
        let pl = GameObject.CreatePrimitive(PrimitiveType.Sphere)
        pl.name <- "Player"
        P.setMaterial pl playerColor
        pl.transform.SetParent(maze.transform, false)
        pl.transform.localScale <- new Vector3(0.9f, 0.9f, 0.9f)
        pl.transform.localPosition <- new Vector3(0.0f, 0.0f, -1.0f)
        pl


module UIUtil =

    let mutable res = new UI.DefaultControls.Resources()

    let mkButton (parent: GameObject) text handler =
        let button = UI.DefaultControls.CreateButton(res)
        button.name <- sprintf "Button: %s" text
        button.transform.SetParent(parent.transform, false)
        let layout = button.AddComponent<UI.LayoutElement>()
        layout.preferredHeight <- 30.0f
        (button.GetComponent<UI.Button>()).onClick.AddListener(
            new Events.UnityAction(handler))
        let btext = button.GetComponentInChildren<UI.Text>()
        btext.font <- Font.CreateDynamicFontFromOSFont([|"Arial"|], 14)
        btext.text <- text

    let mkSliderLabel (parent: GameObject) text value =
        let label = new GameObject(sprintf "Slider label: %s" text)
        label.transform.SetParent(parent.transform)
        let hlayout = label.AddComponent<UI.HorizontalLayoutGroup>()
        hlayout.childForceExpandHeight <- false
        let labelName = new GameObject(sprintf "Slider name: %s" text)
        labelName.transform.SetParent(label.transform)
        let textName = labelName.AddComponent<UI.Text>()
        textName.alignment <- TextAnchor.MiddleLeft
        textName.font <- Font.CreateDynamicFontFromOSFont([|"Arial"|], 14)
        textName.text <- text
        let labelValue = new GameObject(sprintf "Slider value: %s" text)
        labelValue.transform.SetParent(label.transform)
        let textValue = labelValue.AddComponent<UI.Text>()
        textValue.alignment <- TextAnchor.MiddleRight
        textValue.font <- Font.CreateDynamicFontFromOSFont([|"Arial"|], 14)
        textValue.text <- string value
        fun value -> textValue.text <- string value; value

    let mkSlider (parent: GameObject) text min max value handler =
        let updateLabel = mkSliderLabel parent text value
        let slider = UI.DefaultControls.CreateSlider(res)
        slider.name <- sprintf "Slider: %s" text
        slider.transform.SetParent(parent.transform, false)
        let layout = slider.AddComponent<UI.LayoutElement>()
        layout.preferredHeight <- 20.0f
        let sslider = slider.GetComponent<UI.Slider>()
        sslider.wholeNumbers <- true
        sslider.minValue <- float32 min
        sslider.maxValue <- float32 max
        sslider.value <- float32 value
        sslider.onValueChanged.AddListener(
            new Events.UnityAction<float32>(updateLabel >> handler))

    let addPanel (parent: GameObject) =
        let panel = new GameObject("Panel")
        panel.transform.SetParent(parent.transform, false)
        let rect = panel.AddComponent<RectTransform>()
        rect.anchorMin <- new Vector2(0.0f, 0.0f)
        rect.anchorMax <- new Vector2(0.0f, 1.0f)
        rect.sizeDelta <- new Vector2(100.0f, 1.0f)
        rect.anchoredPosition <- new Vector2(rect.sizeDelta.x / 2.0f, 0.0f)
        let vlayout = panel.AddComponent<UI.VerticalLayoutGroup>()
        vlayout.childForceExpandHeight <- false
        vlayout.spacing <- 5.0f
        vlayout.padding <- new RectOffset(5, 5, 5, 5)
        panel

    let buildUI (ms: IMainScript) =
        let canvas = (GameObject.FindObjectOfType<Canvas>()).gameObject
        let panel = addPanel canvas

        mkButton panel "regenerate" (ms.ClearMaze >> ms.GenMaze)
        mkSlider panel "width" 2 30 (ms.getWidth()) (int >> ms.setWidth)
        mkSlider panel "height" 2 30 (ms.getHeight()) (int >> ms.setHeight)

        canvas



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
    let mutable player: GamePlayer option = None

    interface IMainScript with
        member this.GenMaze () =
            match maze with
                | Some _ -> Debug.Log("Maze already exists!")
                | None ->
                    let m = Maze.GrowingTree.gen mazeWidth mazeHeight
                    let go = MazeUtil.spawnMaze m gap 100.0
                    maze <- Some (GameMaze (m, go))
                    player <- Some {pos=(0, 0); go=(Player.spawnPlayer go)}

        member this.ClearMaze () =
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
        let crt = canvas.GetComponent<RectTransform>()
        cam.orthographicSize <- crt.rect.height / 2.0f
        (this :> IMainScript).GenMaze ()

    member this.Update() =
        let move mp pl (dir: string) =
            if Input.GetKeyDown(dir) then Player.move mp pl dir
        match maze, player with
            | Some (GameMaze (m, go)), Some pl ->
                let mp = {MazeUtil.maze=m; MazeUtil.gap=gap; MazeUtil.go=go}
                List.iter (move mp pl) ["up"; "down"; "left"; "right"]
            | _, _ -> ()

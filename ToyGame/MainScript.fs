namespace ToyGame

open UnityEngine


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

    let mutable mazeMaterial: Material option = None
    let mutable startMaterial: Material option = None
    let mutable endMaterial: Material option = None

    let mkMaterial () =
        let go = GameObject.CreatePrimitive(PrimitiveType.Cube)
        let mat = go.GetComponent<Renderer>().material
        mat.shader <- Shader.Find("Unlit/MazeShader")
        GameObject.Destroy(go)
        mat

    let setMazeMaterial (go: GameObject) =
        let mat =
            match mazeMaterial with
                | Some mat -> mat
                | None ->
                    let mat = mkMaterial ()
                    mazeMaterial <- Some mat
                    mat
        go.GetComponent<Renderer>().material <- mat

    let setStartMaterial (go: GameObject) =
        let mat =
            match startMaterial with
                | Some mat -> mat
                | None ->
                    let mat = mkMaterial ()
                    mat.color <- Color.red
                    startMaterial <- Some mat
                    mat
        go.GetComponent<Renderer>().material <- mat

    let setEndMaterial (go: GameObject) =
        let mat =
            match endMaterial with
                | Some mat -> mat
                | None ->
                    let mat = mkMaterial ()
                    mat.color <- Color.blue
                    endMaterial <- Some mat
                    mat
        go.GetComponent<Renderer>().material <- mat

    let V3 (x, y) z = new Vector3(float32 x, float32 y, float32 z)

    let spawnCube p pos =
        let cube = GameObject.CreatePrimitive(PrimitiveType.Cube)
        cube.transform.SetParent(p.go.transform, false)
        cube.transform.localPosition <- (V3 pos 0)
        setMazeMaterial cube
        cube

    let pos p (x, y) = (x * (1.0 + p.gap), y * (1.0 + p.gap))

    let bridgePos p ((x1, y1), (x2, y2)) =
        pos p (float (x1 + x2) / 2.0, float (y1 + y2) / 2.0)

    let spawnCell p (x, y) =
        let cube = spawnCube p (pos p (float x, float y))
        cube.name <- sprintf "Cell (%d, %d)" x y
        if (x, y) = (0, 0) then setStartMaterial cube
        if (x, y) = (p.maze.width-1, p.maze.height-1) then setEndMaterial cube

    let spawnBridge p ((x1, y1), (x2, y2) as bridge) =
        let size = match x1 = x2, y1 = y2 with
                   | true, false -> (1.0, p.gap)
                   | false, true -> (p.gap, 1.0)
                   | _ -> failwith "Bad bridge."
        let cube = spawnCube p (bridgePos p bridge)
        cube.name <- sprintf "Bridge (%d, %d) <-> (%d, %d)" x1 y1 x2 y2
        cube.transform.localScale <- V3 size 1

    let transformMaze p uiWidth =
        let w = float p.maze.width * (p.gap + 1.0) - p.gap
        let h = float p.maze.height * (p.gap + 1.0) - p.gap
        let c = Array.get Camera.allCameras 0 // Assume we only have one.
        let csize = float c.orthographicSize * 0.95
        let scalex = (csize * 2.0 * (float c.aspect) - uiWidth) / w
        let scaley = csize * 2.0 / h
        let scale = min scalex scaley
        p.go.transform.localScale <- V3 (scale, scale) 1

        let offset l = -(l - 1.0) * scale / 2.0
        p.go.transform.localPosition <-
            V3 (offset w + (uiWidth / 2.0), offset h) 0

    let spawnMaze (maze: Maze.T) gap uiWidth =
        let p = { maze = maze; gap = gap; go = new GameObject("maze") }
        transformMaze p uiWidth
        Maze.cells maze.width maze.height |> Seq.iter (spawnCell p)
        Maze.getBridges maze |> Seq.iter (spawnBridge p)
        p.go


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
        member this.GenMaze () =
            match maze with
                | Some _ -> Debug.Log("Maze already exists!")
                | None ->
                    let m = Maze.GrowingTree.gen mazeWidth mazeHeight
                    let go = MazeUtil.spawnMaze m gap 100.0
                    maze <- Some (GameMaze (m, go))

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

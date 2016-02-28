module ToyGame.Primitives

open UnityEngine

type ColorTuple = float32*float32*float32*float32


let mutable materials: Map<ColorTuple, Material> = Map.empty


let coltuple (c: Color) = (c.r, c.g, c.b, c.a)

let mkMaterial color =
    let go = GameObject.CreatePrimitive(PrimitiveType.Cube)
    let mat = go.GetComponent<Renderer>().material
    mat.shader <- Shader.Find("Unlit/MazeShader")
    mat.color <- color
    GameObject.Destroy(go)
    mat

let getMaterial color =
    match Map.tryFind (coltuple color) materials with
        | Some mat -> mat
        | None ->
            let mat = mkMaterial color
            materials <- Map.add (coltuple color) mat materials
            mat

let setMaterial (go: GameObject) color =
    go.GetComponent<Renderer>().material <- getMaterial color


let V3 (x: float, y: float) z = new Vector3(float32 x, float32 y, float32 z)


module UI =

    let res = new UI.DefaultControls.Resources()

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

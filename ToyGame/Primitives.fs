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

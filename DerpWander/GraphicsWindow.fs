/// Contains the form in which all graphical information is displayed
module GraphicsWindow

open System
open System.Drawing
open System.Windows.Forms
open System.Threading

open Util
open Derp
open WorldOptions
open World

let atlas = 
    new System.Drawing.Bitmap (System.Reflection.Assembly.GetCallingAssembly().GetManifestResourceStream "atlas_new.png")

let tileSize = 8

let rectangle x y w h = Rectangle (x, y, w, h)
let makeTile x y = rectangle x y tileSize tileSize
let tileAt x y = copyBitmapRegion atlas <| makeTile (x * tileSize) (y * tileSize)

let blankSprite     = tileAt 0 0
let foodSprite      = tileAt 1 0
let derpEastSprite  = tileAt 2 0
let derpWestSprite  = tileAt 3 0
let derpSouthSprite = tileAt 4 0
let derpNorthSprite = tileAt 5 0
let derpDeadSprite  = tileAt 6 0

/// The form in which the world will be displayed graphically
type GraphicsWindow (world : World) as this =
    inherit Form ()
    let mutable world = world
    let options = world.Options

    let setSpeed (newSpeed : string) = options.generalOptions.genSpeed <- newSpeed

    do
        this.Text <- "DerpWander"
        this.ClientSize <- Size (world.Width * tileSize, world.Height * tileSize)
        this.StartPosition <- FormStartPosition.CenterScreen
        this.Icon <- Icon.FromHandle (atlas.GetHicon ())
        this.SetStyle (ControlStyles.AllPaintingInWmPaint, true)
        this.SetStyle (ControlStyles.UserPaint, true)
        this.SetStyle (ControlStyles.OptimizedDoubleBuffer, true)

    member this.Update () =
        let speedVal = GeneralOptions.speedValue options.generalOptions
        let paused = speedVal = -1
        if not paused then
            world.Update ()
            let noDisplay = speedVal = 0
            if not noDisplay then this.Invalidate ()

    member this.World
        with get () = world
        and set value = world <- value
        
    override this.OnKeyUp e =
        match e.KeyData with
        | Keys.Escape -> Environment.Exit 0
        | Keys.Space -> setSpeed "Paused"
        | _ -> ()
        e.Handled <- true

    override this.OnMouseClick e =
        let menuItem update option name = 
            new MenuItem (name, new EventHandler (fun _ _ -> update option))

        let speedItems =
            genSpeeds
            |> Array.map (fst >> (fun name -> (menuItem setSpeed name name)))

        let speeds = new MenuItem ("Speeds", speedItems)

        match e.Button with
        | MouseButtons.Right ->
            let speedSelect = new ContextMenu ([| speeds |])
            speedSelect.Show (this, e.Location)
        | _ -> ()

    override this.OnPaint (e : PaintEventArgs) =
        e.Graphics.CompositingQuality <- Drawing2D.CompositingQuality.HighSpeed
        e.Graphics.CompositingMode <- Drawing2D.CompositingMode.SourceOver
        e.Graphics.InterpolationMode <- Drawing2D.InterpolationMode.NearestNeighbor

        for row = 0 to world.Height - 1 do
            for col = 0 to world.Width - 1 do
                    let point = Point (col * tileSize, row * tileSize)
                    match world.Map.[col, row] with
                    | Empty ->
                        e.Graphics.DrawImageUnscaled (blankSprite, point)
                    | Food ->
                        e.Graphics.DrawImageUnscaled (foodSprite, point)
                    | Derp derp ->
                        match derp.Status with
                        | Alive ->
                            match derp.Orientation with
                            | Orientation.North -> e.Graphics.DrawImageUnscaled (derpNorthSprite, point)
                            | Orientation.South -> e.Graphics.DrawImageUnscaled (derpSouthSprite, point)
                            | Orientation.East  -> e.Graphics.DrawImageUnscaled (derpEastSprite,  point)
                            | Orientation.West  -> e.Graphics.DrawImageUnscaled (derpWestSprite,  point)
                        | Dead -> e.Graphics.DrawImageUnscaled (derpDeadSprite, point)

module Window

open System
open System.Drawing
open System.Windows.Forms
open System.Threading

open Util
open Derp
open WorldOptions
open World

/// The form in which the world will be displayed graphically.
type GraphicsWindow (world : World) as this =
    inherit Form ()

    let atlas = 
        new System.Drawing.Bitmap (System.Reflection.Assembly.GetCallingAssembly().GetManifestResourceStream "atlas_new.png")

    let mutable world = world

    let tileSize = 8

    let rectangle x y w h = Rectangle(x, y, w, h)
    let makeTile x y = rectangle x y tileSize tileSize
    let tileAt x y = copyBitmapRegion atlas <| makeTile (x * tileSize) (y * tileSize)

    let blankSprite     = tileAt 0 0
    let foodSprite      = tileAt 1 0
    let derpEastSprite  = tileAt 2 0
    let derpWestSprite  = tileAt 3 0
    let derpSouthSprite = tileAt 4 0
    let derpNorthSprite = tileAt 5 0
    let derpDeadSprite  = tileAt 6 0

    let setSpeed (newSpeed : string) =
        world.Options.Speed <- GenSpeed.Parse (typedefof<GenSpeed>, newSpeed) :?> GenSpeed

    do
        this.Text <- "DerpWander"
        this.ClientSize <- Size (world.Width * tileSize, world.Height * tileSize)
        this.StartPosition <- FormStartPosition.CenterScreen
        this.Icon <- Icon.FromHandle (atlas.GetHicon ())
        this.SetStyle (ControlStyles.AllPaintingInWmPaint, true)
        this.SetStyle (ControlStyles.UserPaint, true)
        this.SetStyle (ControlStyles.OptimizedDoubleBuffer, true)

    member this.Update () =
        world.Update ()
        if world.Options.Speed <> GenSpeed.FastestNoDisp then
            this.Invalidate ()

    member this.World
        with get () = world
        and set value = world <- value
        
    override this.OnKeyUp e =
        match e.KeyData with
        | Keys.Escape -> Environment.Exit 0
        | _ -> ()
        e.Handled <- true

    override this.OnMouseClick e =
        match e.Button with
        | MouseButtons.Right ->
            let menuItems =
                [|
                    let names = GenSpeed.GetNames typedefof<GenSpeed>
                    for name in names do 
                        let item = new MenuItem(name)
                        item.Click.Add (fun _ -> setSpeed name)
                        yield item
                |]
            let speedSelect = new ContextMenu (menuItems)
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

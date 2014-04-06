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

    let derpNorthSprite = copyBitmapRegion atlas (new Rectangle (0,  0, 8, 8))
    let derpSouthSprite = copyBitmapRegion atlas (new Rectangle (8,  0, 8, 8))
    let derpEastSprite  = copyBitmapRegion atlas (new Rectangle (16, 0, 8, 8))
    let derpWestSprite  = copyBitmapRegion atlas (new Rectangle (24, 0, 8, 8))
    let foodSprite      = copyBitmapRegion atlas (new Rectangle (32, 0, 8, 8))
    let blankSprite     = copyBitmapRegion atlas (new Rectangle (40, 0, 8, 8))

    let setSpeed (newSpeed : string) =
        world.Options.Speed <- GenSpeed.Parse (typedefof<GenSpeed>, newSpeed) :?> GenSpeed

    do
        this.Text <- "DerpWander"
        this.ClientSize <- Size (world.Width * 8, world.Height * 8)
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
                    let point = Point (col * 8, row * 8)
                    match world.Map.[col, row] with
                    | Empty ->
                        e.Graphics.DrawImageUnscaled (blankSprite, point)
                    | Food ->
                        e.Graphics.DrawImageUnscaled (foodSprite, point)
                    | Derp derp ->
                        match derp.Orientation with
                        | Orientation.North -> e.Graphics.DrawImageUnscaled (derpNorthSprite, point)
                        | Orientation.South -> e.Graphics.DrawImageUnscaled (derpSouthSprite, point)
                        | Orientation.East  -> e.Graphics.DrawImageUnscaled (derpEastSprite,  point)
                        | Orientation.West  -> e.Graphics.DrawImageUnscaled (derpWestSprite,  point)

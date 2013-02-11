module Window

open System
open System.Drawing
open System.Windows.Forms
open System.Threading

open Util
open Derp
open World

/// The form in which the world will be displayed graphically.
type GraphicsWindow (world : World) as this =
    inherit Form ()

    let atlas = 
        new System.Drawing.Bitmap (System.Reflection.Assembly.GetCallingAssembly().GetManifestResourceStream "Atlas.png")

    let derpNorthSprite = copyBitmapRegion atlas (new Rectangle (0,  0, 8, 8))
    let derpSouthSprite = copyBitmapRegion atlas (new Rectangle (8,  0, 8, 8))
    let derpEastSprite  = copyBitmapRegion atlas (new Rectangle (16, 0, 8, 8))
    let derpWestSprite  = copyBitmapRegion atlas (new Rectangle (24, 0, 8, 8))
    let foodSprite      = copyBitmapRegion atlas (new Rectangle (32, 0, 8, 8))
    let blankSprite     = copyBitmapRegion atlas (new Rectangle (40, 0, 8, 8))

    do
        this.Text <- "DerpWander"
        this.ClientSize <- Size (world.Size * 8, world.Size * 8)
        this.Icon <- Icon.FromHandle (atlas.GetHicon ())
        this.SetStyle (ControlStyles.AllPaintingInWmPaint, true)
        this.SetStyle (ControlStyles.UserPaint, true)
        this.SetStyle (ControlStyles.OptimizedDoubleBuffer, true)

    member this.Update () =
        world.Update ()
        this.Invalidate ()
        

    override this.OnKeyUp e =
        match e.KeyData with
        | Keys.Escape -> Environment.Exit 0
        | _ -> ()
        e.Handled <- true

    override this.OnPaint (e : PaintEventArgs) = 
        for x = 0 to world.Size - 1 do
            for y = 0 to world.Size - 1 do
                let point = Point (x * 8, y * 8)
                match world.Map.[x, y] with
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
                
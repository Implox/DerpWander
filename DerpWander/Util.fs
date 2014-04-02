module Util

open System.Drawing

type Point2 = int * int

type Random () =
    inherit System.Random ()

    ///Returns a new float in the range [0 .. n).
    member this.NextDouble n = (float (this.Next (n * 10))) / 10.0 + this.NextDouble ()

/// Contains useful extension functions for the F# List module.
module List =
    /// Returns the first n elements in a list.
    let inline take n (col : _ list) = [for i = 0 to n - 1 do yield col.[i]]

    /// Drops the first n elements in a list and returns what remains.
    let inline drop n (col : _ list) = [for i = n to col.Length - 1 do yield col.[i]]

    /// Takes a given colection and returns two colections that are partitioned at the given index.
    let splitAt n (col : _ list) = (take n col, drop n col)

/// Contains useful extension functions for the F# Array module.
module Array =
    /// Returns the first n elements in a given array.
    let inline take n (col : _ []) = [| for i = 0 to n - 1 do yield col.[i] |]

    /// Drops the first n elements in a given array and returns what remains.
    let inline drop n (col : _ []) = [| for i = n to col.Length - 1 do yield col.[i] |]

    /// Takes a given array and returns two arrays that are partitioned at the given index.
    let splitAt n (col : _ []) = (take n col, drop n col)

    /// Breaks an array down into uniformly-sized chunks and returns an array containing each of them.
    let breakBy n (col : _ []) =
        let window = Seq.windowed n col
        [| for i = 0 to (Seq.length window) - 1 do if i % n = 0 then yield Seq.nth i window |]

    /// Returns a normalized version of a given array.
    let normalize (col : double []) =
        let sum = Array.sum col
        col |> Array.map (fun x -> x / sum)

    /// Converts a one-dimensional array into a two-dimensional array with the given lengths.
    let elevate (col : double []) (length1 : int) (length2 : int) = 
        Array2D.init length1 length2 (fun i j -> col.[i + j * length1])

/// Contains useful extension functions for the F# Array2D module.
module Array2D =
    /// Converts a two-dimensional array into a one-dimensional array.
    let flatten (col : _ [,]) =
        [| for i = 0 to (Array2D.length1 (col) - 1) do
            for j = 0 to (Array2D.length2 (col) - 1) do
                yield col.[i, j] |]

let rand = new Random ()

/// Returns the integer and fractional parts of a float as (frac, int).
let inline modf (x : float) = (x - (int >> float) x, (int >> float) x)

/// Checks if a point is within the bounds of a grid
let inline isInBounds x y width height = (0 <= x && x < width) && (0 <= y && y < height)

/// Adds two tuples together
let inline tupleAdd ((ax, ay) : Point2) ((bx, by) : Point2) = (ax + bx, ay + by)

/// Modified mod function, used for wrapping world coordinates properly
let (%%) x m = ((x % m) + m) % m

/// Wraps a coordinate pair to the bounds of a world
let wrap (w, h) (x, y) = (x %% w, y %% h)

/// Creates a new bitmap which contains a given region within a given bitmap.
let copyBitmapRegion (srcMap : Bitmap) (region : Rectangle) =
    let newMap = new Bitmap (region.Width, region.Height)
    let g = Graphics.FromImage newMap
    g.DrawImageUnscaledAndClipped (srcMap, Rectangle(-region.X, -region.Y, srcMap.Width, srcMap.Height))
    g.Dispose ()
    newMap

/// Rotates a given bitmap by a given angle (in degrees).
let rotateBitmap (srcMap : Bitmap) (theta : float) =
    let newMap = new Bitmap (srcMap.Width, srcMap.Height)
    let g = Graphics.FromImage newMap
    newMap.SetResolution(srcMap.HorizontalResolution, srcMap.VerticalResolution)
    g.TranslateTransform (float32 (srcMap.Width / 2), float32 (srcMap.Height / 2))
    g.RotateTransform (float32 theta)
    g.TranslateTransform (- float32 (srcMap.Width / 2), - float32 (srcMap.Height / 2))
    g.DrawImage (srcMap, new Point (0, 0))
    g.Dispose ()
    newMap
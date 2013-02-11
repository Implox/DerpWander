module Util

open System.Drawing

type Random () =
    inherit System.Random ()

    ///Returns a new float in the range [0 .. n).
    member this.NextDouble n = (float (this.Next (n * 10))) / 10.0 + this.NextDouble ()

/// Contains useful extension functions for the F# List module.
module List =
    /// Returns the first n elements in a list.
    let take n (coll : _ list) = [for i = 0 to n - 1 do yield coll.[i]]

    /// Drops the first n elements in a list and returns what remains.
    let drop n (coll : _ list) = [for i = n to coll.Length - 1 do yield coll.[i]]

    /// Takes a given collection and returns two collections that are partitioned at the given index.
    let splitAt n (coll : _ list) = (take n coll, drop n coll)

/// Contains useful extension functions for the F# Array module.
module Array =
    /// Breaks a list down into uniformly-sized chunks and returns each of them.
    let breakBy n (col : _ []) =
        let window = Seq.windowed n col
        [| for i = 0 to (Seq.length window) - 1 do if i % n = 0 then yield Seq.nth i window |]


let rand = new Random ()

/// Returns the integer and fractional parts of a float as (frac, int).
let inline modf (x : float) = (x - (int >> float) x, (int >> float) x)

/// Checks if a point is within the bounds of a grid
let inline isInBounds x y width height = (0 <= x && x < width) && (0 <= y && y < height)

/// Adds two tuples together
let inline tupleAdd ((ax, ay) : int * int) ((bx, by) : int * int) = (ax + bx, ay + by)

/// Creates a new bitmap which contains a given region within a given bitmap.
let copyBitmapRegion (srcMap : Bitmap) (region : Rectangle) =
    let newMap = new Bitmap (region.Width, region.Height)
    let g = Graphics.FromImage newMap
    g.DrawImageUnscaledAndClipped (srcMap, Rectangle(-region.X, -region.Y, srcMap.Width, srcMap.Height))
    g.Dispose ()
    newMap
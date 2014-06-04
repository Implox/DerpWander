/// The form in which world options can be changed.
module OptionsWindow

open System.Windows.Forms

open WorldOptions

type OptionsWindow (options : CompleteOptionSet.T, icon : System.Drawing.Icon) as this =
    inherit Form ()

    do
        this.Text <- "Options"
        this.StartPosition <- FormStartPosition.WindowsDefaultLocation
        this.Icon <- icon
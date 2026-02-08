namespace Aegith

open System.IO

type ReadFile() =
    static member Read (path: string) =
        use sr = new StreamReader(path)
        sr.ReadToEnd()

namespace Aegith.Compiler

type TypeInferece() =
    member val private FlatAST = None with get, set
    member this.setFlatAST fast =
        this.FlatAST <- Some fast

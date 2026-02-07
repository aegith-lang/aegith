namespace Aegith.Compiler

type TypeInferece(fast: FlatAST) =
    member val private FlatAST = fast with get, set
    member val private Data = fast.getData() with get, set

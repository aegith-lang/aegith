namespace Aegith.Compiler

type HMTypeInferece(fast: FlatAST) =
    member val private FlatAST = fast with get, set
    member val private Data = fast.getData() with get, set

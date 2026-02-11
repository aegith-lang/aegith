open Aegith.Compiler

[<EntryPoint>]
let main arv =
    let p = Parser()
    let input = @"
package main
import std::fmt

struct A {
    init new() {@}
}
"
    p.run input |> printfn "%A"
    let fast = p.getFlatAST()
    //fast.initData()
    fast |> printfn "%A"
    //let hmti = HMTypeInference(fast)
    0

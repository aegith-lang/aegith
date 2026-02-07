open Aegith.Compiler

[<EntryPoint>]
let main arv =
    let p = Parser()
    let input = @"
package main
import std::fmt

func main(*a: i32) {
    let *a = 1.a + 3
}
"
    p.run input |> printfn "%A"
    let fast = p.getFlatAST()
    fast.initData()
    fast |> printfn "%A"
    let ti = TypeInferece(fast)
    0
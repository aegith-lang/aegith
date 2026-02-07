open Aegith.Compiler

[<EntryPoint>]
let main arv =
    let p = Parser()
    let ti = TypeInferece()
    let input = @"
package main
import std::fmt

func main(*a: i32) {
    let *a: Vec<_> = &1
}
"
    p.run input |> printfn "%A"
    p.getFlatAST() |> printfn "%A"
    p.getFlatAST() |> ti.setFlatAST
    0
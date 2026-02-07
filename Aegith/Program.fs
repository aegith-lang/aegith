open Aegith.Compiler

[<EntryPoint>]
let main arv =
    let p = Parser()
    let input = @"
package main
import std::fmt

func main(*a: i32) {
    let *a: Vec<chan i32> = &1
}
"
    p.run input |> printfn "%A"
    p.getFlatAST() |> printfn "%A"
    0
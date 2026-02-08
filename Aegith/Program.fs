open Aegith.Compiler

[<EntryPoint>]
let main arv =
    let p = Parser()
    let input = @"
package main
import std::fmt

func main() {
    func f(x, y) {
        func g(x, y) {
            x + y
        }
        g(x, y)
    }
    f(1, 2)
}
"
    p.run input |> printfn "%A"
    let fast = p.getFlatAST()
    //fast.initData()
    fast |> printfn "%A"
    //let hmti = HMTypeInference(fast)
    0
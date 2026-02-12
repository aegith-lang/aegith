open Aegith.Compiler
open FParsec

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
    
    0

namespace Frolog.Tests

open Frolog

open NUnit.Framework

[<AutoOpen>]
module Common =
    let test f = f()
    let testeq r res = 
        Assert.AreEqual(box r, box res)
    let testneq r res = 
        Assert.AreNotEqual(box r, box res)
    let testfail f =
        Assert.Throws(fun () -> f() |> ignore) |> ignore
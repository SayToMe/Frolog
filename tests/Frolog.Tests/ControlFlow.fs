namespace Frolog.Tests

open NUnit.Framework

open Frolog
open Frolog.DefineRule
open Frolog.DefineRule.DefPublic
open Frolog.SearchMachines
open Frolog.CustomRules

[<TestFixture>]
module ControlFlow =
    open Frolog.DefineRule.DefPublicDirectOperators
    let def = signf
    let clearExecute facts call =
        let m = SearchMachines.Simple.CreateClear()
        for fact in facts do
            m.AddRule(fact) 
        m.Execute(call) |> Seq.map(fun s -> s.AsString) |> Seq.toList
    let execute facts call =
        let m = SearchMachines.Simple.CreateDebug()
        for fact in facts do
            m.AddRule(fact) 
        m.Execute(call) |> Seq.map(fun s -> s.AsString) |> Seq.toList

    [<Test>]
    let ``Test of fact``() = 
        let s = def "f(1)"
        let f = defFact s
        Assert.AreEqual(clearExecute [f] s, ["f(1)"])
        Assert.AreEqual(clearExecute [! "simple(X, X)"] (signf "simple(1, X)"), ["simple(1, 1)"])
    [<Test>]
    let ``Test of call``() = 
        let f = defFact(def "f(1)")
        let c = defCall (def "call(X)") (def "f(X)")
        Assert.AreEqual(execute [f; c] (def "call(1)"), ["call(1)"])
        Assert.AreEqual(execute [f; c] (def "call(X)"), ["call(1)"])
    [<Test>]
    let ``Test of conjunction``() = 
        let f1 = defFact(def "f(1)")
        let f2 = defFact(def "f(2)")
        let c = "call(X, Y)" => "f(X)" |& "f(Y)"
        Assert.AreEqual(execute [f1; f2; c] (def "call(1, 0)"), [])
        Assert.AreEqual(execute [f1; f2; c] (def "call(1, 2)"), ["call(1, 2)"])
        Assert.AreEqual(execute [f1; f2; c] (def "call(X, Y)"), ["call(1, 1)"; "call(1, 2)"; "call(2, 1)"; "call(2, 2)"])
    [<Test>]
    let ``Test of or``() = 
        let f1 = defFact(def "f1(1)")
        let f2 = defFact(def "f2(2)")
        let c = defOr (def "call(X)") (callBody "f1(X)") (callBody "f2(X)")
        Assert.AreEqual(execute [f1; f2; c] (def "call(0)"), [])
        Assert.AreEqual(execute [f1; f2; c] (def "call(1)"), ["call(1)"])
        Assert.AreEqual(execute [f1; f2; c] (def "call(X)"), ["call(1)"; "call(2)"])
    [<Test>]
    let ``Test of cut``() = 
        let f1 = defFact(def "f(1)")
        let f2 = defFact(def "f(2)")
        let cut = defCall (def "cut(X)") (def "f(X)") |> combine Cut
        Assert.AreEqual(execute [f1; f2; cut] (def "cut(1)"), ["cut(1)"])
        Assert.AreEqual(execute [f1; f2; cut] (def "cut(X)"), ["cut(1)"])
        let ncut = defCall (def "ncut(X, Y)") (def "f(X)") |> combine (callBody "cut(Y)")
        Assert.AreEqual(execute [f1; f2; cut; ncut] (def "ncut(X, Y)"), ["ncut(1, 1)"; "ncut(2, 1)"])
    [<Test>]
    let ``Test of not``() = 
        let f1 = defFact(def "a(1)")
        let f2 = defFact(def "b(1)")
        let f3 = defFact(def "b(2)")
        let not = defCall(def "call(X)") (def "b(X)") |> combine (Not(callBody "a(X)"))
        let rl = [f1; f2; f3; not]

        Assert.AreEqual(execute rl (def "call(1)"), [])
        Assert.AreEqual(execute rl (def "call(2)"), ["call(2)"])
        Assert.AreEqual(execute rl (def "call(X)"), ["call(2)"])
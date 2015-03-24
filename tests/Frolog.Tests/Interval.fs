namespace Frolog.Tests

open NUnit.Framework

open Frolog
open Frolog.DefineRule
open Frolog.DefineRule.DefPublic
open Frolog.SearchMachines
open Frolog.CustomRules

[<TestFixture>]
module Interval =
    let execute call =
        let m = SearchMachines.Simple.CreateDebug()
        Interval.appendInterval m
        m.Execute(signf call) |> Seq.map(fun s -> s.AsString) |> Seq.toList

    [<Test>]
    let ``Test interval``() =
        let call a b c = Interval.call (a.ToString()) (b.ToString()) (c.ToString())
        let bottom = 1
        let upper = 3
        let simpleCall = Interval.call (bottom.ToString()) (upper.ToString()) "E"
        let r = (execute simpleCall)
        let expectedCalls = 
            List.map(fun i -> Interval.call(bottom.ToString()) (upper.ToString()) (i.ToString())) [bottom..upper]
        Assert.AreEqual(execute simpleCall, expectedCalls)
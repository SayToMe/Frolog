namespace Frolog.Tests

open NUnit.Framework

open Frolog
open Frolog.DefineRule
open Frolog.DefineRule.DefPublic
open Frolog.SearchMachines
open Frolog.CustomRules

[<TestFixture>]
module MatchesGame =
    let execute call =
        let m = SearchMachines.Simple.Create()
        MatchesGame.appendGoodMoveRules m
        m.Execute(call) |> Seq.map(fun s -> s.AsString) |> Seq.toList

    [<Test>]
    let ``Test matches game``() =
        let simpleCall = MatchesGame.call "4" "L"
        Assert.AreEqual(execute (signf simpleCall), [MatchesGame.call "4" "3"])
        let cCall = MatchesGame.call "10" "L"
        Assert.AreEqual(execute (signf cCall), [MatchesGame.call "10" "1"])
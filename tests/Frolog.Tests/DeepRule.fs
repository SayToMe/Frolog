namespace Frolog.Tests

open NUnit.Framework

open Frolog
open Frolog.DefineRule
open Frolog.DefineRule.DefPublic
open Frolog.SearchMachines
open Frolog.CustomRules

[<TestFixture>]
module VariableDeepRule =
    let execute n facts call =
        let m = SearchMachines.Simple.CreateDebug()
        let mainRule = DeepRule.getRule n
        for f in mainRule::facts do
            m.AddRule f
        m.Execute(call) |> Seq.map(fun s -> s.AsString) |> Seq.length

    [<Test>]
    let ``Test matches game``() =
        let facts = [ DeepRule.getFact "1" "1"; DeepRule.getFact "1" "2"; DeepRule.getFact "1" "3"]
        let test n = 
            let rec pow x n = if n = 0 then 1 else x * (pow x (n - 1))
            let call = DeepRule.call n "X" "Y"
            let res = execute n facts (signf call)
            Assert.AreEqual(res, pow (List.length facts) n)
        test 1
        test 2
        test 3
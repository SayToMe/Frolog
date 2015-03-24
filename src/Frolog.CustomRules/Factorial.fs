namespace Frolog.CustomRules

open Frolog
open Frolog.DefineRule
open Frolog.DefineRule.DefPublic
open Frolog.DefineRule.DefPublicDirectOperators

[<RequireQualifiedAccess>]
module Factorial =
    let fromZero = tryDefFact "factorial(0, 1)" |> Option.get
    let fromN = 
        "factorial(X, F)" => ">(X, 0)" |& "--(X, X1)" |& "factorial(X1, F1)" |& "*(X, F1, F)"

    let appendFactorial(m: ISearchMachine) =
        m.AddRule fromZero
        m.AddRule fromN
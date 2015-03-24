namespace Frolog.CustomRules

open Frolog
open Frolog.DefineRule
open Frolog.DefineRule.DefPublic
open Frolog.DefineRule.DefPublicDirectOperators

[<RequireQualifiedAccess>]
module Interval =
    /// Lower, Upper, Current
    let call = sprintf "interval(%s, %s, %s)"
    
    let appendInterval (m: ISearchMachine) =
        let checkLower = ("interval(L, U, E)" => ">(L, U)") |! false
        let current = ! "interval(L, U, L)"
        let next = "interval(L, U, E)" => "+(L, 1, L1)" |& "interval(L1, U, E)"
        for r in [checkLower; current; next] do
            m.AddRule r
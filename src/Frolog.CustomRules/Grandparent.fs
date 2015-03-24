namespace Frolog.CustomRules

open Frolog
open Frolog.DefineRule
open Frolog.DefineRule.DefPublic
open Frolog.DefineRule.DefPublicDirectOperators

[<RequireQualifiedAccess>]
module Grandparent =
    let parent p c = tryDefFact (sprintf "parent(%s, %s)" p c) |> Option.get
    let grandparent =
        "grandparent(G, C)" => "parent(P, C)"

    let appendParent (m: ISearchMachine) p c = m.AddRule <| parent p c
    let appendGrandparent (m: ISearchMachine) = m.AddRule grandparent
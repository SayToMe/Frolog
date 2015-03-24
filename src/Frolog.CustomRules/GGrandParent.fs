namespace Frolog.CustomRules

open Frolog
open Frolog.DefineRule
open Frolog.DefineRule.DefPublic
open Frolog.DefineRule.DefPublicDirectOperators

[<RequireQualifiedAccess>]
module GGrandparent =
    let parent p c = tryDefFact (sprintf "parent(%s, %s)" p c) |> Option.get
    let ggrandparent =
        "ggrandparent(GG, C)" => "parent(GG, G)" |& "parent(G, P)" |& "parent(P, C)"

    let appendGGrandparent (m: ISearchMachine) = m.AddRule ggrandparent
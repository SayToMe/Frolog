namespace Frolog.CustomRules

open Frolog
open Frolog.DefineRule
open Frolog.DefineRule.DefPublic
open Frolog.DefineRule.DefPublicDirectOperators

module MatchesGame =
    let call = sprintf "good_move(%s, %s)"

    let move x = tryDefFact (sprintf "move(%i)" x) |> Option.get
    let winMoves = [move 1; move 2; move 3]

    let gMoves = 
        let call c = Call(term c |> Option.bind sign |> Option.get)
        let not c = Not(call c)
        let def = "good_move(X, M)" |> term |> Option.bind sign |> Option.get 
        let invalidMove = defFact(signf "good_move(-1, _)") |! false
        let wonMove = defFact def |> combine (call "move(M)") |& "-(X, M, 1)" |! true
        let tryNextMove = defFact def |> combine (call "move(M)") |> combine (call "-(X, M, X1)") |> combine (not "good_move(X1, _)")
        [ invalidMove; wonMove; tryNextMove ]

    let appendGoodMoveRules(m: ISearchMachine) =
        for wm in winMoves do
            m.AddRule wm
        for gm in gMoves do
            m.AddRule gm
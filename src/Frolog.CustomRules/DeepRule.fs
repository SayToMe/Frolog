namespace Frolog.CustomRules

open Frolog
open Frolog.DefineRule
open Frolog.DefineRule.DefPublic
open Frolog.DefineRule.DefPublicDirectOperators

module DeepRule =
    let private call' = sprintf "fact(%s, %s)"
    let call = sprintf "drule%i(%s, %s)"
    let getFact a b = 
        tryDefFact (call' a b) |> Option.get
    let getRule n =
        if n <= 0 then
            ! "drule(X, X)"
        else
            let def = signf (sprintf "drule%i(X, Y)" n)
            let rec _getRule curP n res =
                let curPs = curP.ToString()
                if n = 1 then
                    res |& call' ("X" + curPs) "Y"
                else
                    let nextP = curP + 1
                    let nextPs = nextP.ToString()
                    _getRule nextP (n-1) (res |& (call' ("X" + curPs) ("Y" + curPs)))
            _getRule 0 n (defFact def)
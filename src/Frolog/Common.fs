namespace Frolog

[<AutoOpen>]
module Common =
    let debug s = System.Diagnostics.Debug.WriteLine s
    
    let constant x = fun y -> x
    let identity x = x
    let allways _ = true
    let same x y = x = y
    
    open System.Text.RegularExpressions
    
    module Regex =
        let (|Match|_|) pattern input =
            let m = Regex.Match(input, pattern) in
            if m.Success then Some (List.tail [ for g in m.Groups -> g.Value ]) else None
        let regex pattern input =
            let m = Regex.Match(input, pattern) in
            if m.Success then Some (List.tail [ for g in m.Groups -> g.Value ]) else None
    
    type System.String with
        member s.isUnderscore() = s = "_"

    module Collection =
       let contains (container) (what) =
          Set.ofSeq(container) |> Set.intersect(Set.ofSeq(what)) |> Set.isEmpty |> not

    module List =
        let rec change f n = function
            | [] -> []
            | h::t when f(h) -> n(h)::t
            | h::t -> h::change f n t
        let rec insert f n = function
            | [] -> [n None]
            | h::t when f(h) -> n(Some(h))::t
            | h::t -> h::insert f n t
        let rec except f = function
            | [] -> []
            | h::t when f(h) -> t
            | h::t -> h::except f t
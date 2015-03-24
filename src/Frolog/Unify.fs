namespace Frolog

open Frolog.Common

[<AutoOpen>]
module Unify =
    type TermUnifyResult = TermUnifyResult of t1: Term * t2: Term * unifyied: Term
        with
        member this.T1 = 
            let (TermUnifyResult(t1, _, _)) = this
            t1
        member this.T2 = 
            let (TermUnifyResult(_, t2, _)) = this
            t2
        member this.Unified = 
            let (TermUnifyResult(_, _, unifyied)) = this
            unifyied

    type UnifyResult = 
        | Unified of Term
        | Failed
        with
        static member isSuccess = function
            | Unified(_) -> true
            | Failed -> false
        static member isFailed = function
            | Unified(_) -> false
            | Failed -> true
        static member tryGetTerm result =
            match result with
            | Unified(termurl) -> 
                Some(termurl)
            | Failed -> None

    /// Guaranties bounding all variables
    let rec unify t1 t2: UnifyResult =
        match Term.tryUnify t1 t2 with
        | None -> Failed
        | Some(t) -> Unified(t)
            
    let unifySignatures s1 s2 =
        match unify (Signature.AsTerm s1) (Signature.AsTerm s2) with
        | Failed -> None
        | Unified(t) ->
            match sign t with
            | None -> None
            | Some(sign) -> Some(sign)
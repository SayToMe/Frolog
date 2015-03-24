namespace Frolog

open System
open Frolog.Common

[<StructuredFormatDisplay("{AsString}")>]
type Term = 
    | Variable of name: string
    | Value of value: string
    | Structure of functor': string * arguments: Term list
    with
    member t.AsString = 
        match t with
        | Variable(n) -> n
        | Value(v) -> v
        | Structure(f, args) -> 
            sprintf "%s(%s)" f <| (("", args) ||> List.fold (sprintf "%s, %A")).Trim([|','; ' '|])
    static member tryGetValue (t: Term) =
        match t with
        | Value(v) -> Some(v)
        | _ -> None
    static member tryUnify t1 t2 =
        match t1, t2 with
        | Variable(_), _ -> Some(t2)
        | _, Variable(_) -> Some(t1)
        | Value(v1), Value(v2) when v1 = v2 -> Some(t2)
        | Structure(f1, a1), Structure(f2, a2) when f1 = f2 && a1.Length = a2.Length ->
            let unifiedArgs = (a1, a2) ||> List.map2(fun t1 t2 -> Term.tryUnify t1 t2)
            if List.exists Option.isNone unifiedArgs then
                None
            else
                Some(Structure(f1, List.map Option.get unifiedArgs))
        | _ -> None
    static member IsChangeable = function
        | Variable(_) -> true
        | _ -> false
    static member Compatible t1 t2 =
        let res = Term.IsChangeable t1 || Term.IsChangeable t2 || Term.StrongEquals t1 t2
        match t1, t2 with
        | Structure(f1, a1), Structure(f2, a2) when f1 = f2 ->
            List.forall2 Term.Compatible a1 a2
        | _ -> res
    static member StrongEquals t1 t2 =
        match t1, t2 with
        | Variable(v1), Variable(v2) -> v1 = v2
        | Value(v1), Value(v2) -> v1 = v2
        | Structure(f1, args1), Structure(f2, args2) when f1 = f2 -> List.forall2 Term.StrongEquals args1 args2
        | _ -> false
    static member IsVariableTerm t = 
        match t with
        | Variable(_) -> true
        | _ -> false
    static member GetVariableName t =
        match t with
        | Variable(name) when name.isUnderscore() -> None
        | Variable(name) -> Some(name)
        | _ -> None

[<AutoOpen>]
module TermHelper =
    /// Value - lowerCase word with digits.
    /// Variable - upperCase word with digits.
    /// Structure - lowerCase word with arguments in brackets separated by commas.
    let rec term: string -> Term option = function
        | s when s.isUnderscore() -> Some(Variable s)
        | Regex.Match "^\s*([a-z0-9\+\-]\w*)\s*$" input -> Some(Value input.Head)
        | Regex.Match "^\s*([A-Z]\w*)\s*$" input -> Some(Variable input.Head)
        | s ->
            let rec getSeparetedByComma cur sindex brLevel res =
                if String.length s = sindex then
                    if String.length cur = 0 then
                        Some(sindex, res)
                    else
                        None
                else
                    let c = s.Chars sindex
                    match c with
                    | '\t' | ' ' -> getSeparetedByComma cur (sindex + 1) brLevel res
                    | ',' ->
                        if String.length cur = 0 then
                            getSeparetedByComma "" (sindex + 1) brLevel res
                        else
                            match term cur with
                            | Some t -> getSeparetedByComma "" (sindex + 1) brLevel (t::res)
                            | None -> None
                    | '(' -> 
                        let inner = getSeparetedByComma "" (sindex + 1) (brLevel + 1) []
                        if String.length cur = 0 then
                            None
                        else
                            match inner with
                            | Some(si, ts) ->
                                getSeparetedByComma "" (si + 1) brLevel (Structure(cur, List.rev ts)::res)
                            | None -> None
                    | ')' when brLevel = 0 -> None
                    | ')' -> 
                        if String.length cur = 0 then
                            Some(sindex, res)
                        else
                            match term cur with
                            | Some t -> Some(sindex, t::res)
                            | None -> None
                    | c -> getSeparetedByComma (cur + (c.ToString())) (sindex + 1) brLevel res
            let args = getSeparetedByComma "" 0 0 []
            match args with
            | Some(_, args) ->
                match args with
                | [Structure(_, _)] as str -> Some(str.Head)
                | _ -> None
            | None -> None
        
    /// Value - lowerCase word with digits
    /// Variable - upperCase word with digits
    /// Structure - lowerCase word with arguments in brackets separated by commas
    let termf input =
        match term input with
        | Some(t) -> t
        | None -> failwith ("Cant create term from " + input)
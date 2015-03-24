namespace Frolog

module Predicate =
    type PredicateInput = PredicateInput of Term list
    type PredicateOutput = PredicateOutput of Term list
    module RuleInputOutputConvert =
        let inputToOutput (PredicateInput(input)) = PredicateOutput(input)
        let outputToInput (PredicateOutput(output)) = PredicateInput(output)
    type PredicateResult = PredicateOutput option
    type Predicate = Predicate of (PredicateInput -> PredicateResult)
        with
        member p.Evaluate input =
            let (Predicate(p)) = p
            p input

open Predicate

type RuleBody = 
    | True
    | False
    | Call of Signature
    | Predicate of (PredicateInput -> PredicateResult)
    | Cut
    | Conjunction of RuleBody * RuleBody
    | Or of RuleBody * RuleBody
    | Not of RuleBody
    with
    override b.ToString() = 
        match b with
        | True -> "true"
        | False -> "false"
        | Cut -> "!"
        | Call s -> s.AsString
        | Predicate(_) -> "``P``"
        | Conjunction(b1, b2) -> sprintf "%s, %s" (b1.ToString()) (b2.ToString())
        | Or(b1, b2) -> sprintf "%s, %s" (b1.ToString()) (b2.ToString())
        | Not(b) -> sprintf "not(%s)" (b.ToString())
   
[<StructuredFormatDisplay("{AsString}")>]
type Rule = Rule of definition: Signature * body: RuleBody * isInternal: bool
    with
    override r.ToString() =
        let (Rule(d, b, isInternal)) = r
        sprintf "%s => %s %s" (d.AsString) (b.ToString()) (if isInternal then "| internal" else "")
    member r.AsString = r.ToString()
    member r.Signature = 
        let (Rule(def, _, _)) = r
        def
    member r.Body =
        let (Rule(_, body, _)) = r
        body

[<AutoOpen>]
module RuleCommon =
    let (|Lexem|_|) = function
    | True -> Some <| Lexem True
    | False -> Some <| Lexem False
    | Call(_) as c -> Some <| Lexem c
    | Predicate(_) as p -> Some <| Lexem p
    | Cut -> Some <| Lexem Cut
    | _ -> None

// Rule is only a structure with defined constraints
// It can be fully defined aka f(1).
// Or partially defined f(1, X).
// And can be constrained by f(1, X) :- X > 3.
module DefineRule =
    /// Concat new body to the end
    /// b -> b1, b2 -> b1, b2, b
    /// b -> b1; b2 -> b1; b2, b
    /// b -> ! -> b, !
    /// b -> b1 -> b1, not(b)
    let rec combine body rule =
        let rec combineBody body1 body2 =
            match body1 with
            | True -> Conjunction(body1, body2)
            | False -> Conjunction(body1, body2)
            | Call(_) -> Conjunction(body1, body2)
            | Predicate(_) -> Conjunction(body1, body2)
            | Cut -> Conjunction(body1, body2)
            | Conjunction(b1, b2) -> Conjunction(b1, combineBody b2 body2)
            | Or(b1, b2) -> Or(b1, Conjunction(b2, body))
            | Not(b) -> Conjunction(body1, body2)
        let (Rule(def, b, isInternal)) = rule
        Rule(def, combineBody b body, isInternal)
    let orJoin def bodies =
        Rule(def, List.reduce(fun b1 b2 -> Or(b1, b2)) bodies, false)

    module internal DefInternal =
        let defCall sign call isInternal = Rule(sign, Call call, isInternal)
        let defFact sign isInternal = Rule(sign, True, isInternal)
        let tryDefFact t isInternal =
            match Option.bind sign (term t) with
            | None -> None
            | Some(sign) -> Some(Rule(sign, True, isInternal)) 
        let defConjunction sign body1 body2 isInternal = Rule(sign, body1, isInternal) |> combine body2
        let defOr sign body1 body2 isInternal = Rule(sign, Or(body1, body2), isInternal)
        let defPredicate term inputConverter predicate isInternal =
            let inputConvert input convert predicate =
                let (PredicateInput(arguments)) = input
                if List.length arguments = List.length convert then
                    let convertedArgs = List.map2(fun conv x -> conv(Term.tryGetValue x)) convert arguments
                    predicate convertedArgs
                else
                    None
            Rule(Signature(term), Predicate(fun input -> inputConvert input inputConverter predicate), isInternal)
        let defCut sign body isInternal = Rule(sign, Conjunction(body, Cut), isInternal)
        let defNot sign body isInternal = Rule(sign, Not(body), isInternal)

    module internal DefAsInternal =
        open DefInternal
        let defFact sign = defFact sign true
        let tryDefFact t = tryDefFact t true
        let defCall sign call = defCall sign call true
        let defConjunction term body1 body2 = defConjunction term body1 body2 true
        let defOr term body1 body2 = defOr term body1 body2 true
        let defPredicate term inputConverter predicate = defPredicate term inputConverter predicate true
        let defCut sign body isInternal = defCut sign body true
        let defNot sign body isInternal = defNot sign body true

    module public DefPublic =
        open DefInternal
        let callBody call = Call(signf call)
        let defFact sign = defFact sign false
        let tryDefFact t = tryDefFact t false
        let defCall sign call = defCall sign call false
        let defConjunction term body1 body2 = defConjunction term body1 body2 false
        let defOr term body1 body2 = defOr term body1 body2 false
        let defPredicate term inputConverter predicate = defPredicate term inputConverter predicate false
        let defCut sign body isInternal = defCut sign body false
        let defNot sign body isInternal = defNot sign body false

    [<AutoOpen>]
    module public DefPublicDirectOperators =
        open DefInternal
        let (!) d = defFact (signf d) false
        let (=>) d c = defCall (signf d) (signf c) false
        let (|!) r b = match b with | false -> r |> combine (Conjunction(Cut, False)) | true -> r |> combine (Conjunction(Cut, (True)))
        let (|&) r c = combine (Call(signf c)) r

    [<AutoOpen>]
    module Converters =
        let convertInt = Option.bind(fun x -> 
                    let ok, v = System.Int32.TryParse x
                    if ok then Some(v) else None)

        // TODO: implement
        let convertIntList x = failwith "Not implemented converter: int list"

    let inline success (list: System.Object list) =
        let inline create t = termf (t.ToString())
        Some(PredicateOutput(List.map create list))

    module internal StandartPredicates =
        open DefAsInternal
    
        let defEq =
            defPredicate (termf "=(X, Y)") [convertInt; convertInt] (function
                | [Some(a); Some(b)] -> if a = b then success [a; b] else None
                | [Some(a); None] -> success [a; a]
                | [None; Some(b)] -> success [b; b]
                | _ -> None)
            
        let defGr =
            defPredicate (termf ">(X, Y)") [convertInt; convertInt] (function
                | [Some(a); Some(b)] -> if a > b then success [a; b] else None
                | _ -> None)
            
        let defInc =
            defPredicate (termf "++(X, Y)") [convertInt; convertInt] (function
                    | [Some(x);Some(y)] when x + 1 = y -> success [x; y]
                    | [Some(x); None] -> success [x; x+1]
                    | [None; Some(y)] -> success [y-1; y]
                    | _ -> None)
        
        let defDec =
            defPredicate (termf "--(X, Y)") [convertInt; convertInt] (function
                    | [Some(x); Some(y)] when x - 1 = y -> success [x; y]
                    | [Some(x); None] -> success [x; x-1]
                    | [None; Some(y)] -> success [y=1; y]
                    | _ -> None)
        
        let defSum =
            defPredicate (termf "+(A, B, C)") [convertInt; convertInt; convertInt] (function
                | [Some(a); Some(b); Some(c)] when a + b = c -> success [a; b; c]
                | [Some(a); Some(b); None] -> success[a; b; a + b]
                | [Some(a); None; Some(c)] -> success[a; c - a; c]
                | [None; Some(b); Some(c)] -> success[c - b; b; c]
                | _ -> None)
        
        let defSub =
            defPredicate (termf "-(A, B, C)") [convertInt; convertInt; convertInt] (function
                | [Some(a); Some(b); Some(c)] when a - b = c -> success [a; b; c]
                | [Some(a); Some(b); None] -> success[a; b; a - b]
                | [Some(a); None; Some(c)] -> success[a; c + a; c]
                | [None; Some(b); Some(c)] -> success[c + b; b; c]
                | _ -> None)
        
        let defMul =
            defPredicate (termf "*(A, B, C)") [convertInt; convertInt; convertInt] (function
                | [Some(a); Some(b); Some(c)] when a * b = c -> success [a; b; c]
                | [Some(a); Some(b); None] -> success[a; b; a * b]
                | [Some(a); None; Some(c)] -> success[a; c / a; c]
                | [None; Some(b); Some(c)] -> success[c / b; b; c]
                | _ -> None)

        let defDiv =
            defPredicate (termf "/(A, B, C)") [convertInt; convertInt; convertInt] (function
                | [Some(a); Some(b); Some(c)] when a / b = c -> success [a; b; c]
                | [Some(a); Some(b); None] -> success[a; b; a / b]
                | [Some(a); None; Some(c)] -> success[a; a / c; c]
                | [None; Some(b); Some(c)] -> success[c * b; b; c]
                | _ -> None)

        let defDivs =
            defPredicate (termf "divs(A, B)") [convertInt; convertIntList] (function
                | _ -> None)

    open StandartPredicates
    let standartRules = [defInc; defDec; defSum; defSub; defMul; defDiv; defEq; defGr]
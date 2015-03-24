namespace Frolog.Tests

open Frolog

// Test term create

open NUnit.Framework
open Frolog.Common

[<TestFixture>]
module TermCreateTests =
    let checkval s = testeq (term s) (Some(Value s))
    let checknval s = testneq (term s) (Some(Value s))

    let checkvar s = testeq (term s) (Some(Variable s))
    let checknvar s = testneq (term s) (Some(Variable s))
    
    let checkStruct s name args =
        let args = List.map termf args
        testeq (term s) (Some(Structure(name, args)))
    let checknStruct s name args =
        let args = List.map termf args
        testneq (term s) (Some(Structure(name, args)))

    let checkf s = testfail (fun () -> termf s)

    [<Test>]
    let ``Create values``() =
        checkval "1"
        checkval "x"
        checknval "0 0"
        checknval "x x"

    [<Test>]
    let ``Create variables``() =
        checkvar "X"
        checkvar "X12yX"
        checknvar "fX"
        checknvar "X X"
        
    [<Test>]
    let ``Create structures: simple``() =
        checkStruct "x(1)" "x" ["1"]
    [<Test>]
    let ``Create structures: concat``() =
        checkStruct "fa(X, 1)" "fa" ["X"; "1"]
    [<Test>]
    let ``Create structures: recursive``() =
        checkStruct "rec(f(X, 1), 2)" "rec" ["f(X, 1)"; "2"]

    [<Test>]
    let ``Create failed``() =
        checkf "\\"
        checkf "rec())"
        checkf "x x"
        checkf "(1)"
(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Introducing your project
========================

Say more

*)

#r "Frolog.dll"
open Frolog

open Frolog
open Frolog.DefineRule
open Frolog.DefineRule.DefPublic
open Frolog.SearchMachines

// Create proving machine
let m = SearchMachines.Simple.CreateDebug()
// Add some facts into it
m.AddRule(defFact <| def "f(1)")
m.AddRule(defFact <| def "f(2)")

let call = "f(X)"
// Execute custom query and enumerate through results
for r in m.Execute(signf call) do
    printf "%A" r
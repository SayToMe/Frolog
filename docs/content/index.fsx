(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "..\..\bin"

(**
Frolog
======================

Documentation

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The Frolog library can be <a href="https://nuget.org/packages/Frolog">installed from NuGet</a>:
      <pre>PM> Install-Package Frolog</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Example
-------

This example demonstrates using a function defined in this sample library.

*)

#r "Frolog.dll"
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

(**
Some more info

Samples & documentation
-----------------------

The library comes with comprehensible documentation. 
It can include tutorials automatically generated from `*.fsx` files in [the content folder][content]. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsprojects/Frolog/tree/master/docs/content
  [gh]: https://github.com/fsprojects/Frolog
  [issues]: https://github.com/fsprojects/Frolog/issues
  [readme]: https://github.com/fsprojects/Frolog/blob/master/README.md
  [license]: https://github.com/fsprojects/Frolog/blob/master/LICENSE.txt
*)

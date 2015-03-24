namespace Frolog.Tests

open Frolog

// Test term create

open NUnit.Framework
open Frolog.Common

[<TestFixture>]
module MachineTests =
    let testMachineCache (machine: #SearchMachines.Custom) =
        machine.AddRule(DefineRule.DefPublic.defFact(signf "s(1)"))
        Assert.AreEqual(machine.CacheHits, 0)
        machine.Execute(signf "s(1)") |> ignore
        Assert.AreEqual(machine.CacheHits, 0)
        machine.Execute(signf "s(1)") |> ignore
        Assert.AreEqual(machine.CacheHits, 1)
    
    [<Test>]
    let testLIFO() =
        let machine = Frolog.SearchMachines.Custom.LIFOMachine({maxPrecedences = 1})
        testMachineCache machine
        
    [<Test>]
    let testLRU() =
        let machine = Frolog.SearchMachines.Custom.LRUMachine({maxPrecedences = 1})
        testMachineCache machine
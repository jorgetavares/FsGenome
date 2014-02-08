open System
open System.Diagnostics

open Genome
open Genome.GA
open Genome.ES

[<EntryPoint>]
let main argv = 

    // GA parameters
    let sphereParams = new Parameters(5, seed = 345, totalGenerations = 200, populationSize = 30)
    
    // run GA and measure execution time
    printfn "Number of logical processors available: %d" Environment.ProcessorCount
    let stopWatch = Stopwatch.StartNew()
    GeneticAlgorithm.RunFloat(sphereParams, Fitness.sphereModel)
    stopWatch.Stop()
    printfn "%f" stopWatch.Elapsed.TotalMilliseconds    

    // before exit...
    let key = Console.ReadKey()


    // ES parameters
    sphereParams.OffspringPoolSize <- 200
    sphereParams.MinGene <- 0
    sphereParams.MaxGene <- 1
    
    // run ES and measure execution time
    printfn "Number of logical processors available: %d" Environment.ProcessorCount
    let stopWatch = Stopwatch.StartNew()
    EvolutionaryStrategy.RunCommaSingleStep(sphereParams, Fitness.sphereModel)
    stopWatch.Stop()
    printfn "%f" stopWatch.Elapsed.TotalMilliseconds    

    // before exit...
    let key = Console.ReadKey()
    0 // return an integer exit code

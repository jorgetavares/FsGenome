namespace Genome

/// Evolutionary Strategies
module ES = 
    open Core
    open MathNet.Numerics.Random
    open MathNet.Numerics.Distributions

    module Mutation = 
    /// apply a mutation operator to an individual according to per individual rate
        let applyGlobalMutation (random: System.Random) operator poolSize (population : LinearPopulation<'a>) = 
            let offsprings = [| for i in 1 .. poolSize -> 
                                let c: 'a array = (operator population.Individuals.[random.Next(population.Size)].Chromossome)
                                new LinearIndividual<'a>(c, 0.0)
                             |]
            new LinearPopulation<'a>(offsprings)

        /// standard deviations close to zero are unwanted
        let sigmaCheck (value: float) epsilon =
            if value < epsilon then epsilon else value

        /// Uncorrelated Mutation with One Step size operator
        let uncorrelatedOneStep (random: System.Random) epsilon (chromossome: float array)  =
            let tau = 1.0 / sqrt (float (chromossome.Length - 1))
            let normal = Normal.WithMeanVariance(0.0, 1.0, random)
            let sigma = sigmaCheck (chromossome.[chromossome.Length - 1] * exp(tau * normal.Sample())) epsilon
            let offspring = chromossome |> Array.map (fun x -> x + sigma * normal.Sample())
            offspring.[chromossome.Length - 1] <- sigma // needs to restore the sigma to the correct value
            offspring

        /// Uncorrelated Mutation with N Step sizes operator
        /// TODO: review to confirm it's working well
        let uncorrelatedNSteps (random: System.Random) epsilon (chromossome: float array) =
            let normal = Normal.WithMeanVariance(0.0, 1.0, random)
            let N = chromossome.Length / 2
            let tau1 = 1.0 / sqrt (2.0 * (float N))
            let tau2 = 1.0 / sqrt (2.0 * sqrt (float N))
            let sigmas = chromossome.[N ..] |> Array.map (fun s -> sigmaCheck (s * exp(tau1 * normal.Sample() + tau2 * normal.Sample())) epsilon)
            let values = Array.map2 (fun x s -> x + s * normal.Sample()) chromossome.[.. N - 1] sigmas
            Array.append values sigmas


     module Crossover =
        /// apply a crossover operator to every element of a population and randomly selectiong the other individual
        let applyGlobalCrossover (random: System.Random) operator (population: LinearPopulation<'a>) =
            for i in 0 .. population.Size - 1 do 
                population.Individuals.[i].Chromossome <- operator random population.Individuals.[i].Chromossome 
                                                                          population.Individuals.[random.Next(population.Size)].Chromossome
            population
                    
        /// intermediary crossover
        let intermediaryCrossover (random: System.Random) (p1: float array) (p2: float array) =
            Array.map2 (fun x y -> (x + y) / 2.0) p1 p2

        /// discrete crossover
        let discreteCrossover (random: System.Random) (p1: float array) (p2: float array) =
            Array.map2 (fun x y -> if random.NextDouble() < 0.5 then x else y) p1 p2    


    module Replacement =
        /// (mu, lambda)
        let commaReplacement (parents: LinearPopulation<'a>) (offspring: LinearPopulation<'a>) = 
            let individuals = offspring.Individuals |> Array.sortBy (fun o -> o.Fitness) 
            parents.Individuals <- individuals.[0 .. parents.Size - 1]
            parents

        /// (mu + lambda)
        let plusReplacement (parents: LinearPopulation<'a>) (offspring: LinearPopulation<'a>) = 
            let individuals = (Array.append parents.Individuals offspring.Individuals ) |> Array.sortBy (fun o -> o.Fitness) 
            parents.Individuals <- individuals.[0 .. parents.Size - 1]
            parents
              

    /// module with base functions to build more specialized evolutionary algorithms
    module Algorithm = 
        open Replacement
        open Crossover
        open Mutation
        open Evaluation
        open MathNet.Numerics.Statistics
        
        /// generational ES (mu, lambda)
        let commaES<'a> (random: System.Random) (parameters: Parameters) chromossomeBuilder crossoverOp mutationFn fitnessFunction =
            let mutationOp = (fun x -> mutationFn random parameters.Epsilon x)
            let population = new LinearPopulation<'a>(parameters.PopulationSize, chromossomeBuilder, fitnessFunction)
            Algorithm.outputStatistics 1 population
            for generation = 2 to parameters.TotalGenerations do
                population.Clone() 
                |> applyGlobalMutation random mutationOp parameters.OffspringPoolSize
                |> applyGlobalCrossover random crossoverOp 
                |> evaluate fitnessFunction
                |> commaReplacement population
                |> Algorithm.outputStatistics generation
            Core.Selection.best population

        /// "steady-state" ES (mu + lambda)
        let plusES<'a> (random: System.Random) (parameters: Parameters) chromossomeBuilder crossoverOp mutationFn fitnessFunction =
            let mutationOp = (fun x -> mutationFn random parameters.Epsilon x)
            let population = new LinearPopulation<'a>(parameters.PopulationSize, chromossomeBuilder, fitnessFunction)
            Algorithm.outputStatistics 1 population
            for generation = 2 to parameters.TotalGenerations do
                population.Clone() 
                |> applyGlobalMutation random mutationOp parameters.OffspringPoolSize
                |> applyGlobalCrossover random crossoverOp 
                |> evaluate fitnessFunction
                |> plusReplacement population
                |> Algorithm.outputStatistics generation
            Core.Selection.best population


    /// TODO: simplify code
    type EvolutionaryStrategy() =      
        static member RunCommaMultipleSteps (parameters: Parameters, fitnessFunction, ?random: System.Random, ?crossoverOp, ?mutationOp) =
            let rng = defaultArg random (Random.mersenneTwisterSeed parameters.Seed) 
            let crossover = defaultArg crossoverOp Crossover.intermediaryCrossover
            let mutation = defaultArg mutationOp Mutation.uncorrelatedNSteps
            let chromossomeSize = parameters.ChromossomeSize * 2    // plus self-adaptive mutation rate for each gene
            let fitnessFn = (fun (x: float array) -> fitnessFunction x.[.. parameters.ChromossomeSize - 1])
            let valuesFn = (fun () -> LinearChromossome.randomFloatRange parameters.ChromossomeSize (float parameters.MinGene) (float parameters.MaxGene) rng)
            let sigmasFn = (fun () -> LinearChromossome.randomFloat parameters.ChromossomeSize rng)
            let chromossomeBuilder = (fun () -> Array.append (valuesFn ()) (sigmasFn ()))        
            Algorithm.commaES<float> rng parameters chromossomeBuilder crossover mutation fitnessFn

        static member RunCommaSingleStep (parameters: Parameters, fitnessFunction, ?random: System.Random, ?crossoverOp, ?mutationOp) =
            let rng = defaultArg random (Random.mersenneTwisterSeed parameters.Seed) 
            let crossover = defaultArg crossoverOp Crossover.intermediaryCrossover
            let mutation = defaultArg mutationOp Mutation.uncorrelatedOneStep
            let chromossomeSize = parameters.ChromossomeSize + 1    // plus single self-adaptive mutation rate
            let fitnessFn = (fun (x: float array) -> fitnessFunction x.[.. parameters.ChromossomeSize - 1])
            let valuesFn = (fun () -> LinearChromossome.randomFloatRange parameters.ChromossomeSize (float parameters.MinGene) (float parameters.MaxGene) rng)
            let sigmasFn = (fun () -> LinearChromossome.randomFloat 1 rng)
            let chromossomeBuilder = (fun () -> Array.append (valuesFn ()) (sigmasFn ()))        
            Algorithm.commaES<float> rng parameters chromossomeBuilder crossover mutation fitnessFn

        static member RunPlusMultipleSteps (parameters: Parameters, fitnessFunction, ?random: System.Random, ?crossoverOp, ?mutationOp) =
            let rng = defaultArg random (Random.mersenneTwisterSeed parameters.Seed) 
            let crossover = defaultArg crossoverOp Crossover.intermediaryCrossover
            let mutation = defaultArg mutationOp Mutation.uncorrelatedNSteps
            let chromossomeSize = parameters.ChromossomeSize * 2    // plus self-adaptive mutation rate for each gene
            let fitnessFn = (fun (x: float array) -> fitnessFunction x.[.. parameters.ChromossomeSize - 1])
            let valuesFn = (fun () -> LinearChromossome.randomFloatRange parameters.ChromossomeSize (float parameters.MinGene) (float parameters.MaxGene) rng)
            let sigmasFn = (fun () -> LinearChromossome.randomFloat parameters.ChromossomeSize rng)
            let chromossomeBuilder = (fun () -> Array.append (valuesFn ()) (sigmasFn ()))        
            Algorithm.plusES<float> rng parameters chromossomeBuilder crossover mutation fitnessFn

        static member RunPlusSingleStep (parameters: Parameters, fitnessFunction, ?random: System.Random, ?crossoverOp, ?mutationOp) =
            let rng = defaultArg random (Random.mersenneTwisterSeed parameters.Seed) 
            let crossover = defaultArg crossoverOp Crossover.intermediaryCrossover
            let mutation = defaultArg mutationOp Mutation.uncorrelatedOneStep
            let chromossomeSize = parameters.ChromossomeSize + 1    // plus single self-adaptive mutation rate
            let fitnessFn = (fun (x: float array) -> fitnessFunction x.[.. parameters.ChromossomeSize - 1])
            let valuesFn = (fun () -> LinearChromossome.randomFloatRange parameters.ChromossomeSize (float parameters.MinGene) (float parameters.MaxGene) rng)
            let sigmasFn = (fun () -> LinearChromossome.randomFloat 1 rng)
            let chromossomeBuilder = (fun () -> Array.append (valuesFn ()) (sigmasFn ()))        
            Algorithm.plusES<float> rng parameters chromossomeBuilder crossover mutation fitnessFn
            
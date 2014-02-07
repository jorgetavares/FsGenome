namespace Genome

/// Genetic Algorithms
module ES = 
    open Core
    open MathNet.Numerics.Random
    open MathNet.Numerics.Distributions

    module Mutation = 
        /// standard deviations close to zero are unwanted
        let sigmaCheck (value: float) epsilon =
            if value < epsilon then epsilon else value

        /// Uncorrelated Mutation with One Step size operator
        let uncorrelatedOneStep (random: System.Random) epsilon (chromossome: float array)  =
            let tau = 1.0 / sqrt (float chromossome.Length)
            let normal = Normal.WithMeanVariance(0.0, 1.0, random)
            let sigma = sigmaCheck (chromossome.[chromossome.Length - 1] * exp(tau * normal.Sample())) epsilon
            let offspring = chromossome |> Array.map (fun x -> x + sigma * normal.Sample())
            offspring.[chromossome.Length - 1] <- sigma // needs to restore the sigma to the correct value
            offspring

        /// Uncorrelated Mutation with N Step sizes operator
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
        let applyGlobalCrossover (random: System.Random) operator poolSize (population: LinearPopulation<'a>) =
            let individuals = 
                [| for i in 1 .. poolSize -> 
                    let chromossome : 'a array = operator random 
                                                          population.Individuals.[random.Next(population.Size)].Chromossome 
                                                          population.Individuals.[random.Next(population.Size)].Chromossome
                    new LinearIndividual<'a>(chromossome, 0.0)
                |] 
            new LinearPopulation<'a>(individuals)

        /// intermediary crossover
        let intermediaryCrossover (random: System.Random) (p1: float array) (p2: float array) =
            Array.map2 (fun x y -> (x + y) / 2.0) p1 p2

        /// discrete crossover
        let discreteCrossover (random: System.Random) (p1: float array) (p2: float array) =
            Array.map2 (fun x y -> if random.NextDouble() < 0.5 then x else y) p1 p2    


    module Replacement =
        /// performs generational replacement where all the parents
        /// are replaced by the generated offspring
        let generational (parents: LinearPopulation<'a>) (offspring: LinearPopulation<'a>) = 
            offspring.Individuals |> Array.sortInPlaceBy (fun o -> o.Fitness) 
            new LinearPopulation<'a>(offspring.Individuals.[0 .. parents.Size - 1])    


    /// module with base functions to build more specialized evolutionary algorithms
    module Algorithm = 
        open Replacement
        open Crossover
        open Mutation
        open Evaluation
        open MathNet.Numerics.Statistics
        
        /// generational ES (mu, lambda) 
        /// TODO: generalize better with previous generationalEA; a more generic, configurable base standard EA should exist
        let generationalES<'a> (random: System.Random) (parameters: Parameters) chromossomeBuilder crossoverOp mutationFn fitnessFunction =
            let mutationOp = (fun x -> mutationFn random parameters.Epsilon x)
            let population = new LinearPopulation<'a>(parameters.PopulationSize, chromossomeBuilder, fitnessFunction)
            Algorithm.outputStatistics 1 population
            for generation = 2 to parameters.TotalGenerations do
                population 
                |> applyGlobalCrossover random crossoverOp parameters.OffspringPoolSize
                |> applyMutation random mutationOp 1.0
                |> evaluate fitnessFunction
                |> generational population
                |> Algorithm.outputStatistics generation


    type EvolutionaryStrategy() =      
        /// ES  
        static member RunGenerational (parameters: Parameters, fitnessFunction, ?random: System.Random, ?crossoverOp, ?mutationOp) =
            let rng = defaultArg random (Random.mersenneTwisterSeed parameters.Seed) 
            let crossover = defaultArg crossoverOp Crossover.discreteCrossover
            let mutation = defaultArg mutationOp Mutation.uncorrelatedNSteps
            let chromossomeSize = parameters.ChromossomeSize * 2    // plus self-adaptive mutation rate for each gene
            let chromossomeBuilder = (fun () -> LinearChromossome.randomFloat chromossomeSize rng)        
            Algorithm.generationalES<float> rng parameters chromossomeBuilder crossover mutation fitnessFunction
            
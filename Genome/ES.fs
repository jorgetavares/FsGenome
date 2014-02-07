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
        let uncorrelatedOneStep (random: System.Random) (chromossome: float array) tau epsilon =
            let normal = Normal.WithMeanVariance(0.0, 1.0, random)
            let sigma = sigmaCheck (chromossome.[chromossome.Length - 1] * exp(tau * normal.Sample())) epsilon
            let offspring = chromossome |> Array.map (fun x -> x + sigma * normal.Sample())
            offspring.[chromossome.Length - 1] <- sigma // needs to restore the sigma to the correct value
            offspring

        /// Uncorrelated Mutation with N Step sizes operator
        let uncorrelatedNSteps (random: System.Random) (chromossome: float array) tau1 tau2 epsilon =
            let normal = Normal.WithMeanVariance(0.0, 1.0, random)
            let N = chromossome.Length / 2
            let sigmas = chromossome.[N ..] |> Array.map (fun s -> sigmaCheck (s * exp(tau1 * normal.Sample() + tau2 * normal.Sample())) epsilon)
            let values = Array.map2 (fun x s -> x + s * normal.Sample()) chromossome.[.. N - 1] sigmas
            Array.append values sigmas


    module Crossover = 
        /// intermediary crossover
        let intermediaryCrossover (random: System.Random) (p1: float array) (p2: float array) =
            Array.map2 (fun x y -> (x + y) / 2.0) p1 p2

        /// discrete crossover
        let discreteCrossover (random: System.Random) (p1: float array) (p2: float array) =
            Array.map2 (fun x y -> if random.NextDouble() < 0.5 then x else y) p1 p2    


    type EvolutionaryStrategy() =      
        /// ES  
        static member RunGenerational (parameters: Parameters, fitnessFunction, ?random: System.Random, ?crossoverOp, ?mutationOp) =
            let rng = defaultArg random (Random.mersenneTwisterSeed parameters.Seed) 
            let crossover = defaultArg crossoverOp Crossover.discreteCrossover
            let mutation = defaultArg mutationOp Mutation.uncorrelatedNSteps
            let chromossomeSize = parameters.ChromossomeSize * 2    // plus self-adaptive mutation rate for each gene
            let chromossomeBuilder = (fun () -> LinearChromossome.randomFloat chromossomeSize rng)        
            Algorithm.generationalES<float> rng parameters chromossomeBuilder crossover mutation fitnessFunction
            
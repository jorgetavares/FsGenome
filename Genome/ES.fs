namespace Genome

/// Genetic Algorithms
module ES = 
    open Core
    open MathNet.Numerics.Random
    open MathNet.Numerics.Distributions

    type EvolutionaryStrategy() =      
        /// ES  
        static member RunSelfAdaptive (parameters: Parameters, fitnessFunction, ?random: System.Random) =
            let rng = defaultArg random (Random.mersenneTwisterSeed parameters.Seed) 
            let chromossomeSize = parameters.ChromossomeSize * 2    // plus self-adaptive mutation rate for each gene
            let chromossomeBuilder = (fun () -> LinearChromossome.randomFloat chromossomeSize rng)
            rng
            // TODO: change baseEA to allow mutation only and use the self=adaptive params
        
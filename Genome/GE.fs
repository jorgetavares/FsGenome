namespace Genome

/// Genetic Algorithms
module GE = 
    open Core
    open MathNet.Numerics.Random

    /// TODO: implement genotype to phenotype evaluation
        
    type GramaticalEvolution() =      
        /// GA + mapping mechanism
        static member RunInteger (parameters: Parameters, fitnessFunction, ?random: System.Random, ?crossoverOp, ?mutationOp, ?selectionOp) =
            let rng = defaultArg random (Random.mersenneTwisterSeed parameters.Seed)  
            let crossover = defaultArg crossoverOp Crossover.onePointCrossover
            let mutationFn = defaultArg mutationOp Mutation.flipIntegerMutation
            let mutation = (fun x -> mutationFn rng x parameters.MutationRatePerGene parameters.MinGene parameters.MaxGene)
            let selection = defaultArg selectionOp (fun pop -> Selection.tournament rng parameters.TournamentSize pop)
            let chromossomeBuilder = (fun () -> LinearChromossome.randomInteger parameters.ChromossomeSize parameters.MinGene parameters.MaxGene rng)
            match parameters.ReplacementMode with
            | ReplacementMode.SteadyState  -> Algorithm.steadyStateEA<int> rng parameters chromossomeBuilder crossover mutation selection fitnessFunction 
            | _ -> Algorithm.generationalEA<int> rng parameters chromossomeBuilder crossover mutation selection fitnessFunction        
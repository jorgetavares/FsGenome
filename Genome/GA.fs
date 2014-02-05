namespace Genome

/// Genetic Algorithms
module GA = 
    open Core
    open MathNet.Numerics.Random

    type GeneticAlgorithm() =      
        /// GA (+crossover +recombination +elite)  
        static member RunBinary (parameters: Parameters, fitnessFunction, ?random: System.Random, ?crossoverOp, ?mutationOp, ?selectionOp) =
            let rng = defaultArg random (Random.mersenneTwisterSeed parameters.Seed)  
            let crossover = defaultArg crossoverOp Crossover.onePointCrossover
            let mutation = defaultArg mutationOp Mutation.flipMutation
            let selection = defaultArg selectionOp (fun pop -> Selection.tournament rng parameters.TournamentSize pop)  
            let chromossomeBuilder = (fun () -> LinearChromossome.randomBinary parameters.ChromossomeSize rng)
            match parameters.ReplacementMode with
            | ReplacementMode.SteadyState  -> Algorithm.steadyStateEA<int> rng parameters chromossomeBuilder crossover mutation selection fitnessFunction
            | _ -> Algorithm.generationalEA<int> rng parameters chromossomeBuilder crossover mutation selection fitnessFunction
    
        /// GA integer variant
        static member RunInteger (parameters: Parameters, fitnessFunction, ?random: System.Random, ?crossoverOp, ?mutationOp, ?selectionOp) = 
            let rng = defaultArg random (Random.mersenneTwisterSeed parameters.Seed)  
            let crossover = defaultArg crossoverOp Crossover.onePointCrossover
            let mutation = defaultArg mutationOp Mutation.flipMutation
            let selection = defaultArg selectionOp (fun pop -> Selection.tournament rng parameters.TournamentSize pop)
            let chromossomeBuilder = (fun () -> LinearChromossome.randomInteger parameters.ChromossomeSize parameters.MinGene parameters.MaxGene rng)
            match parameters.ReplacementMode with
            | ReplacementMode.SteadyState  -> Algorithm.steadyStateEA<int> rng parameters chromossomeBuilder crossover mutation selection fitnessFunction 
            | _ -> Algorithm.generationalEA<int> rng parameters chromossomeBuilder crossover mutation selection fitnessFunction        

        /// GA float variant
        static member RunFloat (parameters: Parameters, fitnessFunction, ?random: System.Random, ?crossoverOp, ?mutationOp, ?selectionOp) =
            let rng = defaultArg random (Random.mersenneTwisterSeed parameters.Seed)  
            let crossover = defaultArg crossoverOp Crossover.onePointCrossover
            let mutation = defaultArg mutationOp Mutation.uniformMutation
            let selection = defaultArg selectionOp (fun pop -> Selection.tournament rng parameters.TournamentSize pop) 
            let chromossomeBuilder = (fun () -> LinearChromossome.randomFloat parameters.ChromossomeSize rng)
            match parameters.ReplacementMode with
            | ReplacementMode.SteadyState -> Algorithm.steadyStateEA<float> rng parameters chromossomeBuilder crossover mutation selection fitnessFunction 
            | _ -> Algorithm.generationalEA<float> rng parameters chromossomeBuilder crossover mutation selection fitnessFunction

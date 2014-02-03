namespace Genome

/// Genetic Algorithms
module GA = 
    open Core
    open MathNet.Numerics.Random

    module Fitness =
        /// One Max fitness function
        let oneMax (chromossome: int array) = 
            float (Array.reduce (+) chromossome)        

        /// Sphere Model fitness function
        let sphereModel (chromossome: float array) = 
            chromossome |> Array.map (fun x -> x ** 2.0) |> Array.sum

    type GeneticAlgorithm() =      
        /// GA (+generational +tournament +crossover +recombination +elite)  
        static member RunGenerationalBinary (parameters: Parameters, fitnessFunction, ?random: System.Random, ?crossoverOp, ?mutationOp, ?selectionOp) =
            let rng = defaultArg random (Random.mersenneTwisterSeed parameters.Seed)  
            let crossover = defaultArg crossoverOp Crossover.onePointCrossover
            let mutation = defaultArg mutationOp Mutation.flipMutation
            let selection = defaultArg selectionOp (fun pop -> Selection.tournament rng parameters.TournamentSize pop)  
            let chromossomeBuilder = (fun () -> LinearChromossome.randomBinary parameters.ChromossomeSize rng)
            Algorithm.generationalEA<int> rng parameters chromossomeBuilder crossover mutation selection fitnessFunction
    
        /// GA integer variant
        static member RunGenerationalInteger (parameters: Parameters, fitnessFunction, ?random: System.Random, ?crossoverOp, ?mutationOp, ?selectionOp) = 
            let rng = defaultArg random (Random.mersenneTwisterSeed parameters.Seed)  
            let crossover = defaultArg crossoverOp Crossover.onePointCrossover
            let mutation = defaultArg mutationOp Mutation.flipMutation
            let selection = defaultArg selectionOp (fun pop -> Selection.tournament rng parameters.TournamentSize pop)
            let chromossomeBuilder = (fun () -> LinearChromossome.randomInteger parameters.ChromossomeSize parameters.MinGene parameters.MaxGene rng)
            Algorithm.generationalEA<int> rng parameters chromossomeBuilder crossover mutation selection fitnessFunction        

        /// GA float variant
        static member RunGenerationalFloat (parameters: Parameters, fitnessFunction, ?random: System.Random, ?crossoverOp, ?mutationOp, ?selectionOp) =
            let rng = defaultArg random (Random.mersenneTwisterSeed parameters.Seed)  
            let crossover = defaultArg crossoverOp Crossover.onePointCrossover
            let mutation = defaultArg mutationOp Mutation.uniformMutation
            let selection = defaultArg selectionOp (fun pop -> Selection.tournament rng parameters.TournamentSize pop) 
            let chromossomeBuilder = (fun () -> LinearChromossome.randomFloat parameters.ChromossomeSize rng) 
            Algorithm.generationalEA<float> rng parameters chromossomeBuilder crossover mutation selection fitnessFunction

        /// Steady-state GA 
        static member RunSteadyStateBinary (parameters: Parameters, fitnessFunction, ?random: System.Random, ?crossoverOp, ?mutationOp, ?selectionOp) =
            let rng = defaultArg random (Random.mersenneTwisterSeed parameters.Seed)  
            let crossover = defaultArg crossoverOp Crossover.onePointCrossover
            let mutation = defaultArg mutationOp Mutation.flipMutation
            let selection = defaultArg selectionOp (fun pop -> Selection.tournament rng parameters.TournamentSize pop)  
            let chromossomeBuilder = (fun () -> LinearChromossome.randomBinary parameters.ChromossomeSize rng)
            Algorithm.steadyStateEA<int> rng parameters chromossomeBuilder crossover mutation selection fitnessFunction
    
        /// Steady-state GA integer variant
        static member RunSteadyStateInteger (parameters: Parameters, fitnessFunction, ?random: System.Random, ?crossoverOp, ?mutationOp, ?selectionOp) = 
            let rng = defaultArg random (Random.mersenneTwisterSeed parameters.Seed)  
            let crossover = defaultArg crossoverOp Crossover.onePointCrossover
            let mutation = defaultArg mutationOp Mutation.flipMutation
            let selection = defaultArg selectionOp (fun pop -> Selection.tournament rng parameters.TournamentSize pop)
            let chromossomeBuilder = (fun () -> LinearChromossome.randomInteger parameters.ChromossomeSize parameters.MinGene parameters.MaxGene rng)
            Algorithm.steadyStateEA<int> rng parameters chromossomeBuilder crossover mutation selection fitnessFunction        

        /// Steady-state GA float variant
        static member RunSteadyStateFloat (parameters: Parameters, fitnessFunction, ?random: System.Random, ?crossoverOp, ?mutationOp, ?selectionOp) =
            let rng = defaultArg random (Random.mersenneTwisterSeed parameters.Seed)  
            let crossover = defaultArg crossoverOp Crossover.onePointCrossover
            let mutation = defaultArg mutationOp Mutation.uniformMutation
            let selection = defaultArg selectionOp (fun pop -> Selection.tournament rng parameters.TournamentSize pop) 
            let chromossomeBuilder = (fun () -> LinearChromossome.randomFloat parameters.ChromossomeSize rng) 
            Algorithm.steadyStateEA<float> rng parameters chromossomeBuilder crossover mutation selection fitnessFunction

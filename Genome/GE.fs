namespace Genome

/// Grammatical Evolution
module GE = 
    open Core
    open System.Collections.Generic
    open System.Text.RegularExpressions
    open MathNet.Numerics.Random

    /// returns a dict that represents a grammar
    type Grammar(grammar: string[]) = 
        let parseGrammar grammar = 
            let rules = new Dictionary<string, string array>()
            grammar
            |> Array.map (fun r -> Regex.Split(r, ":=")) 
            |> Array.iter (fun r -> rules.Add(r.[0], r.[1].Split('|')))  

        member val Rules = parseGrammar grammar with get

    /// TODO: complete basic implementation    
    type GrammaticalEvolution() =      
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
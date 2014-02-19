namespace Genome

/// Grammatical Evolution
module GE = 
    open Core
    open System.IO
    open System.Collections.Generic
    open System.Text.RegularExpressions
    open MathNet.Numerics.Random

    /// returns a dict that represents a grammar
    type Grammar(grammar: string[], startSymbol: string) = 
        let parseGrammar grammar = 
            let rules = new Dictionary<string, string array>()
            grammar
            |> Array.map (fun r -> Regex.Split(r, ":=")) 
            |> Array.iter (fun r -> rules.Add(r.[0].Trim(), r.[1].Split('|') |> Array.map (fun t -> t.Trim())))
            rules  

        member val Rules = parseGrammar grammar with get
        member val StartSymbol = startSymbol with get

        new(filename: string) = 
            let grammar = File.ReadAllLines(filename)
            let start = Regex.Split(grammar.[0], ":=").[0].Trim()
            new Grammar(grammar, start)
        
        member this.Rewrites(symbol: string) =
            this.Rules.Values  


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
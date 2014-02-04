namespace Genome

module Core = 

    /// collection of functions to generate random genotypes
    /// these can be passed directly to the LinearIndividual construtor
    module LinearChromossome =    
        let randomBinary size (random: System.Random) = 
            [|for i in 1 .. size -> random.Next(2)|]

        let randomInteger size min max (random: System.Random) = 
            [|for i in 1 .. size -> random.Next(min, max)|]

        let randomFloat size (random: System.Random) = 
            [|for i in 1 .. size -> random.NextDouble()|]


    /// an individual holds a genotypes of a given type and a fitness value
    type LinearIndividual<'a>(chromossome: 'a array, fitness: float) =    
    
        member val Chromossome = chromossome with get, set
        member val Fitness = fitness with get, set

        // build an individual with a 
        // chromossome from a given function
        new(buildChromossome: (unit -> 'a array), fitness: float) = 
            let chromossome: 'a array = buildChromossome ()
            new LinearIndividual<'a>(chromossome, fitness)

        // same as above but also uses a fitness
        // function to compute the fitness value
        new(buildChromossome: (unit -> 'a array), fitnessFun: ('a array -> float)) = 
            let chromossome: 'a array = buildChromossome ()
            let fitness = fitnessFun chromossome
            new LinearIndividual<'a>(chromossome, fitness)

        member this.Clone() = this.MemberwiseClone() :?> LinearIndividual<'a>
    
        override this.ToString() =
            sprintf "Fitness:%f Chromossome:%A" this.Fitness this.Chromossome
        

    /// standard population of individuals
    type LinearPopulation<'a>(individuals: LinearIndividual<'a> array) = 

        member val Individuals = individuals with get, set

        // build an empty population
        new() = new LinearPopulation<'a>(Array.empty)

        // build a random population init with worse case fitness 
        new (size: int, buildIndividual: (unit -> 'a array)) = 
            let individuals = Array.Parallel.init size (fun _ -> new LinearIndividual<'a>(buildIndividual, System.Double.MaxValue))
            new LinearPopulation<'a>(individuals)
    
        // build and evaluate a random population
        new (size : int, buildIndividual : (unit -> 'a array), fitnessFun : ('a array -> float)) = 
            let individuals = Array.Parallel.init size (fun _ -> new LinearIndividual<'a>(buildIndividual, fitnessFun))
            new LinearPopulation<'a>(individuals)

        member this.Size = this.Individuals.Length
         

    module Evaluation =
        /// applies a fitness function to a population
        let evaluate fitnessFunction (population: LinearPopulation<'a>) = 
            population.Individuals |> Array.Parallel.iter (fun (i : LinearIndividual<'a>) -> i.Fitness <- fitnessFunction i.Chromossome)
            population


    module Replacement =
        /// performs generatonal replacement where all the parents
        /// are replaced by the generated offspring
        let generational (parents: LinearPopulation<'a>) (offspring: LinearPopulation<'a>) = 
            offspring.Individuals 
            |> Array.Parallel.iteri (fun i offspring -> parents.Individuals.[i] <- offspring)
            parents    
            
        /// replaces randomly two individuals in the population with the two offspring
        let steadyStateRandom (random: System.Random) (parents: LinearPopulation<'a>) (offspring: LinearPopulation<'a>) =
            offspring.Individuals
            |> Array.Parallel.iter (fun offspring -> parents.Individuals.[random.Next(parents.Size)] <- offspring)
            parents
        
        /// performs the elistist strategy where the best individual from
        /// the previous population is kept in the new one
        let eliteStrategy (random: System.Random) (best: LinearIndividual<'a>) (population: LinearPopulation<'a>)  =
            population.Individuals.[random.Next(population.Size)] <- best
            population


    module Selection =
        /// applies a selection operator to produce the set of individuals
        /// that will be subject to recombination and mutation
        let select selectionFn (population: LinearPopulation<'a>) = 
            let individuals = Array.Parallel.map (fun _ -> selectionFn population) population.Individuals
            new LinearPopulation<'a>(individuals)

        /// select only two individuals of the original 
        /// population to allow steady-state evolution 
        let selectStedyState selectionFn (population: LinearPopulation<'a>) = 
            let parents = [| for i in 1 .. 2 -> selectionFn population|]
            new LinearPopulation<'a>(parents)
        
        /// get best individual from population (minimization)
        let best (population: LinearPopulation<'a>) =
            let mutable best = population.Individuals.[0]
            for i = 1 to population.Size - 1 do
                if (population.Individuals.[i].Fitness < best.Fitness) then
                    best <- population.Individuals.[i]
            best.Clone()            

        /// tournament selection operator for minimization
        let tournament (random: System.Random) size (population: LinearPopulation<'a>) =  
            let mutable best = population.Individuals.[random.Next(population.Size)]
            for round = 2 to size do
                let current = population.Individuals.[random.Next(population.Size)]
                if (current.Fitness < best.Fitness) then best <- current
            best.Clone() 
   
    
    module Crossover =
        /// apply a crossover operator to selected individuals
        let applyCrossover (random: System.Random) operator rate (population: LinearPopulation<'a>) =
            for i in 0 .. 2 .. (population.Size - 1) do
                if (random.NextDouble() < rate) then 
                    let o1, o2 = operator random population.Individuals.[i].Chromossome population.Individuals.[i + 1].Chromossome
                    population.Individuals.[i].Chromossome <- o1
                    population.Individuals.[i + 1].Chromossome <- o2
            population
             
        /// 1 point crossover
        let onePointCrossover (random: System.Random) (p1: 'a array) (p2: 'a array) = 
            let cutPoint = random.Next(p1.Length)
            let o1 = Array.append p1.[0..cutPoint - 1] p2.[cutPoint..p2.Length - 1]
            let o2 = Array.append p2.[0..cutPoint - 1] p1.[cutPoint..p1.Length - 1]
            (o1, o2)
         

    module Mutation = 
        /// apply a mutation operator to an individual according to per individual rate
        let applyMutation (random: System.Random)  operator rate (population : LinearPopulation<'a>) = 
            population.Individuals 
            |> Array.Parallel.iter (fun (i : LinearIndividual<'a>) -> if (random.NextDouble() < rate) then i.Chromossome <- operator i.Chromossome)
            population

        /// flip mutation for binary chromossomes
        let flipMutation (random: System.Random) (chromossome: int array) rate = 
            let flipGene gene = if (gene = 0) then 1 else 0
            chromossome |> Array.map (fun gene -> if (random.NextDouble() < rate) then flipGene gene else gene)

        /// uniform mutation
        let uniformMutation (random: System.Random) (chromossome: float array) rate =
            chromossome |> Array.map (fun gene -> if (random.NextDouble() < rate) then random.NextDouble() else gene)


    /// module with base functions to build more specizalized evolutionary algorithms
    module Algorithm = 
        open Selection
        open Replacement
        open Evaluation
        open Crossover
        open Mutation
        open MathNet.Numerics.Statistics

        /// output generation statistics
        let outputStatistics generation (population: LinearPopulation<'a>) =
            let samples = population.Individuals |> Array.Parallel.map (fun i -> i.Fitness)
            let stats = new DescriptiveStatistics(samples)
            printfn "%d\t%f\t%f\t%f\t%f\t%f" generation stats.Minimum stats.Maximum stats.Mean stats.Variance stats.StandardDeviation
        
        /// standard evolutionary algorithm 
        let generationalEA<'a> (random: System.Random) (parameters: Parameters) chromossomeBuilder crossoverOp mutationFn selectionOp fitnessFunction =
            let mutationOp = (fun x -> mutationFn random x parameters.MutationRatePerGene)
            let population = new LinearPopulation<'a>(parameters.PopulationSize, chromossomeBuilder, fitnessFunction)
            outputStatistics 1 population
            for generation = 2 to parameters.TotalGenerations do
                population 
                |> select selectionOp
                |> applyCrossover random crossoverOp parameters.CrossoverRate
                |> applyMutation random mutationOp parameters.MutationRatePerIndividual
                |> generational population
                |> eliteStrategy random (best population)   
                |> evaluate fitnessFunction
                |> outputStatistics generation

        /// steady-state 
        let steadyStateEA<'a> (random: System.Random) (parameters: Parameters) chromossomeBuilder crossoverOp mutationFn selectionOp fitnessFunction =
            let mutationOp = (fun x -> mutationFn random x parameters.MutationRatePerGene)
            let population = new LinearPopulation<'a>(parameters.PopulationSize, chromossomeBuilder, fitnessFunction)
            outputStatistics 1 population
            for generation = 2 to parameters.TotalGenerations do
                population 
                |> selectStedyState selectionOp
                |> applyCrossover random crossoverOp parameters.CrossoverRate
                |> applyMutation random mutationOp 1.0
                |> steadyStateRandom random population
                |> evaluate fitnessFunction
                |> outputStatistics generation



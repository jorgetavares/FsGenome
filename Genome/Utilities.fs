namespace Genome

/// container with all the parameters for a GA
/// all are optional with default values to allow 
/// easier configuration
/// the exception is the chromossome size since it's
/// problem dependent and should be defined always
/// in the future it probably should be broken into 
/// different sub-classes according to EA type
type Parameters(chromossomeSize: int,
                ?populationSize: int, 
                ?totalGenerations: int, 
                ?crossoverRate: float, 
                ?mutationRatePerGene: float, 
                ?mutationRatePerIndividual: float,
                ?minGene: int,
                ?maxGene: int,
                ?tournamentSize: int,
                ?seed: int) =
    // default values
    let valuePopulationSize = defaultArg populationSize 200
    let valueTotalGenerationsSize = defaultArg totalGenerations 100
    let valueCrossoverRate = defaultArg crossoverRate 0.7
    let valueMutationRatePerGene = defaultArg mutationRatePerGene 0.01 
    let valueMutationRatePerIndividual = defaultArg mutationRatePerIndividual 1.0
    let valueMinGene = defaultArg minGene 0
    let valueMaxGene = defaultArg maxGene 9
    let valueTournamentSize = defaultArg tournamentSize 3
    let valueSeed = defaultArg seed 123

    // properties
    member val ChromossomeSize = chromossomeSize with get, set
    member val PopulationSize = valuePopulationSize with get, set
    member val TotalGenerations = valueTotalGenerationsSize with get, set
    member val CrossoverRate = valueCrossoverRate with get, set
    member val MutationRatePerGene = valueMutationRatePerGene with get, set
    member val MutationRatePerIndividual = valueMutationRatePerIndividual with get, set
    member val MinGene = valueMinGene with get, set
    member val MaxGene = valueMaxGene with get, set
    member val TournamentSize = valueTournamentSize with get, set
    member val Seed = valueSeed with get, set

   


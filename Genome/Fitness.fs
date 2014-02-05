namespace Genome

/// Fitness Functions
module Fitness =
    /// One Max
    let oneMax (solution: int array) = 
        float (Array.reduce (+) solution)        

    /// Sphere Model
    let sphereModel (solution: float array) = 
        solution |> Array.map (fun x -> x ** 2.0) |> Array.sum

    /// Ackley function
    let ackleyFunction (solution: float array) = 
        let n = float solution.Length
        let term1 = exp(-0.2 * sqrt((1.0 / n) * Array.sum (solution |> Array.map (fun x -> x * x))))
        let term2 = exp((1.0 / n) * Array.sum (solution |> Array.map (fun x -> 2.0 * System.Math.PI * x)))
        -20.0 * term1 - term2 + 20.0 + System.Math.E       



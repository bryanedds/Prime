The Prime F# Code Library [![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/bryanedds/Prime/blob/master/License.md) [![NuGet](https://img.shields.io/nuget/v/Nuget.Core.svg)](https://www.nuget.org/packages/Prime)
=

## Features

- A metaprogramming system based on symbolic expressions with the **Symbol** and **SymbolicConverter** types.
- A generalized serialization system based on the above **Symbol** types.
- A dynamic property system called **Xtension**.
- A purely functional random number generator called **Rand**.
- The **Vsync** monad allowing the same program to be run in parallel or debugged sequentially.
- Fastest persistent **UList**, **UMap**, and **USet** collections rivaling the speed of .NET List, Dictionary and HashSet.
- Innovative pure-functional wrappers for arbitrary impure objects, **KeyedCache** and **MutantCache**.
- Segmented dynamic collections types that keep large temporary collections from thrashing the LOH, **SArray**, **SList**, **SHashSet**, and **SDictionary**
- A powerful and reusable scripting language, AMSL, with the types in the **Scripting** module.
- An algebraic **Ecs** (Entity-Component-System) library extracted from the F# Nu Game Engine.
- So many extension primitives I couldn't hope to mention them all!

Prime is built with clean and modular **Abstract Data Type** programming style as presented here - https://vimeo.com/128464151

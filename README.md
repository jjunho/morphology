# morphology

## Project Description

The **morphology** project is a Haskell library designed for generating morphology. It provides structures and functions to handle general morphology. We hope to add more languages in the future. The current version focuses on the Portuguese language.

## Structure

The library is structured as follows:

- NLP.Morphology
  - PT
    - Verb
    - Nominal
    - Uninflected
  - EN
    - Verb
    - Nominal
    - Uninflected

## Installation

To install the **morphology** library using **Stack**, ensure you have [Stack](https://docs.haskellstack.org/en/stable/README/) installed. Then, clone the repository and build the project:

```bash
git clone https://github.com/jjunho/morphology.git
cd morphology
stack setup
stack build
```

## Usage

Import the library modules into your Haskell project to utilize its functionalities:

```haskell
>>> import Data.Either
>>> import NLP.Morphology.PT.Verb
>>> v = fromRight undefined $ mkParadigm "falar"
>>> v `get` IPRS 
TenseTable {tense = IPRS, forms = [VS4 (Root "fal") A' IPRS P1, VS4 (Root "fal") A' IPRS P2, VS4 (Root "fal") A' IPRS P3, VS4 (Root "fal") A' IPRS P4, VS4 (Root "fal") A' IPRS P5, VS4 (Root "fal") A' IPRS P6]}
>>> v `get` IPRS `get` P1
VS4 (Root "fal") A' IPRS P1
```

Create verb structures and perform morphological transformations:

```haskell
let citation = "falar"
case mkParadigm citation of
    Right paradigm -> putStrLn $ txt paradigm
    Left errorMsg  -> putStrLn $ txt (Left errorMsg)
```

## Features

- **Verb Structures**: Define and manipulate different verb structures based on mood, tense, gender, number, and person.
- **Morphological Analysis**: Perform deep and shallow morphological transformations of verbs.
- **Paradigm Construction**: Generate paradigms for verbs, including various tense tables and forms.
- **Serialization**: Convert verb structures and paradigms to JSON for easy data interchange.
- **Flexible Text Representation**: Customize text representations of morphological components.

## Contributing

Contributions are welcome! Please follow these guidelines:

1. **Fork the Repository**: Create a fork of the project on GitHub.
2. **Create a Branch**: Create a new branch for your feature or bugfix.
3. **Commit Changes**: Write clear and concise commit messages.
4. **Submit a Pull Request**: Open a pull request detailing your changes and their purpose.

Please ensure your code adheres to the project's coding standards and includes appropriate tests.

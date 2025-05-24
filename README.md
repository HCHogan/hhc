# hhc

My Playground

## Features

- Hindley-Milner type inference (Algorithm W)
- Interactive REPL interface
- Basic lambda calculus support and let-bindings

## Work in Progress

- User-defined algebraic data types
- Type constructors
- Type classes

## Installation

### Prerequisites

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- [Cabal](https://www.haskell.org/cabal/)

### Build & Run

```bash
git clone https://github.com/HCHogan/hhc.git
cd hhc
cabal update
cabal install --only-dependencies
cabal run
```

## Usage

Start the REPL by running:

```bash
cabal run
```

Then you can interact with the language:

```text
HM> let compose f g x = f (g x)
HM> :type compose
compose: forall a b c. (a -> b) -> (c -> a) -> c -> b
HM> let t1 = (\f -> let g = (f True) in f 3) (\x -> x)
Cannot unify types:
    Bool
with
    Int
HM> let t2 = let f = (\x -> x) in let g = (f True) in f 3
HM> t2
3 : Int
```

## Roadmap

Future enhancements include:

- Full support for user-defined algebraic data types
- Advanced type constructors and kind checking
- Typeclass definitions and instance resolution
- Module system

## Notes

I wrote several notes while implementing the compiler:
[implementing typeclasses](./docs/typeclass.md)

## License

This project is distributed under the MIT License. See [LICENSE](LICENSE) for details.

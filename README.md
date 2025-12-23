# F# Snippets

A curated collection of functional programming patterns and advanced techniques implemented in F#

## Overview

This repository showcases various functional programming concepts, from lazy evaluation and staging to type-level programming and advanced abstractions. Each snippet is self-contained and demonstrates idiomatic F# code, exploring the boundaries of what's possible in F#.

## Snippets

### Lazy Evaluation & Circular Programming

| File | Description |
|------|-------------|
| [LazyList.fsx](src/FSharpSnippets/LazyList.fsx) | Infinite lazy lists with functional operations |
| [Lazy fixed-point and Infinite Streams.fsx](src/FSharpSnippets/Lazy%20fixed-point%20and%20Infinite%20Streams.fsx) | Lazy fixed-point combinator with infinite stream examples |
| [A Lazy fixed-point combinator.fsx](src/FSharpSnippets/A%20Lazy%20fixed-point%20combinator.fsx) | Lazy fixed-point combinator for defining recursive values |
| [Fixed-point ﬁnding function.fsx](src/FSharpSnippets/Fixed-point%20ﬁnding%20function.fsx) | Classic fixed-point finding implementation |
| [GC Friendly Fixpoint.fsx](src/FSharpSnippets/GC%20Friendly%20Fixpoint.fsx) | Memory-efficient fixed-point combinator |
| [Polyvariadic fixpoint.fsx](src/FSharpSnippets/Polyvariadic%20fixpoint.fsx) | Fixed-point combinator with variable arity |
| [LazyBFSTreeLabeling.fsx](src/FSharpSnippets/LazyBFSTreeLabeling.fsx) | Breadth-first tree labeling using circular programming |
| [LazyGenNameLambda.fsx](src/FSharpSnippets/LazyGenNameLambda.fsx) | Fresh name generation using circular programs |
| [The repmin problem.fsx](src/FSharpSnippets/The%20repmin%20problem.fsx) | Bird's classic repmin problem using circular programming |
| [From Löb's Theorem to Spreadsheet Evaluation.fsx](src/FSharpSnippets/From%20Löb's%20Theorem%20to%20Spreadsheet%20Evaluation.fsx) | Löb's theorem as a spreadsheet evaluator |
| [From Löb's Theorem to Spreadsheet Evaluation (memoized).fsx](src/FSharpSnippets/From%20Löb's%20Theorem%20to%20Spreadsheet%20Evaluation%20(memoized).fsx) | Memoized version of spreadsheet evaluation |
| [Lazy Xml.fsx](src/FSharpSnippets/Lazy%20Xml.fsx) | Lazy XML processing |
| [Infinite sequences.fsx](src/FSharpSnippets/Infinite%20sequences.fsx) | Working with infinite sequences |

### Continuations & Control Flow

| File | Description |
|------|-------------|
| [Delimited continuations.fsx](src/FSharpSnippets/Delimited%20continuations.fsx) | Shift/reset operators for delimited continuations |
| [yin-yang puzzle.fsx](src/FSharpSnippets/yin-yang%20puzzle.fsx) | The famous Yin-Yang puzzle demonstrating first-class continuations |
| [Eff.fsx](src/FSharpSnippets/Eff.fsx) | Algebraic effects and handlers |
| [Eff via delimited continuations.fsx](src/FSharpSnippets/Eff%20via%20delimited%20continuations.fsx) | Implementing effects using delimited continuations |
| [Effects and Handlers.fsx](src/FSharpSnippets/Effects%20and%20Handlers.fsx) | Effect system with handlers |

### Monads & Abstractions

| File | Description |
|------|-------------|
| [Functor - Applicative - Monad.fsx](src/FSharpSnippets/Functor%20-%20Applicative%20-%20Monad.fsx) | Category theory abstractions in F# |
| [Abstracting over 'M'.fsx](src/FSharpSnippets/Abstracting%20over%20'M'.fsx) | Higher-kinded type abstraction patterns |
| [Monadic Memoization.fsx](src/FSharpSnippets/Monadic%20Memoization.fsx) | Memoization using monadic patterns |
| [Monadic Retry.fsx](src/FSharpSnippets/Monadic%20Retry.fsx) | Retry logic with monads |
| [Monadic Trampoline.fsx](src/FSharpSnippets/Monadic%20Trampoline.fsx) | Stack-safe recursion via trampolines |
| [Async Choice.fsx](src/FSharpSnippets/Async%20Choice.fsx) | Async workflows with choice operations |
| [Staged Free Monads.fsx](src/FSharpSnippets/Staged%20Free%20Monads.fsx) | Free monads with multi-stage programming |

### Multi-Stage Programming

| File | Description |
|------|-------------|
| [Staged Fixed-point combinator.fsx](src/FSharpSnippets/Staged%20Fixed-point%20combinator.fsx) | Staged fixed-point combinator |
| [Yet another staged fixed-point combinator.fsx](src/FSharpSnippets/Yet%20another%20staged%20fixed-point%20combinator.fsx) | Alternative staged fixed-point implementation |
| [Staged Ackermann.fsx](src/FSharpSnippets/Staged%20Ackermann.fsx) | Staged version of the Ackermann function |
| [Staged Parser Combinators.fsx](src/FSharpSnippets/Staged%20Parser%20Combinators.fsx) | Parser combinators with staging |
| [Staged Regular Expression Matcher.fsx](src/FSharpSnippets/Staged%20Regular%20Expression%20Matcher.fsx) | Staged regex matcher |
| [Staged CPS Regular Expression Matcher.fsx](src/FSharpSnippets/Staged%20CPS%20Regular%20Expression%20Matcher.fsx) | CPS-style staged regex matcher |
| [Staged Trampoline.fsx](src/FSharpSnippets/Staged%20Trampoline.fsx) | Staged trampoline for stack-safe recursion |
| [Staged Monoidal Folds.fsx](src/FSharpSnippets/Staged%20Monoidal%20Folds.fsx) | Staged folding operations |
| [Staged Reducers.fsx](src/FSharpSnippets/Staged%20Reducers.fsx) | Staged reducers for efficient data processing |
| [Staged HOAS.fsx](src/FSharpSnippets/Staged%20HOAS.fsx) | Higher-order abstract syntax with staging |
| [Staged typed formatting.fsx](src/FSharpSnippets/Staged%20typed%20formatting.fsx) | Type-safe formatting with staging |
| [Staged Functional Unparsing.fsx](src/FSharpSnippets/Staged%20Functional%20Unparsing.fsx) | Staged unparsing for pretty-printing |
| [Staged Ziria Streams.fsx](src/FSharpSnippets/Staged%20Ziria%20Streams.fsx) | Stream processing with staging |
| [Staged Parallel Ziria Streams.fsx](src/FSharpSnippets/Staged%20Parallel%20Ziria%20Streams.fsx) | Parallel stream processing with staging |
| [Lightweight Staged Numeric code.fsx](src/FSharpSnippets/Lightweight%20Staged%20Numeric%20code.fsx) | Staged numeric computations |
| [Stage your boilerplate.fsx](src/FSharpSnippets/Stage%20your%20boilerplate.fsx) | Generic programming with staging |

### Type-Level Programming

| File | Description |
|------|-------------|
| [HLists, Peano - Type-level computations.fsx](src/FSharpSnippets/HLists,%20Peano%20-%20Type-level%20computations.fsx) | Heterogeneous lists and Peano arithmetic at the type level |
| [Type-level Fold.fsx](src/FSharpSnippets/Type-level%20Fold.fsx) | Folding at the type level |
| [Type-level SAT solver.fsx](src/FSharpSnippets/Type-level%20SAT%20solver.fsx) | SAT solver implemented at the type level |
| [Generic Numeric Literals and Compile time Peano arithmetic.fsx](src/FSharpSnippets/Generic%20Numeric%20Literals%20and%20Compile%20time%20Peano%20arithmetic.fsx) | Compile-time arithmetic with Peano numbers |
| [Type Safe Higher-order abstract syntax.fsx](src/FSharpSnippets/Type%20Safe%20Higher-order%20abstract%20syntax.fsx) | Type-safe HOAS representation |
| [Session Types.fsx](src/FSharpSnippets/Session%20Types.fsx) | Session types for communication protocols |
| [Equality Type.fsx](src/FSharpSnippets/Equality%20Type.fsx) | Type equality proofs |

### Generic Programming & Data Types

| File | Description |
|------|-------------|
| [Generic Collections, Type Classes and friends.fsx](src/FSharpSnippets/Generic%20Collections,%20Type%20Classes%20and%20friends.fsx) | Type class patterns and generic collections |
| [Scrap Your Boilerplate.fsx](src/FSharpSnippets/Scrap%20Your%20Boilerplate.fsx) | Generic programming à la "Scrap Your Boilerplate" |
| [Scrap Your Boilerplate (with class).fsx](src/FSharpSnippets/Scrap%20Your%20Boilerplate%20(with%20class).fsx) | SYB using type classes |
| [Bananas.fsx](src/FSharpSnippets/Bananas.fsx) | Catamorphisms, anamorphisms, and other -morphisms |
| [Hughes's FuncList.fsx](src/FSharpSnippets/Hughes's%20FuncList.fsx) | Hughes's functional lists with O(1) append |
| [Hughes's CPSFuncList.fsx](src/FSharpSnippets/Hughes's%20CPSFuncList.fsx) | CPS-style functional lists |
| [Reducers.fsx](src/FSharpSnippets/Reducers.fsx) | Rich Hickey's reducers pattern in F# |

### Language Implementations & Interpreters

| File | Description |
|------|-------------|
| [Church numerals.fsx](src/FSharpSnippets/Church%20numerals.fsx) | Church encoding of natural numbers |
| [Unlambda.fsx](src/FSharpSnippets/Unlambda.fsx) | Interpreter for the Unlambda esoteric language |
| [Normalisation by evaluation.fsx](src/FSharpSnippets/Normalisation%20by%20evaluation.fsx) | Normalization by evaluation for lambda calculus |
| [Functional style Regex engine.fsx](src/FSharpSnippets/Functional%20style%20Regex%20engine.fsx) | Regular expression engine in functional style |
| [Type based Regex Active Patterns.fsx](src/FSharpSnippets/Type%20based%20Regex%20Active%20Patterns.fsx) | Type-indexed regex active patterns |
| [Quotations to A-Normal form.fsx](src/FSharpSnippets/Quotations%20to%20A-Normal%20form.fsx) | Converting F# quotations to A-normal form |

### Actors & Concurrency

| File | Description |
|------|-------------|
| [Actors acting as Lambdas.fsx](src/FSharpSnippets/Actors%20acting%20as%20Lambdas.fsx) | Actor model as lambda calculus |
| [Clojure's Atoms.fsx](src/FSharpSnippets/Clojure's%20Atoms.fsx) | Clojure-style atoms in F# |
| [Clojure atom memoization example.fsx](src/FSharpSnippets/Clojure%20atom%20memoization%20example.fsx) | Memoization using atoms |
| [Dining philosophers (Joinads).fsx](src/FSharpSnippets/Dining%20philosophers%20(Joinads).fsx) | Classic dining philosophers using joinads |

### Puzzles & Applications

| File | Description |
|------|-------------|
| [Countdown.fsx](src/FSharpSnippets/Countdown.fsx) | The countdown numbers game solver |
| [Eternity2.fsx](src/FSharpSnippets/Eternity2.fsx) | Eternity II puzzle solver |
| [Life.fsx](src/FSharpSnippets/Life.fsx) | Conway's Game of Life |
| [Evens and Odds via Z3.fsx](src/FSharpSnippets/Evens%20and%20Odds%20via%20Z3.fsx) | Using Z3 solver for even/odd problems |
| [Norvig's Spelling Corrector.fsx](src/FSharpSnippets/Norvig's%20Spelling%20Corrector.fsx) | Peter Norvig's spelling corrector in F# |
| [Erik Lippert's Comma Quibbling.fsx](src/FSharpSnippets/Erik%20Lippert's%20Comma%20Quibbling.fsx) | Comma quibbling problem solution |

### Utilities & Miscellaneous

| File | Description |
|------|-------------|
| [n-ary Seq.map.fsx](src/FSharpSnippets/n-ary%20Seq.map.fsx) | n-ary version of Seq.map |
| [n-ary Seq.map (Numerals).fsx](src/FSharpSnippets/n-ary%20Seq.map%20(Numerals).fsx) | n-ary Seq.map using Church numerals |
| [Y(n) Polyvariadic fixpoint.fsx](src/FSharpSnippets/Y(n)%20Polyvariadic%20fixpoint.fsx) | Polyvariadic Y combinator |
| [YIntensional.fsx](src/FSharpSnippets/YIntensional.fsx) | Intensional Y combinator |
| [Functional Unparsing SQL.fsx](src/FSharpSnippets/Functional%20Unparsing%20SQL.fsx) | SQL pretty-printing using functional unparsing |
| [Loop Unrolling.fsx](src/FSharpSnippets/Loop%20Unrolling.fsx) | Compile-time loop unrolling |
| [A simple Quine.fsx](src/FSharpSnippets/A%20simple%20Quine.fsx) | A self-reproducing program |

## Getting Started

### Prerequisites

- [.NET SDK](https://dotnet.microsoft.com/download) (F# included)
- A text editor or IDE with F# support (VS Code with Ionide, Visual Studio, or JetBrains Rider)

### Running Examples

Most snippets are self-contained F# script files (.fsx) that can be executed directly:

```bash
dotnet fsi "src/FSharpSnippets/Church numerals.fsx"
```

Or load them interactively in F# Interactive:

```bash
dotnet fsi
> #load "src/FSharpSnippets/Church numerals.fsx";;
```

## References

- [Using Circular Programs for Higher-Order Syntax](https://emilaxelsson.github.io/documents/axelsson2013using.pdf)
- [From Löb's Theorem to Spreadsheet Evaluation](http://blog.sigfpe.com/2006/11/from-l-theorem-to-spreadsheet.html)
- [Scrap Your Boilerplate: A Practical Design Pattern for Generic Programming](https://www.microsoft.com/en-us/research/publication/scrap-your-boilerplate-a-practical-design-pattern-for-generic-programming/)
- [Delimited Continuations in Operating Systems](http://okmij.org/ftp/continuations/)
- [Lightweight Modular Staging](https://scala-lms.github.io/)

## License

See [LICENSE](LICENSE) for details.

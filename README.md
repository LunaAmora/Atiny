<div align="center">

![banner]

&nbsp;

[Getting started](#getting-started) •
[Examples](#examples) •
[Roadmap](#roadmap)

&nbsp;

</div>

# Getting Started

Atiny is a simple compiler for a ML like language with some cool features from the future like algebraic effects that compiles to LLVM. To get started with the language clone the repo and build the compiler with cargo. 

# Installation

To install, clone the repository and then use `cargo install --path crates/atiny-cli` but if you 
want just to experiment with the language build it and use `cargo run -- <commands>`. You need to 
have LLVM 16 installed.

# Examples

```
type List t =
    | Cons t (List t) 
    | Nil

type Bool = 
    | True 
    | False

type User = {
    alive: Bool,
    numbers_chosen: List Int
}

fn main (x: List Bool) : Int {
    let user = User #{
        alive = True,
        numbers_chosen = Cons 1 (Cons 2 (Cons 3 Nil))
    };

    let number = user.numbers_chosen;

    match number {
        Cons x xs => x,
        Nil       => 0,
    }
}
```

# Roadmap

The objectives with the language are:

- [x] Surface Syntax
- [x] Parsing
- [x] Basic Type Checking
- [x] Exhaustiveness Checking
- [x] Elaboration
- [x] Records 
- [ ] Improve parser
    - [ ] Change it to a hand-written one
    - [ ] Add resilience and error recovery
- [ ] Compilation to MIR
    - [ ] Closure Conversion and Lambda Lifting
    - [ ] Inlining
    - [ ] Unboxing
- [ ] Compilation to LLVM
- [ ] Module Resolution
- [ ] Perceus Garbage Collection
- [ ] Improvements of the Type Checker
    - [ ] Type Classes
- [ ] Improvements of the Type Checker
    - [ ] Row polymorphism
    - [ ] Algebraic Effects
- [ ] Improvements on Compilation
    - [ ] Row polymorphism
    - [ ] Algebraic Effects
- [ ] Query System

[banner]: ./images/banner.png

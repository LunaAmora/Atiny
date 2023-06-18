<div align="center">

![banner]

&nbsp;

[Getting started](#getting-started) •
[Roadmap](#roadmap)

&nbsp;

</div>

# Getting Started

Atiny is a simple compiler for a ML like language with some cool features from the future like algebraic effects that compiles to LLVM. To get started with the language clone the repo and build the compiler with cargo.

# Roadmap

The objectives with the language are:

- [ ] Surface Syntax
- [ ] Parsing
- [ ] Basic Type Checking
- [ ] Exhaustiveness Checking
- [ ] Elaboration
- [ ] Records 
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

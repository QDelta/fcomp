# FCOMP

A compiler for a statically typed, purely functional programming language with lazy evaluation.

### Basics

- [x] Parser
- [x] Type checker
- [x] Compile to G-machine code
- [x] Compile to C
- [x] Garbage collector

### Features

- [x] Integer Primitives
- [x] Type inference
- [x] Parametric polymorphism
- [x] Let bindings
- [x] Lambda expressions
- [ ] Type classes
- [ ] Higher kinded types
- [ ] Monadic IO
- [ ] Bootstrapping

### Example

```bash
make
build/fcomp examples/nprime.src
gcc -O2 build/main.c -o build/main
echo "64" | build/main
```

[Implementing Functional Languages: a tutorial](https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/)

[Write you a haskell](http://dev.stephendiehl.com/fun/index.html)

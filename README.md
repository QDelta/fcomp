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
- [ ] Parametric polymorphism
- [ ] Let bindings
- [ ] Lambda expressions
- [ ] Ad hoc polymorphism
- [ ] Monadic IO
- [ ] Bootstrapping

### Example

```bash
make
./Main examples/nprime.src build/main.c
gcc -O2 build/main.c -o build/main
echo "64" | build/main
```

[Implementing Functional Languages: a tutorial](https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/)

[Write you a haskell](http://dev.stephendiehl.com/fun/index.html)

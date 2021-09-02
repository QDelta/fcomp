# FCOMP

A statically-typed non-strict pure-functional programming language with an S-expression like syntax.

### Basics

- [x] Parser
- [x] Type checker
- [x] Compile to some abstract machine code (e.g. G-machine)
- [x] Interpreter
- [x] Compile to C
- [x] Garbage collector

### Features

- [x] Integer Primitives
- [ ] Type inference
- [ ] Polymorphism
- [ ] Let bindings
- [ ] Lambda expressions
- [ ] Monadic IO
- [ ] Optimizations
- [ ] Bootstrapping

### Example

```bash
make
./Main examples/nprime.src build/main.c
gcc -O2 build/main.c -o build/main
echo "64" | build/main
```

[ref](https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/)
# FCOMP

A compiler for a statically typed, purely functional programming language with lazy evaluation.

### Features

- [x] Algebraic data types
- [x] Integer primitives
- [x] Type inference
- [x] Parametric polymorphism
- [x] Let bindings
- [x] Lambda expressions
- [ ] Type classes
- [ ] Higher kinded types
- [ ] Monad, do notation
- [ ] Monadic IO
- [ ] Bootstrapping
- [ ] Infix operators, indentation based parsing, ...

### Example

```bash
make
build/fcomp examples/nprime.src
gcc -O2 build/main.c -o build/main
echo "64" | build/main
```

[Implementing Functional Languages: a tutorial](https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/)

[Write you a haskell](http://dev.stephendiehl.com/fun/index.html)

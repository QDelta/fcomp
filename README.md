# FCOMP

A compiler for a statically typed, purely functional programming language with lazy evaluation.

### Features

- [x] Algebraic data types
- [x] Integer primitives
- [x] Type inference
- [x] Parametric polymorphism (for global definitions)
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
make test SRC=examples/nprime.src INPUT=64
```

[Implementing Functional Languages: a tutorial](https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/)

[Write you a haskell](http://dev.stephendiehl.com/fun/index.html)
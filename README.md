## Philosophy

How to interpret the scheme language ?

Ways are introduced by many books.

But they all too tedious.

As the scheme language insist a philosophy of conciseness,

I decide to write an interpreter as brief as possible.

## Orthogonality

Many feature in the scheme lanauge are orthogonal.

They can be implemented independently.

For example, continuation and hygienic-macro.

To avoid to implement them in a whole interpreter,

helps us to minimize the code.

## Reference

There is a process of thinking to create them,

I also write Chinese blog to explain it.

[词法作用域是怎样实现的](https://thzt.github.io/lisp/2015/08/15/implementation-of-lexical-scope/)

[call/cc是怎样实现的](https://thzt.github.io/lisp/2015/08/20/implementation-of-callcc/)
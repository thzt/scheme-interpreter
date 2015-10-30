## Philosophy

How to interpret the scheme language ?

Ways are introduced by many books.

However, they are all very tedious.

As the scheme language insist a philosophy of conciseness,

I decide to write an interpreter as brief as possible.

## Orthogonality

Many features in the scheme language are orthogonal.

They can be implemented independently, 

such as, the continuation and the hygienic macro.

To avoid to implement them in a whole interpreter,

I minimize the code.

## Reference

There is a process of thinking to create them,

I also write Chinese blog to explain it.

[词法作用域是怎样实现的](http://thzt.github.io/implementation-of-lexical-scope/)

[call/cc是怎样实现的](http://thzt.github.io/implementation-of-callcc/)

# lcbf
Lambda calculus to Brainfuck

This is an attempt at a compiler that will compile a simple untyped lambda calculus into brainfuck code. This should be possible by the Church-Turing thesis.

The current implementation contains a brainfuck interpreter, a lambda calculus interpreter (with beta reductions). A LC to SKI-Combinator compiler.

The future aims of this project is to convert a SKI-Tree into BF code. It may be possible to use other combinators, but for a proof of concept SK should be fine. It may also be possible to do some graph reduction on a SKI-Tree in order to make it more accesable although during the lambda calculus phase it is fully beta reduced to "normal" form.

The current ideas for how to implement the BF code is to make some sort of stack in BF and have the functions be interpreted in the BF. This would essentially be a graph traversal of the SKI-tree with reduction rules all in BF which seems unnecessery.

The other option is to compile SK directly to BF however this seems difficult to understand for now.

# Correctness-preserving transformations for miniKanren: `mk-cpts.scm`

Defines the following:
* The macro `defrel-optimized`. Equivalent to `defrel` from The Reasoned Schemer 2nd ed. (https://github.com/TheReasonedSchemer2ndEd/CodeFromTheReasonedSchemer2ndEd/blob/master/trs2-impl.scm). Only supports `fresh`, `conde`, and `==`. It does not support `=/=` or constructs from the host language, e.g., `define`, `lambda`, and forms of `let`.
* The function `apply-cpts` which takes a quoted `defrel` as argument.

Currently, `apply-cpts` only performs two steps:
1. `single-fresh`, which rewrites the `defrel` to use one top-level `fresh` statement. Assumes unique names were given to the fresh variables.
2. `optimize-condes`, which greedily extracts out the relation that appears in the most conde branches. It recurses until no more relations can be extracted.

This codebase relies on the following nonstandard Scheme constructs:
* Pattern matching from `pmatch.scm` (Oleg Kiselyov, https://github.com/webyrd/quines/blob/master/pmatch.scm)
* One occurence of `(gensym)` from Chez Scheme (https://www.scheme.com/csug6/objects.html)

# How to use `mk-cpts.scm`
Have a copy of `pmatch.scm` in your home directory, `~/pmatch.scm`.

When loading `mk-cpts.scm`, load it alongside `trs2-impl.scm`. You can see an example of this in [test.scm](test.scm).

Replace any `defrel`s in your miniKanren code with `defrel-optimized`. Or, if you prefer, run `apply-cpts` directly in your editor (using something like [sexp-rewrite](https://github.com/rmculpepper/sexp-rewrite)).

# Future improvements to this codebase
## Features
* More CPTs, such as removing redundant goals.
* Support for `=/=`.

## Code quality
* Algorithmic improvements.
* Rewrite instances of `letrec*` where `let` would do fine. Same for `equal?` to `eqv?`, `eq?`.
* Rewrite `syntax-rules` to not use `eval`.
* Turn top-level definitions into local definitions, where possible, to clean up the namespace.
* Maybe even remove instances of `pmatch`.
* Use Typed Racket types in the comments rather than Haskell types. Maybe even make a full Racket implementation.
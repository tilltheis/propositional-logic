# Propositional Logic
Convert a propositional formula into a given normal form or simplify it.

# NOTE
This is not usable as a program yet! I'm still writing the library code.

# Usage
    propositional-logic [-o operation=simplify] input-formula

## Operations
* CNF (Conjunctive normal form)
* DNF (Disjunctive normal form)
* NNF (Negation normal form)
* Simplify

## Formula Format
* Conjunction: `^` (caret)
* Disjunction: `v` (lower v)
* Negation: `!` (bang)

## Examples
    $ propositional-logic A v !A
    True

    $ propositional-logic !!(Var ^ Var) v NotVar
    Var v NotVar


# Plans For The Future
- Compile to JavaScript (with Emscripten) and embed it in a website

# License
MIT

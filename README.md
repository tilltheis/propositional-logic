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
Copyright (C) 2011 by Till Theis, http://www.tilltheis.de

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

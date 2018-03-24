# Macros

When we do `~name`, in defmacro, `name` itself will not be evaluated,
because arguments of macros are not evaluated.

It is `'~name` instead of just `~name`, because we want to allow people to do

```
(domain foo)
```

to get

```
{:content [], :attrs {:name "foo"}, :tag :domain}
```

If you did just `~name`, then at evaluation time it would try and resolve foo,
and probably fail

# Quoting

+ ' quote
+ ` syntax-quote

# Evaluation

When a collection is evaluated, each of its contained items is evaluated first,
unless it is a list that starts with
+ the name of a macro
+ a special operator

A symbol is evaluated to a local, a var, or a Java class name
Literal scalar values evaluate to themselves
Vectors: start by evaluating the containing items; vector itself evaluates to itself

`(quote ...) or `'` prevents its argument from being evaluated at all;
it evaluates to just the symbol

The empty list `()` evaluates to itself - special case,
no need to quote the empty list in Clojure

` ` ` prevents arguments from being evaluated, but auto-qualifies all unqualified symbols.
Within a syntax-quoted form, prefix forms with `~` (an unquote) to mark as "requiring evaluation"
Prefix forms with `~@` to mark as "requiring evaluation, and after evaluating, splice here"

A var is named by a symbol and holds a value.

# REPL phases

Read -> Expand -> Compile -> Eval -> Print -> [(Loop)]

Read step transforms text to data structures.

Eval step takes a data structure representing a Clojure expression,
evaluate it, and return the result.

# Controlling symbol resolution time

Functions accept and return values that are meaningful at runtime.

Macros accept and return code forms that are meaningful at compile time.

Symbols have subtleties depending on
+ Whether it's fully qualified
+ Resolution time
+ Lexical context

# Name Capture

Problem when a name generated at compile time clashes with a name that exists at runtime.

Clojure macros are usually safe from this because the use of syntax-quote resolves symbols
at macro-expansion time.

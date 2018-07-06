# ImLF
ImLF is an impure lazy functional language. Functions are allowed to have side effects and programs are evaluated as lazily as possible to still preserve the proper (as in eager evaluation) order of execution of side effects.

In ImLF there are no special constructs for modeling side effects, all functions are defined in the same way as ordinary pure functions, pure/lazy and impure code can be easily mixed.

## Examples

- Pure infinite list passed to impure head function. There is no infinite loop - only first element is evaluated and `print` inside `head` is evaluated before `print` inside `main`
```
data List a
    = Cons(a,List a)
    | Nil;

-- seq has no special compiler support, it is ordinary function --
seq = (a,b) -> b;

undefined = undefined;

head = (xs) ->
    case xs of
        Cons(x,xs) -> seq(print(+(x,1)),x)
      | Nil -> undefined;

repeat = (x) ->
    Cons(x,repeat(x));

main = print(head(repeat(1)))
```
Output
```
2
1
```

- Pure and impure functions passed to higher order function. Function `streamf` will construct pure infinite list if we pass pure function to it and it will try to create fully evaluated infinite list when we pass impure function to it. Latter case results in infinite loop
```
data List a
    = Cons(a,List a)
    | Nil;

seq = (a,b) -> b;

undefined = undefined;

head = (xs) ->
    case xs of
        Cons(x,xs) -> x
      | Nil -> undefined;

streamf = (f) ->
    Cons(f(0),streamf(f));

-- f is pure --
f = (x) -> +(x,1);

-- g is impure --
g = (x) -> seq(print(x),x);

main = seq( print(head(streamf(f)))
          , head(streamf(g)) )
```

Output
```
1
0
0
0
... (infinite stream of zeros)
```

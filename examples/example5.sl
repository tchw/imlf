data List a
    = Cons(a,List a)
    | Nil;

seq = (a,b) -> b;

undefined = undefined;

head = (xs) ->
    case xs of
        Cons(x,xs) -> x
      | Nil -> undefined;

-- streamf will try to construct fully evaluated infinite list when passed impure function --
streamf = (f) ->
    Cons(f(0),streamf(f));

-- f is pure --
f = (x) -> +(x,1);

-- g is impure --
g = (x) -> seq(print(x),x);

main = seq( print(head(streamf(f)))
          , head(streamf(g)) )
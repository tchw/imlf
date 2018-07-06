data List a
    = Cons(a,List a)
    | Nil;

seq = (a,b) -> b;

undefined = undefined;

map = (f,xs) ->
    case xs of
        Cons(x,xs) -> Cons(f(x),map(f,xs))
      | Nil -> Nil;

take = (n,xs) ->
    case ==(n,0) of
        True  -> Nil
      | False -> case xs of
                     Cons(x,xs) -> Cons(x,take(-(n,1),xs))
                   | Nil        -> undefined;

-- f has side effects --
f = (x) ->
    seq(print(x),+(x,1));

-- g is pure --
g = (x) ->
    +(x,1);

repeat = (n) ->
    Cons(n,repeat(n));

main =
    seq( -- here map is passed function with side effects --
         map(f,Cons(0,Cons(1,Nil)))
	 -- here map is passed pure function --
       , print(take(5,map(g,repeat(1)))) )
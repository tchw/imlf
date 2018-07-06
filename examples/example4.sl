data List a
    = Cons(a,List a)
    | Nil;

seq = (a,b) -> b;

undefined = undefined;

head = (xs) ->
    case xs of
        Cons(x,xs) -> seq(print(+(x,1)),x)
      | Nil -> undefined;

repeat = (x) ->
    Cons(x,repeat(x));

main = print(head(repeat(1)))
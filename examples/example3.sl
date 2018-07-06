seq = (a,b) -> b;

undefined = undefined;

-- foo(x) is treated as function with side effects even if x is False --
-- it has to be because x in general may be arbitrarily complex computation --
foo = (x) ->
    case x of
        True  -> (x) -> seq(print(x),x)
      | False -> (x) -> undefined;

const = (a,b) -> a;

-- const drops its second argument but foo(False,0) is evaluated, program hangs --
main =
    const(0,foo(False,0))
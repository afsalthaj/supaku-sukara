## Full Type Inference
Type signature is optional in Haskell

`f x = x+1`

instead of `def f(x: Int): Int = x + 1`

Now lets try to find the type of the above function `f` in Haskell

Prelude> :t f
f :: Num a => a -> a
Prelude>
Prelude>

This implies `a` should have an instance of type class `Num` where `+` is defined.
And it takes an `a` and returns an `a`

If you don't want the generic type for your function

`Prelude> g:: Int -> Int; g x = x + 1`

Example of how to use:

```
Prelude>
Prelude> g 3
4
Prelude>
Prelude> :t g
g :: Int -> Int
Prelude>
Prelude> f (g 3)
5
Prelude> f $ g 3
5
```
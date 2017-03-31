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

Internally what Haskell does is

`($) f x = f x`

Now the function `$` act as infix operator, as it was wrapped in brackets

Example:

```
Prelude> ($$) f x = f x
Prelude> :t $$
Prelude> :t ($$)
($$) :: (t -> t1) -> t -> t1

```

## Function Composition

In Scala

```
scala> def f (x: Int) = x + 1
f: (x: Int)Int

scala> def g (x: Int) = x + 1
g: (x: Int)Int

scala> f compose g
<console>:10: error: missing arguments for method f;
follow this method with `_' if you want to treat it as a partially applied function
              f compose g
              ^

scala> f _ compose g
res1: Int => Int = <function1>

scala>


```

Inn Haskell


```

// Point Free
Prelude> h = f . g
Prelude> h 3
5
Prelude>

// Not a Point Free - a bit verbose
Prelude> h x = f . g $ x
Prelude> h 3
5
Prelude>

```


## Anonymous functions

In Scala

```
((x: Int) => x + 1) (3)
```

In Haskell

```

Prelude> f = \x -> x + 1
Prelude> f 3
4
Prelude> (\x -> x + 1) 3
4
Prelude>

Prelude> x = (+1)
Prelude> x 3
4
Prelude> :t x
x :: Num a => a -> a
Prelude>

```


## Parametric Polymorphism

```
Prelude> myLength xs = foldr (\ _ acc -> acc + 1) (0) (xs)
Prelude>
Prelude> :t myLength
myLength :: (Foldable t, Num a1) => t a -> a1
Prelude>

```

instead of


```
import scalaz._, Scalaz._

def myLength[A, F[_] : Foldable](xs: F[A]): Int = {
  xs.foldRight(0)((_, acc) => 1 + acc)
}

```

## Currying

All functions are curried by default

In Scala

```
def add (x: Int)(y: Int) = x + y

```

In Haskell

Functions in Haskell are curried by default

```
add x y = x + y

```


## Types

Value class in Scala

```
case class Age (age: Int) extends AnyVal

```

```
newtype Age = Age { age :: Int } deriving (Show, Eq, Ord, Functor)

```

No runtime penalty


## Product Types

```
data Person = Person String Age

```
You can assume what this is in Scala
or you can provide accessors to the String and Age if you need.

## Sum Types

In Scala

```
trait Boolean

case object True extends Boolean

case object False extends Boolean

```

In Haskell


```
data Boolean = True | False deriving (Show, Eq, Ord)

```

## Creating functor instance for Free !

You don't get this in scala, unless you import scalaz._ and implement `map` function

```

newtype Age a = Age { age :: a } deriving (Show, Eq, Ord, Functor)

```

!!! Simply means Haskell is so straight forward !!!
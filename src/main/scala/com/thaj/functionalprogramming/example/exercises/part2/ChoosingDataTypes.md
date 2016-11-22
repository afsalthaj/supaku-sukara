#### Data type design

```
// A Unit of parallelism
def unit[A](a: => A): Par[A]
// Get the result of parallel computation
def get[A](a: Par[A]): A
```

We avoided the use of get function to avoid the possibilities of delay and subsequent sequential operation. Or in other words, usage of get results in a situation in which the the whole process (on the left side in the first instance) seem to be waiting until unit operation is `executed`


```
// we represent parallel computation using Par, and to combine two Pars
def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
```

```
def sum(ints: IndexedSeq[Int]): Par[Int] =
if (ints.size <= 1)
  Par.unit(ints.headOption getOrElse 0)
else {
  val (l,r) = ints.splitAt(ints.length/2)
  Par.map2(sum(l), sum(r))(_ + _)
}
```

The whole parallel computation is represented by the box called Par. We have a map2 function that simply combines two parallel computation. This is what we want to achieve. In the above integer sum function, we tried to parallelize two parallel computation (or perhaps we wanted to do so). As a part of it, the final statement of the function is calling map2 function that returns the combined effect two parallel computations. Hence the return type of the whole function is Par, and not Int.

>Why unit is accepting a lazy argument? And not strict in the argument?

We will *come back* to this question later on.

> Is map2 strict in its arguments?

At this moment, YES! It is strict in its arguments. We also wanted to make sure that the arguments passed to map2 function have to be executed in parallel, as they are independent from each other.

#### If arguments to map2 were strict:

```
sum(IndexedSeq(1,2,3,4))

```

The execution flow is :

```
map2(
  sum(IndexedSeq(1,2)),
  sum(IndexedSeq(3,4))
)(_ + _)

map2(
  map2(
    sum(IndexedSeq(1)),
    sum(IndexedSeq(2))
  )(_ + _),
  sum(IndexedSeq(3, 4))  
)(_ + _)

map2(
  map2(
   unit(1),
   unit(2)
  )(_ + _),
  sum(IndexedSeq(3, 4))
)(_ + _)

map2(
  map2(
   unit(1),
   unit(2)
  )(_ + _),
  map2(
   sum(IndexedSeq(3)),
   sum(IndexedSeq(4))
  )(_ + _)
)(_ + _)

map2(
  map2(
   unit(1),
   unit(2)
  )(_ + _),
  map2(
   unit(3),
   unit(4)
  )(_ + _)
)(_ + _)

...

// Here sum(IndexedSeq(3, 4)) is evaluated only after sum(IndexedSeq(1, 2)) is fully expanded.. The unfortunate consequence of map2 being strict in its arguments.
```

Because map2 is strict, and Scala evaluates argument from left to right, whenever we want to encounter `map2(sum(x), sum(y))(_ + _)`, we have to then evaluate sum(x) and so on recursively. This has a further implication that it is still evaluating the left branch of the tree before  moving on to strictly confirm the right half.

And if map2 evaluates its arguments in parallel, this implies left half of our computation will start even before we begin constructing the right half of our computation.

#### If arguments to map2 were strict, but doesn't execute?


Does this help? If map2 doesn’t begin evaluation immediately, this implies a Par value is merely constructing a description of what needs to be computed in parallel. Nothing actually occurs until we evaluate this description, perhaps using a get-like function. The problem is that if we construct our descriptions strictly, they’ll be rather heavy- weight objects. Looking back at our trace, our description will have to contain the full tree of operations to be performed:

```
map2(
  map2(
    unit(1),
    unit(2)
  )(_ + _),
  map2(
    unit(3),
    unit(4)
  )(_ + _)
)(_ + _)
```

#### What is map2 is lazy in its arguments?

It seems we should make map2 lazy and have it begin immediate execution of both sides in parallel. This also addresses the problem of giving neither side priority over
the other.

map2 is lazy, and hence the description isn't formed then and there itself. Not only we want map2 lazy, we want it to execute both of its side immediately in parallel.

###### What is the issue with this approach?
Is it always the case that we want to implement both sides of map2 in parallel? Probably not! Consider the below example:

```
Par.map2(Par.unit(1), Par.unit(2))(_ + _)

```
In this case, we happen to know that the two computations we’re combining will exe- cute so quickly that there isn’t much point in spawning off a separate logical thread to evaluate them. But our API doesn’t give us any way of providing this sort of information. That is, our current API is very inexplicit about when computations get forked off the main thread—the programmer doesn’t get to specify where this forking should occur.

#### Explicit forking?

What if we make the forking explicit?

```
// this means that the given Par should execute in a separate logical thread.
def fork[A](a: => Par[A]): Par[A]
```

```
def sum(ints: IndexedSeq[Int]): Par[A] =
  if(ints.length <= 1)
    Par.unit(ints.headOption.getOrElse(0))
  else {
    val (l, r) = ints.splitAt(ints.length/2)
    Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
  }  
```

We solved two problems here: We explicity told the program that the two computations given to map2 should
execute in separate logical thread. We could also combine two parallel computations.

Based on the above program, making unit lazy doesn't give any advantage. Hence, let us define a strict unit function.

```
def unit[A](a: A): Par[A]
def lazyUnit[A] (a: => A) = fork(unit(a))
```

If fork begins evaluating its argument immediately in parallel, the implementation must clearly know something, either directly or indirectly, about how to create threads or submit tasks to some sort of thread pool. Moreover, this implies that the thread pool (or whatever resource we use to implement the parallelism) must be (globally) accessible and properly initialized wherever we want to call fork.4 This means we lose the ability to control the parallelism strategy used for different parts of our program. And though there’s nothing inherently wrong with having a global resource for executing parallel tasks, we can imagine how it would be useful to have more fine-grained control over what implementations are used where (we might like for each subsystem of a large application to get its own thread pool with different parameters, for example). It seems much more appropriate to give get the responsi- bility of creating threads and submitting execution tasks.


In contrast, if fork simply holds on to its unevaluated argument until later, it requires no access to the mechanism for implementing parallelism. It just takes an unevaluated Par and “marks” it for concurrent evaluation. Let’s now assume this meaning for fork. With this model, Par itself doesn’t need to know how to actually implement the parallelism. It’s more a description of a parallel computation that gets interpreted at a later time by something like the get function.

This is a slight shift from the concepts that we learned. We saw Par as a container of a value that we could simply `get` when it becomes available. Now Par is more like a first class Program that we can run. It represents parallel computation, but may not deal with the actual mechanism of running programs using thread pool. So lets run Par using `run` function

```
def run[A](a: Par[A]): A

```
Because Par is now just a pure `data structure`, run has to have some means of imple- menting the parallelism, whether it spawns new threads, delegates tasks to a thread pool, or uses some other mechanism.

So our API consist of:


```
def unit[A](a: A): Par[A]
def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C]
def fork[A](a: => Par[A]): Par[A]
def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
def run[A](a: Par[A]): A

```

Ok, now you know we need these functionalities. A Par to represent parallel computation.
A unit promotes a constant value to a parallel computation,
A map2 combining the results of two parallel computations with a binary function.
A fork marking􏰀 lazyUnit wraps its unevaluated argument in a Par and marks it for concurrent
evaluation. a computation for concurrent evaluation. The evaluation won’t
actually occur until forced by run.
􏰀A lazyUnit wraps its unevaluated argument in a Par and marks it for concurrent
evaluation.
A run extracts a value from a Par by actually performing the computation.



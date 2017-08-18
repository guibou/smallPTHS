# smallpths

This is a rewrite of [smallpt](http://www.kevinbeason.com/smallpt/) in
Haskell, with a strong emphasis on performance.

You can run a benchmark compared to the original smallpt (the explicit
version) using `Bench`:

```
stack bench
```

Result:

```
......
......
......

Haskell is 3.1816533299577854 times slower.
```

# Benchmark results

Current unacceptable values :

- Intel Core i7-4700HQ @ 2.4 GHz (1 core) : 3.18x slower


# Code quality consideration

The initial aim of this rewrite was to get something as close as
possible to the original smallpt code. Once we'll get something which
is fast enough, we'll start writing something clean. That's why
variable name are usually single letter or why a the `radiance`
function is too long. I'm not writing code like that in real life ;)

Some part of smallpt (the cpp version) are not written for an
immutable context. Hence there is a lot of variable "mutated" by
creating a new one with an added prime (i.e `f` became `f'`).

Seriously, this will be rewritten with a cleaner approach soon, but I
wanted to be sure that I'm executing something similar as the original
smallpt code.

# Benchmark consideration

Due to a difference in random number generation, we are not sure
that's really the same code which is executed. However, the current 3x
slowdown cannot be just because of a totally different space
exploration.

# image output

```
stack build
stack exec SmallPTHS -- 4 output.ppm
```

or, for the CPP version

```
make -C CPP
./CPP/smallpt 4 output.ppm
```
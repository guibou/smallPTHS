# smallpths

This is a rewrite of [smallpt](http://www.kevinbeason.com/smallpt/) in
Haskell, with a strong emphasis on performance.

The current projet status is undefined : the Haskell code is now
really different from the C++ code due to some refactoring I did, and
I wanted to add new features. Soon I'll split the projet in two parts :

- One with an Haskell code similar to the C++ code, with a benchmark comparing both
- One with an Haskell code which con evolve, as I want to clean this
  mess and introduce new feature with a more type safe approach.

# Benchmark

You can run a benchmark compared to the original smallpt (the explicit
version) using `Bench`:

```
stack bench
```

# Benchmark results

Current unacceptable values :

- Laptop: Intel Core i7-4700HQ @ 2.4 GHz (4 cores, 8 threads) : 1.40x slower in linear, 1.7x slower in parallel.
- Workstation: Intel Core i7-6800K CPU @ 3.40GHz (6 cores, 12 threads) : 1.60x slower in linear, 3.2x slower in parallel.

Apparently, it does not scale with many cores.

# Benchmark consideration

Due to a difference in random number generation, we are not sure
that's really the same code which is executed. However, the current
slowdown cannot be just because of a total different space
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

# Difference with original smallpt

We implemented some differences with the original smallpt, you can see them with `git log CPP/smallpt.hs`. Mostly :

- Light are extracted from the sphere list, hence lighting is `O(nbLights)` instead of `O(nbSpheres)`.
- Output (console and image on the disk) are disabled for benchmarking
- There is only one ray per recursion, instead of the initial combinatorial explosion

TODO: I'll rollback theses changes soon to get a benchmark equivalent to the original C++ code

# Difference between the C++ and Haskell version

During direct lighting, the C++ version use the sphere offset to
compare the current light with the intersected object. If they are the
same, there is no intersection.

I was not able to do something similar (and performant) in Haskell, so
instead I compare the distance of the intersection with the distance
of the intersection with the current light. If they are the same,
there is no intersection.

TODO: I'll rollback theses changes soon to get a benchmark equivalent to the original C++ code.

# Performance questions

- Why is the parallel version so "slow"
- There is a space leak somewhere, it should take approximatly 18M (1024 x 768 x 3 * sizeof(Double)) in memory, and it takes 50M. Profiler says it is `STACK`, I don't know.
- Unboxed vector on the list of sphere does not help
- `Strict` is enable, leading to better performance, however it may be interesting to understand why, and make the smartest choice when possible. For example, I'm not sure that unpacking + stict on the field of `Intersect` is a good thing (it will copies a lot a `Sphere`)

# Room for improvement

The C++ and Haskell version can be improved in many ways for
performance (I'm not discussing ray tracing science here).

- Better vector rotation scheme
- Using Float instead of Double everywhere (Double are just here to
  help for the scene hack where walls are big spheres)
- Terminal recursion?

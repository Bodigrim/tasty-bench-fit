# tasty-bench-fit

Benchmark a given function for variable input sizes and find out its time complexity.

```haskell
> fit $ mkFitConfig (\x -> sum [1..x]) (10, 10000)
1.2153e-8 * x
> fit $ mkFitConfig (\x -> Data.List.nub [1..x]) (10, 10000)
2.8369e-9 * x ^ 2
> fit $ mkFitConfig (\x -> Data.List.sort $ take x $ iterate (\n -> n * 6364136223846793005 + 1) (1 :: Int)) (10, 10000)
5.2990e-8 * x * log x
```

Irregular overhead caused by generational GC may skew observed asymptotics a lot. Sometimes you are lucky to measure things like in-place vector sort or fused list operations like `sum [1..x]`, which do not allocate much. But otherwise you are likely to need a mitigation strategy:

* Consider running measurements with `-O0` or in `ghci` prompt. This is how the usage example above was generated. Without optimizations you will allocate a lot and trigger GC regularly, evening out its effect.

* Another option is to set `+RTS -A128K` roughly to the same effect: smaller nursery causes more regular GC. In this case it is advisable to set the lower bound of input sizes to a relatively large value.

* If it does not work, push benchmarking interval higher, so that the main asymptotic term reveals itself more prominently, and decrease `RelStDev`. This unfortunately does affect running times significantly.

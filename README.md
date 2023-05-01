# tasty-bench-fit

Benchmark a given function for variable input sizes and find out its time complexity.

```haskell
> fit $ mkFitConfig (\x -> sum [1..x]) (10, 10000)
1.2153e-8 * x
> fit $ mkFitConfig (\x -> Data.List.nub [1..x]) (10, 10000)
2.8369e-9 * x ^ 2
> fit $ mkFitConfig (\x -> Data.List.sort $ take (fromIntegral x) $ iterate (\n -> n * 6364136223846793005 + 1) (1 :: Int)) (10, 100000)
5.2990e-8 * x * log x
```

One can usually get reliable results for functions, which do not allocate much: like in-place vector sort or fused list operations like `sum [1..x]`.

Unfortunately, fitting functions, which allocate a lot, is likely to be disappointing: GC kicks in irregularly depending on nursery and heap sizes and often skews observations beyond any recognition. Consider running such measurements with `-O0` or in `ghci` prompt. This is how the usage example above was generated. Without optimizations your program allocates much more and triggers GC regularly, somewhat evening out its effect.

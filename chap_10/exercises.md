# Chapter 10 Exercises

Catamorphisms are a means of deconstructing data 
Where map applies a function to each member of a list and returns a list, a fold replaces the cons constructors with the function and reduces the list


### 10.5 "Fold Left" Intermission

`foldl` and `foldr` definitions as reference

```haskell
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

-- Self-calls (tail-call) through the list, only beginning to produce values after it’s reached the end of the list.
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs
```

```
foldr (*) 1 [1..5]
(*) 1 (foldr (*) 1 [2..5])
(*) 1 ((*) 2 (foldr (*) 1 [3..5]))
(*) 1 ((*) 2 ((*) 3 (foldr (*) 1 [4..5])))
(*) 1 ((*) 2 ((*) 3 ((*) 4 (foldr (*) 1 [5]))))
(*) 1 ((*) 2 ((*) 3 ((*) 4 ((*) 5 (foldr (*) 1 []))))))
-- foldr f acc [] = acc
-- foldr (*) 1 [] = 1
(*) 1 ((*) 2 ((*) 3 ((*) 4 ((*) 5 (1))))))
(*) 1 ((*) 2 ((*) 3 ((*) 4 5)))
(*) 1 ((*) 2 ((*) 3 20))
(*) 1 ((*) 2 60))
(*) 1 120
120

foldr (^) 2 [1..3]
(1 ^ (2 ^ (3 ^ 2)))
(1 ^ (2 ^ 9))
1 ^ 512
1
```

```
foldl (*) 1 [1..5]
foldl (*) ((*) 1 1) [2..5]
foldl (*) ((*) 1 2) [3..5]
foldl (*) ((*) 2 3) [4..5]
foldl (*) ((*) 6 4) [5]
foldl (*) ((*) 24 5) []
-- foldl f acc [] = acc
120 

foldl (^) 2 [1..3]
((2 ^ 1) ^ 2) ^ 3
(2 ^ 2) ^ 3
4^3
64
```

### Understanding Folds:

If your folding function isn’t commutative, a left fold can lead to a different result than a right fold of the same.

1.b
2. foldl (flip (*)) 1 [1..3]
  foldl (flip (*)) (flip (*) 1 1) [2,3]
  foldl (flip(*)) (flip (*) 1 2) [3]
  foldl (flip(*)) (flip (*) 2 3
  ((1 * 1) * 2) * 3)
3. c.
4. a
5. 
a. foldr (++) ""  ["woot", "WOOT", "woot"]
b. foldr max "" ["fear","is","the","little","death"]
c. 
d. no
e. foldl (\x -> (++)(x) .show) "" [1..5]
f. foldr (flip const) 'a' [1..5]
g. foldr (flip const) 0 "tacos"
h. foldl const 0 "burritos"
i. foldl const 'z' [1..5]

### Questions:
1) Understanding Folds: 361
foldr and True [False, True]
2) is it possible to solve mostRecent 367 w/o foldr1


### Good Functions:
foldr
const - ignore second argument
scanr - Scans are similar to folds but return a list of all the intermediate stages of the fold.
foldr1 - no explicit argument necessary
(!!) :: [a] -> Int -> a
takeWhile
elem

### Good resources:
http://pointfree.io/
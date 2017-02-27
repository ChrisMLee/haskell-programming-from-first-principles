# Chapter 20 exercises

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

### Exercises: Library Functions
see [library_functions.hs](./library_functions.hs)

### Links
http://javran.github.io/posts/2014-02-28-type-tetris-and-typeclassopedia.html

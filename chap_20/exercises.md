#chap 20 exercises

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

### Links
http://javran.github.io/posts/2014-02-28-type-tetris-and-typeclassopedia.html
# Chapter 21 Exercises  
Traversable is commonly described as a way to traverse a data structure, mapping a function inside a structure while accumulating the applicative contexts along the way.  

In a literal sense, anytime you need to flip two type constructors around, or map something and then flip them around, that’s probably Traversable  

traverse = fmap and then flip structures


### Good Functions
`catMaybes :: [Maybe a] -> [a]`  
`Prelude> catMaybes [Just 1, Just 2, Nothing]`  
`[1,2]`

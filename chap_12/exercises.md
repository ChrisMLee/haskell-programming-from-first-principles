# Chapter 12 Exercises

Type constructors (that is, higher-kinded types) are types that take more types as arguments

A lifted type, which includes any datatype you could define yourself, is any that can be inhabited by bottom. Lifted types are represented by a pointer and include most of the datatypes we’ve seen and most that you’re likely to encounter and use.

Data constructors really are functions. As it happens, they behave just like Haskell functions in that they are curried as well.

### 12.5 Chapter Exercises
1. Given `id :: a -> a`, What is the kind of a?
  -  `* `
2. `r :: a -> f a`. What are the kinds of a and f?
  - a - `*`. f - `* -> *`

( See chapter_exercises.hs for rest of solutions )

### Good Resources
[Why You Should Learn Haskell](https://www.reddit.com/r/haskell/comments/31tsru/why_should_i_learn_haskell/?st=ixl68qd6&sh=debe6220)
[John Carmack: Thoughts on Haskell](http://functionaltalks.org/2013/08/26/john-carmack-thoughts-on-haskell/)
[Eric Raymond](http://esr.ibiblio.org/?p=1796)
[Lessons Learning Haskell](http://dubhrosa.blogspot.co.uk/2012/12/lessons-learning-haskell.html)

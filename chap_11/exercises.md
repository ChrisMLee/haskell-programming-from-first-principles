# Chapter 11 Exercises

### Exercises: Cardinality

1. 1
2. 3
3. 

### Exercises: For Example
1. 
`` `
  Prelude> :t MakeExample
  MakeExample :: Example

  Prelude> :t Example
  <interactive>:1:1: error: Data constructor not in scope: Example
```
2.  
```
  :info Example
  data Example = MakeExample  -- Defined at <interactive>:13:1
  instance [safe] Show Example -- Defined at <interactive>:13:37
```
3. 
```
  MakeExample :: Int -> Example
```

`newtype` a way to define a type that will only ever have a single unary data constructor  
a `newtype` cannot be a product type, sum type, or have nullary constructors
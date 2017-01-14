### Chapter 13
We use `do` inside functions that return IO in order to sequence side effects in a convenient syntax.

### Chapter Exercises
**Modifying your code**
1. Ciphers: Open your Ciphers module and modify it so that the Caesar and Vigenère ciphers work with user input.

```
main :: IO ()
main = do
  putStr "input caesar shift: "
  shift <- getLine
  putStr "input text: "
  str <- getLine
  putStrLn $ "encrypted text: " ++ runCipher str (read shift :: Int)
```

see [vigenere_updated.hs](./vigenere_updated.hs)

2. see [palindrome.hs](./palindrome.hs)
3. see [palindrome.hs](./palindrome.hs)
4. see [gimmePerson.hs](./gimmePerson.hs)

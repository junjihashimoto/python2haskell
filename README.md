# python2haskell

Transform Python code to Haskell

language-python-package creates python-ast, and template-haskell generates haskell-code.

# Usage

When you pass a python code as an argument, haskell code will be output as follows.

```
$ cat > fib.py
def fib(n):
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fib(n-1) + fib(n-2)
 
print(fib(9))
print(fib(8))
$ cabal run python2haskell fib.py 
Up to date
fib n = if n == 0
         then 0
         else if n == 1
               then 1
               else fib (n - 1) + fib (n - 2)
main = do {print (fib 9); print (fib 8)}
```

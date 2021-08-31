def addf(a:float, b:float) -> float:
    return (a + b)

def add(a:int, b:int) -> int:
    return (a + b)

def fib(n:int) -> int:
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fib(n-1) + fib(n-2)
 
print(fib(9))
print(fib(8))

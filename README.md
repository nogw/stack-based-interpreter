# stack-based-interpreter

## Reference:
### * [forth](https://www.forth.com/starting-forth/2-stack-manipulation-operators-arithmetic/)

## How to run

```
ghci app/main.hs
```

## Some examples

```haskell
> [Push 1, Push 2, Op Add]
[3.0]
```

```haskell
> evaluate [Push 1, Push 2, Over, Swap, Dup]
[0.1, 0.1, 0.3, 0.3]
```

```haskell
> [Push 0, Push 2, Push 3, Rot, Push 4, Rot, Swap, Drop, Swap, Rot, Push 1]
[1.0, 2.0, 3.0, 4.0]
```

```haskell
> evaluate [Push 2, Push 4, Op Minus, Push 6, Swap, Push 8, Op Minus]
[1.0, 1.0]
```


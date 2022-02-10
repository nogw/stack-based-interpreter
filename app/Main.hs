{-# LANGUAGE LambdaCase #-}

module Main where

errorMsgSmallStack :: String -> String
errorMsgSmallStack msg = msg ++ ": stack needs a minimum of two values"

data Operation = Add | Minus | Divide | Multiply deriving (Show)

type StackValue = Double

type Stack = [StackValue]

data StackInstructs
  = Push StackValue
  | Op Operation
  | Dup
  | Swap
  | Drop
  | Over
  | Rot
  deriving (Show)

stackTest :: [StackInstructs]
stackTest = [Push 1, Push 2, Push 2, Op Add, Op Minus]

-- in this case I could do case pattern inside the evaluation function,
-- but I preferred to do it here because I wanted error messages with operation type
-- maybe I could do something like Type -> String and print a formatted error, but that's good enough

evaluateOp :: Operation -> Stack -> Stack
evaluateOp = \case
  Add -> \case
    a : b : rest -> a + b : rest
    _ -> error (errorMsgSmallStack "Add")
  Minus -> \case
    a : b : rest -> a + b : rest
    _ -> error (errorMsgSmallStack "Minus")
  Multiply -> \case
    a : b : rest -> a + b : rest
    _ -> error (errorMsgSmallStack "Multiply")
  Divide -> \case
    a : b : rest -> a + b : rest
    _ -> error (errorMsgSmallStack "Divide")

evaluate :: [StackInstructs] -> Stack
evaluate stack = aux stack []
  where
    aux (Push x : rest) acc = aux rest (x : acc)
    aux (Op x : rest) acc = aux rest (evaluateOp x acc)
    aux (Swap : rest) acc =
      aux
        rest
        ( case acc of
            a : b : rest -> b : a : rest
            _ -> error (errorMsgSmallStack "Swap")
        )
    aux _ acc = acc

main :: IO ()
main = print stackTest
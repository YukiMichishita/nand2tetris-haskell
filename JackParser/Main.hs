module Main where

import Class
import Expression
import Parser
import TestInput
import Tokens

main :: IO ()
main = do
  -- Test simple tokens first
  putStrLn "Testing keyword 'class':"
  print $ parse (Parser.symbol "class") "class"

  putStrLn "\nTesting identifier:"
  print $ parse identifier "Main"

  putStrLn "\nTesting symbol '{':"
  print $ parse (Parser.symbol "{") "{"

  putStrLn "\nTesting simple class structure:"
  print $ parse class' "class Main {}"

  putStrLn "\nTesting class with function:"
  print $ parse class' "class Main { function void main() { return; } }"

  putStrLn "\nTesting class with var declaration:"
  print $ parse class' "class Main { function void main() { var int x; return; } }"

  putStrLn "\nTesting with Array type:"
  print $ parse class' "class Main { function void main() { var Array a; return; } }"

  putStrLn "\nTesting with multiple vars on one line:"
  print $ parse class' "class Main { function void main() { var int i, sum; return; } }"

  putStrLn "\nTesting comment parser directly:"
  print $ parse comment "// comment\n"

  putStrLn "\nTesting symbol after comment:"
  print $ parse (Parser.symbol "class") "// comment\nclass"

  putStrLn "\nTesting with comment:"
  let withComment = "// comment\nclass Main {}"
  print $ parse class' withComment

  putStrLn "\nTesting block comment:"
  let blockComment = "/** block */\nclass Main {}"
  print $ parse class' blockComment

  putStrLn "\nTesting multiple line comments:"
  let multiComment = "// line1\n// line2\nclass Main {}"
  print $ parse class' multiComment

  putStrLn "\nTesting with leading newline:"
  let leadingNewline = "\n// comment\nclass Main {}"
  print $ parse class' leadingNewline

  putStrLn "\nFirst 10 chars of testInput:"
  print $ take 10 testInput

  putStrLn "\nTesting simplified version of testInput:"
  let simplified = "\n// comment\n/** block comment */\nclass Main { function void main() { return; } }"
  print $ parse class' simplified

  putStrLn "\nTesting with let statement:"
  let withLet = "class Main { function void main() { var int x; let x = 5; return; } }"
  case parse class' withLet of
    [] -> putStrLn "Failed!"
    [result] -> putStrLn "Success!"

  putStrLn "\nTesting string constant:"
  print $ parse stringConstant "\"test\""

  putStrLn "\nTesting method call (simple):"
  let simpleCall = "class Main { function void main() { do Output.printInt(5); return; } }"
  case parse class' simpleCall of
    [] -> putStrLn "Failed!"
    [result] -> putStrLn "Success!"

  putStrLn "\nTesting method call  expression:"
  print $ parse expression "Keyboard.readInt(\"test\")"

  putStrLn "\nTesting method call with string:"
  let withCall = "class Main { function void main() { var int x; let x = Keyboard.readInt(\"test\"); return; } }"
  case parse class' withCall of
    [] -> putStrLn "Failed!"
    [(c, remaining)] -> do
      putStrLn $ "Partial success, remaining: " ++ take 50 remaining
    [result] -> putStrLn "Success!"

  putStrLn "\nTesting full input:"
  let result = parse class' testInput
  case result of
    [] -> do
      putStrLn "Parse failed: no results"
    [(cls, "")] -> do
      putStrLn "Parse succeeded!"
      print cls
    [(cls, remaining)] -> do
      putStrLn "Parse partially succeeded!"
      putStrLn $ "Parsed class: " ++ show (className cls)
      putStrLn $ "Remaining input (" ++ show (length remaining) ++ " chars):"
      putStrLn $ take 200 remaining
    multiple -> do
      putStrLn $ "Multiple parse results (" ++ show (length multiple) ++ "):"
      mapM_ (\(c, r) -> putStrLn $ "Class: " ++ show (className c) ++ ", remaining: " ++ show (take 50 r)) multiple

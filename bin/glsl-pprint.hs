module Main where

import System.Environment (getArgs)
import Text.PrettyPrint.HughesPJClass

import Language.GLSL
import Language.GLSL.Tests (parsePrettyId, tests)

info :: [String]
info = 
  [ "This is glsl-pprint."
  ]

usage :: [String]
usage = info ++
  ["usage:\n  glsl-pprint [-p] filename\n  glsl-pprint --run-tests"]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--run-tests"] -> tests
    [fn] -> do
      content <- readFile fn
      putStrLn . show . parse $ content
    ["--check", fn] -> do
      content <- readFile fn
      case parse content of
        Left err -> putStrLn $ "parse error:\n" ++ show err
        Right ast ->
          if parsePrettyId ast
            then putStrLn "parse . pretty == id"
            else putStrLn "parse . pretty /= id"
    ["-p", fn] -> do
      content <- readFile fn
      case parse content of
        Left err -> putStrLn $ "parse error:\n" ++ show err
        Right ast -> print . pPrint $ ast
    _ -> putStrLn $ unlines usage



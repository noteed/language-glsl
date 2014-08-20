module Main where

import System.Environment (getArgs)
import Text.PrettyPrint.HughesPJClass

import Language.GLSL

info :: [String]
info = 
  [ "This is glsl-pprint."
  ]

usage :: [String]
usage = info ++
  ["usage:\n  glsl-pprint [-p] filename"]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> do
      content <- readFile fn
      putStrLn . show . parse $ content
    ["-p", fn] -> do
      content <- readFile fn
      case parse content of
        Left err -> putStrLn $ "parse error:\n" ++ show err
        Right ast -> print . pPrint $ ast
    _ -> putStrLn $ unlines usage



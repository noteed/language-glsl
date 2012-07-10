module Main where

import System.Environment (getArgs)
import Text.PrettyPrint.HughesPJClass

import Language.GLSL.Syntax
import Language.GLSL.Parser
import Language.GLSL.Compact

info :: [String]
info = 
  [ "This is glsl-compact."
  ]

usage :: [String]
usage = info ++
  ["usage:\n  glsl-compact [-p] filename"]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> do
      content <- readFile fn
      case parse content of
        Left err -> putStrLn $ "parse error:\n" ++ show err
        Right ast -> print . pPrint $ ast
    _ -> putStrLn $ unlines usage



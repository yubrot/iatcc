module Main where

import Data.Text.IO as T (getContents, putStrLn)
import Iatcc.Compiler (exec)

main :: IO ()
main = T.getContents >>= exec >>= T.putStrLn

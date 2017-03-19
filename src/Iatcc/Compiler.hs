module Iatcc.Compiler where

import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess, readProcess)
import Paths_iatcc (getDataFileName)

import Iatcc.AST
import Iatcc.Emitter (emitToText)
import qualified Iatcc.Parser as Parser

exec :: Text -> IO Text
exec src = case Parser.parse Parser.program "exectmp" src of
  Right program -> execExpr program
  Left e -> throwIO e

execExpr :: Program -> IO Text
execExpr program = withSystemTempFilePath "exec" $ \tmp -> do
  compile tmp program
  fromString <$> readProcess tmp [] []

compile :: String -> Program -> IO ()
compile dest program = withSystemTempFilePath "iatcc.s" $ \tmp -> do
  writeFile tmp (emitToText program)
  sys <- getDataFileName "rt/sys.c"
  rt <- getDataFileName "rt/rt.c"
  callProcess "gcc" ["-Wall", "-o", dest, sys, rt, tmp]

withSystemTempFilePath :: String -> (FilePath -> IO a) -> IO a
withSystemTempFilePath temp f = withSystemTempFile temp $ \tmp h -> hClose h >> f tmp

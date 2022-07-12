{-# LANGUAGE BlockArguments #-}
module Main where

import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)
import FileUtilities (predicateFind)
import ArgUtilities 
import System.FilePath (takeExtension)
import Control.Monad (join, forM, liftM, sequence, forM_)
import Data.List
import ArgUtilities (fromArgumentsToIgnoreFilter, listToArgument)


test = [".hs", "--ignore", "debug"]

main :: IO ()
main = do
    cwd <- getCurrentDirectory
    args <- getArgs 
    let argADT = listToArgument args
    let fil = fromArgumentsToIgnoreFilter argADT
    let ext = fromArgumentsToExt argADT
    forM_ ext (printLines cwd fil)
    where
        printLines cwd fil ext = eval cwd fil ext >>= \x -> putStrLn (ext <> " Files are: " <> show x <> " lines")

eval path fil ext = do
    files <- predicateFind (\p -> takeExtension p == ext) path fil
    let ln = map ((length . lines <$>) . readFile) files
    sum <$> sequence ln


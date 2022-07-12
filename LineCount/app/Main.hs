{-# LANGUAGE BlockArguments #-}
module Main where

import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)
import FileUtilities (predicateFind)
import System.FilePath (takeExtension)
import Control.Monad (join, forM, liftM, sequence, forM_)
import Data.List

main :: IO ()
main = do
    cwd <- getCurrentDirectory
    args <- getArgs
   
    forM_ args (printLines cwd) 
    where
        printLines cwd ext = eval cwd ext >>= \x -> putStrLn (ext <> " Files is: " <> show x <> " lines")

eval path ext = do
    files <- predicateFind (\p -> takeExtension p == ext) path
    let ln = map ((length . lines <$>) . readFile) files
    sum <$> sequence ln



{-# LANGUAGE BlockArguments #-}

module FileUtilities
(
    getRecursivePaths,
    predicateFind
)
where

import System.Directory
import System.FilePath
import Control.Monad
import Data.Char (toLower)
import Data.Monoid

getRecursivePaths :: FilePath -> [String] -> IO [FilePath]
getRecursivePaths topPath fil = do
    names' <- listDirectory topPath
    let names = filter (not . genFunc fil) names'
    paths <- forM names $ \name -> do
        let path = topPath </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursivePaths path fil
            else return [path]
    return (concat paths)

predicateFind :: (FilePath -> Bool) -> FilePath -> [String] -> IO [FilePath]
predicateFind p path fil = do
    names <- getRecursivePaths path fil
    return $ filter p names



genFunc :: [String] -> String -> Bool
genFunc ls = getAny . foldMap (Any .) (gen' ls)
    where
        gen' list = map (\x -> (==tL x)) list
        tL = map toLower


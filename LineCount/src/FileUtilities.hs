module FileUtilities
( 
    getRecursivePaths,
    predicateFind
) 
where

import System.Directory
import System.FilePath
import Control.Monad


getRecursivePaths :: FilePath -> IO [FilePath]
getRecursivePaths topPath = do
    names <- listDirectory topPath
    paths <- forM names $ \name -> do
        let path = topPath </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory 
            then getRecursivePaths path
            else return [path]
    return (concat paths)

predicateFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
predicateFind p path = do
    names <- getRecursivePaths path
    return $ filter p names
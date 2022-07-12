module ArgUtilities
(
    Argument(..),
    listToArgument,
    fromArgumentsToExt,
    fromArgumentsToIgnoreFilter
)
where

data Argument = Ext String | Command String String
    deriving (Show)

listToArgument :: [String] -> [Argument]
listToArgument (x@('.':ys):xs) = listToArgument xs ++ [Ext x]
listToArgument (x:y:xs) = listToArgument xs ++ [Command x y]
listToArgument (x:xs) = listToArgument xs
listToArgument [] = []

fromArgumentsToExt :: [Argument] -> [String]
fromArgumentsToExt ((Ext x):xs) = fromArgumentsToExt xs ++ [x]
fromArgumentsToExt (_:xs) = fromArgumentsToExt xs
fromArgumentsToExt [] = []

fromArgumentsToIgnoreFilter :: [Argument] -> [String]
fromArgumentsToIgnoreFilter ((Command "--ignore" x):xs) = fromArgumentsToIgnoreFilter xs ++ [x]
fromArgumentsToIgnoreFilter (_:xs) = fromArgumentsToIgnoreFilter xs
fromArgumentsToIgnoreFilter [] = []
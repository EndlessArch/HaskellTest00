module Lib
    ( myZip, someFunc
    ) where

import GHC.Base

myZip :: [a] -> [b] -> [(a, b)]
myZip (a:as) (b:bs) = (a, b) : myZip as bs

myZip _ [] = []
myZip [] _ = []

myLength :: [a] -> Int
myLength (a:as) = 1 + (myLength as)
myLength _ = 0

showTup :: (Show a, Show b) => (a, b) -> String
showTup (a, b) = "(" ++ show a ++ ", " ++ show b ++ ")"

printTupList :: (Show a, Show b) => [(a, b)] -> IO ()
printTupList (t:ts) = do
    putStr ( showTup t ++ ", " )
    printTupListItl ts

printTupListItl :: (Show a, Show b) => [(a, b)] -> IO ()
printTupListItl (x:[]) = putStrLn ( showTup x )
printTupListItl (x:xs) = do
    putStr (showTup x ++ ", ")
    printTupListItl xs

someFunc :: IO ()
someFunc =
    let const'String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
    printTupList ( myZip const'String [1..(myLength const'String)] )
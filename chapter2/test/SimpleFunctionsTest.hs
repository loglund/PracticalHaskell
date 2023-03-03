module SimpleFunctionsTest where

import Practical.Haskell.SimpleFunctions

import Test.Tasty
import Test.Tasty.Discover
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unit_firstOrEmptyNonEmpty :: IO ()
unit_firstOrEmptyNonEmpty = (firstOrEmpty ["hello", "world"]) @?= "hello"

unit_firstOrEmptyEmpty :: IO ()
unit_firstOrEmptyEmpty = (firstOrEmpty []) @?= "empty"

unit_ConcatEmptyEmpty :: IO ()
unit_ConcatEmptyEmpty = (([] :: [Int]) +++ ([] :: [Int])) @?= ([] :: [Int])

unit_ConcatLeftEmpty :: IO ()
unit_ConcatLeftEmpty = ([] +++ [4, 5, 6]) @?= [4, 5, 6]

unit_ConcatRightEmpty :: IO ()
unit_ConcatRightEmpty = ([1, 2, 3] +++ []) @?= [1, 2, 3]

unit_ConcatNonEmpty :: IO ()
unit_ConcatNonEmpty = ([1, 2, 3] +++ [4, 5, 6]) @?= [1, 2, 3, 4, 5, 6]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [
                                      testCase "firstOrEmpty ['hello', 'world'] = 'hello'" unit_firstOrEmptyNonEmpty,
                                      testCase "firstOrEmpty [] = 'empty'" unit_firstOrEmptyEmpty,
                                      testCase "[] +++ [] = []" unit_ConcatEmptyEmpty,
                                      testCase "[] +++ [4, 5, 6] = [4, 5, 6]" unit_ConcatLeftEmpty,
                                      testCase "[1, 2, 3] +++ [] = [1, 2, 3]" unit_ConcatRightEmpty,
                                      testCase "[1, 2, 3] +++ [4, 5, 6] = [1, 2, 3, 4, 5, 6]" unit_ConcatNonEmpty
                                      ]


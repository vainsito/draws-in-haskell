module Main (main) where
import Pred
import Dibujo (figura,  (.-.))
import Test.HUnit

testPred :: Test
testPred = TestList [
    -- cambiar
    "Test cambiar1" ~: cambiar (== 1) (const (figura 2)) (figura 1) ~?= figura 2,
    "Test cambiar2" ~: cambiar (== 1) (const (figura 2)) (figura 2) ~?= figura 2,
    "Test cambiar3" ~: cambiar (== 1) (const (figura 2)) (figura 1 .-. figura 1) ~?= figura 2 .-. figura 2,
    -- anyDib
    "Test anyDib1" ~: anyDib (== 1) (figura 1) ~?= True,
    "Test anyDib2" ~: anyDib (== 2) (figura 1) ~?= False,
    "Tset anyDib3" ~: anyDib (== 1) (figura 1 .-. figura 1) ~?= True,
    -- allDib
    "Test allDib1" ~: allDib (== 1) (figura 1) ~?= True,
    "Test allDib2" ~: allDib (== 2) (figura 1) ~?= False,
    "Test allDib3" ~: allDib (== 1) (figura 1 .-. figura 1) ~?= True,
    -- andP
    "Test andP1" ~: andP (> 0) (< 2) 1 ~?= True,
    "Test andP2" ~: andP (> 0) (< 2) 2 ~?= False,
    "Test andP3" ~: andP (> 0) (< 2) 0 ~?= False,
    -- orP
    "Test orP 1" ~: orP (> 0) (> 2) 1 ~?= True,
    "Test orP 2" ~: orP (> 0) (> 2) 2 ~?= True,
    "Test orP 3" ~: orP (> 0) (> 2) 0 ~?= False
    ]

main :: IO ()
main = do
    putStrLn "Running Test Suite for Pred."
    runTestTTAndExit testPred
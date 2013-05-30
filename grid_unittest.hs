import Test.HUnit
import Grid

main = runTestTT tests

test1 = TestCase $ assertEqual "first row" [0,4,1,5,2,6,3,7,7,7] (gridTriStripIndices 3 1)

test2 = TestCase $ assertEqual "first to second" [0,4,1,5,2,6,3,7,7,7,11,6,10,5,9,4,8,8] (gridTriStripIndices 3 2)

test3 = TestCase $ assertEqual "first to third" [0,4,1,5,2,6,3,7,7,7,11,6,10,5,9,4,8,8,8,12,9,13,10,14,11,15,15,15] (gridTriStripIndices 3 3)

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3]


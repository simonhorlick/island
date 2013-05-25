import HUnit

main = runTestTT tests

test1 = TestCase $ assertEqual "first row" (gridTriStripIndices 4 1) [0,4,1,5,2,6,3,7,7,7]

test2 = TestCase $ assertEqual "first to second" (gridTriStripIndices 4 2) [0,4,1,5,2,6,3,7,7,7,11,6,10,5,9,4,8,8,8]

test3 = TestCase $ assertEqual "first to third" (gridRowTriStripIndices 4 3) [0,4,1,5,2,6,3,7,7,7,11,6,10,5,9,4,8,8,8,12,9,13,10,14,11,15,15,15]

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3]


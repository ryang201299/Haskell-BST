import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Lib
import Data.List

-- Helper Function checks for duplicate entries in lists
hasDuplicates :: (Ord item) => [item] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

-- Quickcheck Tests
-- QuickCheck is a library of ints and strings, providing sample data for the tests

-- Property-based Insert Tests
prop_insertIntoEmptyTree :: Int -> String -> Bool
prop_insertIntoEmptyTree keyVal itemVal =
  contains (insertBST Leaf (keyVal, itemVal)) keyVal

prop_insertWithStringKey :: String -> String -> Bool
prop_insertWithStringKey keyVal itemVal = 
  contains (insertBST Leaf (keyVal, itemVal)) keyVal

prop_insertSameKeyTwice :: Int -> String -> Bool
prop_insertSameKeyTwice keyVal itemVal =
  length (inorder(recursiveInsertion Leaf [keyVal, keyVal] [itemVal, itemVal])) == 1

prop_insertTwoNodes :: Int -> String -> Bool
prop_insertTwoNodes keyVal itemVal =
  length (inorder(recursiveInsertion Leaf [keyVal, keyVal + 1] [itemVal, itemVal])) == 2

prop_insertToBothChildPaths :: Int -> String -> Bool
prop_insertToBothChildPaths keyVal itemVal =
  inorder (recursiveInsertion Leaf [keyVal, keyVal + 1, keyVal -1] [itemVal, itemVal, itemVal]) == [(keyVal - 1, itemVal), (keyVal, itemVal), (keyVal + 1, itemVal)]

prop_recursiveInsert :: [Int] -> Bool
prop_recursiveInsert [] = True 
prop_recursiveInsert keyList =
  if hasDuplicates keyList
    then True
    else length (inorder (recursiveInsertion Leaf keyList keyList)) == length keyList

-- Property-based Contains Tests
prop_containsFindsIntRoot :: Int -> Bool
prop_containsFindsIntRoot keyVal =
  contains (insertBST Leaf (keyVal, "")) keyVal

prop_containsFindsStringRoot :: String -> Bool
prop_containsFindsStringRoot keyVal = 
  contains (insertBST Leaf (keyVal, "")) keyVal

prop_containsFindsLeftChild :: Int -> Bool
prop_containsFindsLeftChild keyVal = 
  contains (recursiveInsertion Leaf [keyVal + 1, keyVal] ["", ""]) keyVal

prop_containsFindsRightChild :: Int -> Bool
prop_containsFindsRightChild keyVal = 
  contains (recursiveInsertion Leaf [keyVal - 1, keyVal] ["", ""]) keyVal

prop_containsFindsNodeAtEndOFTree :: [Int] -> Bool
prop_containsFindsNodeAtEndOFTree keyList = do
  contains (recursiveInsertion Leaf (keyList ++ [567]) (keyList ++ [567])) 567

-- Property-based In Order Tests
prop_inorderCorrectLength :: [Int] -> Bool
prop_inorderCorrectLength [] = True
prop_inorderCorrectLength keyList =
  if hasDuplicates keyList
    then True
    else length (inorder (recursiveInsertion Leaf keyList keyList)) == length keyList

prop_inorderCorrectFirstVal :: [Int] -> Bool
prop_inorderCorrectFirstVal keyList =
  if hasDuplicates (keyList ++ [-1000])
    then True
    else head (inorder (recursiveInsertion Leaf (keyList ++ [-1000]) (keyList ++ [-1000]))) == (-1000, -1000)

prop_inorderCorrectFinalVal :: [Int] -> Bool
prop_inorderCorrectFinalVal keyList =
  if hasDuplicates (keyList ++ [1000])
    then True
    else last (inorder (recursiveInsertion Leaf (keyList ++ [1000]) (keyList ++ [1000]))) == (1000, 1000)

-- Unit Tests

-- create contains test cases and tree below like inorder

-- Contains Unit Tests
unit_containsRoot :: Assertion
unit_containsRoot = 
  assertEqual "Contains Root Node" True (contains (insertBST Leaf (1, "")) 1)

unit_containsRootWithValue :: Assertion
unit_containsRootWithValue = 
  assertEqual "" True (contains (insertBST Leaf (1, 1)) 1)

unit_containsNodeWithString :: Assertion
unit_containsNodeWithString = 
  assertEqual "" True (contains (insertBST Leaf ("1", "")) "1")

unit_containsNodeWithValue :: Assertion
unit_containsNodeWithValue = 
  assertEqual "" True (contains (insertBST Leaf ("1", "One")) "1")

unit_containsNotInTree :: Assertion
unit_containsNotInTree = 
  assertEqual "" False (contains (insertBST Leaf (1, 1)) 2)

unit_containsWithChildren :: Assertion
unit_containsWithChildren = do
  let testTree = recursiveInsertion Leaf [3, 4, 5] ["Three", "Four", "Five"]
  assertEqual "" True (contains testTree 5)


-- Insert Unit Tests
unit_insertIntegerKey :: Assertion
unit_insertIntegerKey = 
  assertEqual "Insert root node" True (contains (insertBST Leaf (1, "")) 1)

unit_insertIntegerItem :: Assertion
unit_insertIntegerItem = 
  assertEqual "" True (contains (insertBST Leaf (1, 1)) 1)

unit_insertStringKey :: Assertion
unit_insertStringKey = 
  assertEqual "" True (contains (insertBST Leaf ("1", "")) "1")

unit_insertStringItem :: Assertion
unit_insertStringItem = 
  assertEqual "" True (contains (insertBST Leaf ("1", "One")) "1")

unit_insertLeftChild :: Assertion
unit_insertLeftChild = do
  let testTree = recursiveInsertion Leaf [3, 2] ["Three", "Two"]
  assertEqual "" 2 (length (inorder (testTree)))

unit_insertRightChild :: Assertion
unit_insertRightChild = do
  let testTree = recursiveInsertion Leaf [3, 4] ["Three", "Four"]
  assertEqual "" 2 (length (inorder (testTree)))

unit_TwoNodesSameKey :: Assertion
unit_TwoNodesSameKey = do
  let testTree = recursiveInsertion Leaf [5, 5] ["Five", "Five"]
  assertEqual "" 1 (length (inorder (testTree)))

-- Delete Unit Tests
unit_deleteRoot :: Assertion
unit_deleteRoot = do
  let testTree = (insertBST Leaf (3, "Three"))
  assertEqual "" [] (inorder (deleteHelper testTree 3))

unit_deleteLeftChildWithNoChildren :: Assertion
unit_deleteLeftChildWithNoChildren = do
  let testTree = recursiveInsertion Leaf [3, 2] ["Three", "Two"]
  assertEqual "" [(3,"Three")] (inorder (deleteHelper testTree 2))

unit_deleteRightChildWithNoChidlren :: Assertion
unit_deleteRightChildWithNoChidlren = do
  let testTree = recursiveInsertion Leaf [3, 5] ["Three", "Five"]
  assertEqual "" [(3,"Three")] (inorder (deleteHelper testTree 5))

unit_deleteLeftChildWithLeftChild :: Assertion
unit_deleteLeftChildWithLeftChild = do
  let testTree = recursiveInsertion Leaf [3, 2, 1] ["Three", "Two", "One"]
  assertEqual "" [(1,"One"),(3,"Three")] (inorder (deleteHelper testTree 2))

unit_deleteLeftChildWithRightChild :: Assertion
unit_deleteLeftChildWithRightChild = do
  let testTree = recursiveInsertion Leaf [4, 2, 3] ["Four", "Two", "Three"]
  assertEqual "" [(3,"Three"),(4,"Four")] (inorder (deleteHelper testTree 2))

unit_deleteRightChildWithLeftChild :: Assertion
unit_deleteRightChildWithLeftChild = do
  let testTree = recursiveInsertion Leaf [3, 5, 4] ["Three", "Five", "Four"]
  assertEqual "" [(3,"Three"),(4,"Four")] (inorder (deleteHelper testTree 5))

unit_deleteRightChildWithRightChild :: Assertion
unit_deleteRightChildWithRightChild = do
  let testTree = recursiveInsertion Leaf [3, 4, 5] ["Three", "Four", "Five"]
  assertEqual "" [(3,"Three"),(5,"Five")] (inorder (deleteHelper testTree 4))

unit_deleteTwoNodes :: Assertion
unit_deleteTwoNodes = do
  let testTree = recursiveInsertion Leaf [3, 4, 5] ["Three", "Four", "Five"]
  assertEqual "" [(3,"Three")] (inorder (deleteHelper (deleteHelper testTree 4) 5))


-- Inorder unit tests

unit_inorderTraversalRootNode :: Assertion
unit_inorderTraversalRootNode = do
  let testTree = insertBST Leaf (3, "Three")
  assertEqual "" [(3,"Three")] (inorder testTree)

unit_inorderLeftChild :: Assertion
unit_inorderLeftChild = do
  let testTree = recursiveInsertion Leaf [3, 2] ["Three", "Two"]
  assertEqual "" [(2,"Two"),(3,"Three")] (inorder testTree)

unit_inorderRightChild :: Assertion
unit_inorderRightChild = do
  let testTree = recursiveInsertion Leaf [3, 4] ["Three", "Four"]
  assertEqual "" [(3,"Three"),(4,"Four")] (inorder testTree)

unit_inorderLeftAndRightChild :: Assertion
unit_inorderLeftAndRightChild = do
  let testTree = recursiveInsertion Leaf [3, 4, 2] ["Three", "Four", "Two"]
  assertEqual "" [(2,"Two"),(3,"Three"),(4,"Four")] (inorder testTree)

unit_inorderLeftChildren :: Assertion
unit_inorderLeftChildren = do
  let testTree = recursiveInsertion Leaf [4, 3, 2, 1] ["Four", "Three", "Two", "One"]
  assertEqual "" [(1,"One"),(2,"Two"),(3,"Three"),(4,"Four")] (inorder testTree)

unit_inorderRightChildren :: Assertion
unit_inorderRightChildren = do
  let testTree = recursiveInsertion Leaf [1, 2, 3, 4] ["One", "Two", "Three", "Four"]
  assertEqual "" [(1,"One"),(2,"Two"),(3,"Three"),(4,"Four")] (inorder testTree)

-- Test Suites

-- All Tests
all_tests :: TestTree
all_tests = testGroup "All Tests"
 [ 
   prop_insertion_tests,
   prop_inorder_tests,
   prop_contains_tests,

   unit_insertion_tests,
   unit_inorder_tests,
   unit_contains_tests,
   unit_deleting_tests
 ]

-- Property-based Insertion Test Group
prop_insertion_tests :: TestTree
prop_insertion_tests = testGroup "Insertion Property-based Tests"
 [ 
  testProperty "Can insert into the root" prop_insertIntoEmptyTree,
  testProperty "Can insert string Keys" prop_insertIntoEmptyTree,
  testProperty "Insert two items with same key" prop_insertSameKeyTwice,
  testProperty "Insert onto left and right child of a node" prop_insertToBothChildPaths,
  testProperty "Recursive insert many items to the tree" prop_recursiveInsert
 ]

-- Insertion Unit Test Group
unit_insertion_tests :: TestTree
unit_insertion_tests = testGroup "Insertion Unit Tests"
 [
   testCase "Can insert integer keys" unit_insertIntegerKey,
   testCase "Can insert integer items" unit_insertIntegerKey,
   testCase "Can insert string keys" unit_insertIntegerKey,
   testCase "Can insert string items" unit_insertIntegerKey,
   testCase "Can insert into left child" unit_insertLeftChild,
   testCase "Can insert into right child" unit_insertRightChild,
   testCase "Cannot insert two nodes with the same key" unit_TwoNodesSameKey

 ]

-- Deletion Unit Test Group
unit_deleting_tests :: TestTree
unit_deleting_tests = testGroup "Delete Unit Tests"
 [
  testCase "Deleting the root of a tree" unit_deleteRoot,
  testCase "Deleting the left child with no children" unit_deleteLeftChildWithNoChildren,
  testCase "Deleting the right child with no children" unit_deleteRightChildWithNoChidlren,
  testCase "Deleting left child that has a left child" unit_deleteLeftChildWithLeftChild,
  testCase "Deleting left child that has a right child" unit_deleteLeftChildWithRightChild,
  testCase "Deleting right child that has a left child" unit_deleteRightChildWithLeftChild,
  testCase "Deleting right child that has a right child" unit_deleteRightChildWithRightChild,
  testCase "Deleting two children nodes" unit_deleteTwoNodes
 ]

 -- Inorder Unit Test Group
unit_inorder_tests :: TestTree
unit_inorder_tests = testGroup "Inorder Unit Tests"
 [
  testCase "Inorder traversal with root node" unit_inorderTraversalRootNode,
  testCase "Inorder traversal with left child" unit_inorderLeftChild,
  testCase "Inorder traversal with right child" unit_inorderRightChild,
  testCase "Inorder traversal with left and right child" unit_inorderLeftAndRightChild,
  testCase "Inorder traversal with left children" unit_inorderLeftChildren,
  testCase "Inorder traversal with right children" unit_inorderRightChildren
 ]

prop_inorder_tests :: TestTree
prop_inorder_tests = testGroup "Inorder Property-based Tests"
 [
   testProperty "Inorder contains all values in BST" prop_inorderCorrectLength,
   testProperty "Inorder contains correct first value" prop_inorderCorrectFirstVal,
   testProperty "Inorder contains correct final value" prop_inorderCorrectFinalVal
 ]

  -- Contains Unit Test Group
unit_contains_tests :: TestTree
unit_contains_tests = testGroup "Contains Unit Tests"
 [
  testCase "BST contains root" unit_containsRoot,
  testCase "BST contains root with value" unit_containsRootWithValue,
  testCase "BST contains node with string key" unit_containsNodeWithString,
  testCase "BST contains node with value" unit_containsNodeWithValue,
  testCase "BST does not contain a value" unit_containsNotInTree,
  testCase "BST contains node with child" unit_containsWithChildren
 ]

prop_contains_tests :: TestTree
prop_contains_tests = testGroup "Contains Property-based Tests"
 [
   testProperty "BST contains integer root" prop_containsFindsIntRoot,
   testProperty "BST contains string root" prop_containsFindsStringRoot,
   testProperty "BST contains left child" prop_containsFindsLeftChild,
   testProperty "BST contains right child" prop_containsFindsRightChild,
   testProperty "BST contains a deep node" prop_containsFindsNodeAtEndOFTree
 ]

-- Main
main :: IO ()
main = defaultMain all_tests

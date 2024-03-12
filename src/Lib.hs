module Lib where

titleFunc :: IO ()
titleFunc = putStrLn "Haskell Binary Search Tree"

-- Recursive insertion method is recursively inserting items and values into the tree
recursiveInsertion :: (Ord item) => BST (item, items) -> [item] -> [items] -> BST (item, items)
recursiveInsertion Leaf insertKeyList insertValList = 
    recursiveInsertion (insertBST Leaf (head insertKeyList,head insertValList)) (tail insertKeyList) (tail insertValList)
recursiveInsertion (Node t1 (treeKey, treeVal) t2) insertKeyList insertValList = 
    if length insertKeyList > 0 && length insertValList > 0
        then recursiveInsertion (insertBST (Node t1 (treeKey, treeVal) t2) (head insertKeyList, head insertValList)) (tail insertKeyList) (tail insertValList)
    else (Node t1 (treeKey, treeVal) t2)

-- Data type for BST Leafs and Nodes
data BST item = Leaf | Node (BST item) item (BST item) deriving Show

-- Lookup method to check if nodes exist by "soughtKey" in the BST
-- Input values are a BST type and a soughtKey. Output is a boolean true or false
contains :: (Ord item) => BST (item, items) -> (item) -> Bool
contains Leaf _ = False
contains (Node leftChild (treeKey, treeVal) rightChild) (soughtKey)
    | soughtKey == treeKey = True -- If soughtkey is found, return true
    | soughtKey < treeKey = contains leftChild (soughtKey) -- If soughtKey is less than treeKey, traverse leftChild
    | soughtKey > treeKey = contains rightChild (soughtKey) -- If soughtKey is greater than treeKey, traverse rightChild

-- Insert method to add new nodes to the BST
-- Input values are a BST type and a pair. Output is a new BST containing new node
insertBST :: (Ord item) => BST (item, items) -> (item, items) -> BST (item, items)
insertBST Leaf (insertKey, insertVal) = Node Leaf (insertKey, insertVal) Leaf
insertBST (Node leftChild (treeKey, treeVal) rightChild) (insertKey, insertVal)
    | treeKey == insertKey = Node leftChild (treeKey, treeVal) rightChild -- If duplicate is found, return original BST
    | treeKey < insertKey = Node leftChild (treeKey, treeVal) (insertBST rightChild (insertKey, insertVal)) -- Traverse leftchild
    | treeKey > insertKey = Node (insertBST leftChild (insertKey, insertVal)) (treeKey, treeVal) rightChild -- Traverse rightChild

-- Inorder method for outputting an in-order traversal of the BST
-- Input value is a BST type. Output is an array of pairs
inorder :: (Ord item) => BST (item, items) -> [(item, items)]
inorder Leaf = [] -- If BST contains no nodes, return empty array
inorder (Node leftChild (treeKey, treeVal) rightChild) = inorder leftChild ++ [(treeKey, treeVal)] ++ inorder rightChild -- Output inorder arrray of pairs

-- Helper removal method for traversing the BST, finding the sought key
-- Input values are a BST and a soughtKey. Output is a new BST without the sought node
deleteHelper :: (Ord item) => BST (item, items) -> item -> BST (item, items)
deleteHelper Leaf _ = Leaf -- If node does not exist, return Leaf
deleteHelper (Node leftChild (treeKey, treeVal) rightChild) soughtKey
    | soughtKey == treeKey = deleteNode (Node leftChild (treeKey, treeVal) rightChild) -- If found soughtKey, call delete method
    | soughtKey < treeKey = Node (deleteHelper leftChild soughtKey) (treeKey, treeVal) rightChild -- Traverse left child
    | soughtKey > treeKey = Node leftChild (treeKey, treeVal) (deleteHelper rightChild soughtKey) -- Traverse right child

-- Removal method to remove sought key
-- Input value is a BST type. Output is a BST type
deleteNode :: (Ord item) => BST (item, items) -> BST (item, items)
deleteNode (Node Leaf (treeKey, treeVal) rightChild) = rightChild -- If node has a right child, replace parent with child
deleteNode (Node leftChild (treeKey, treeVal) Leaf) = leftChild -- If node has a left child, replace parent with child

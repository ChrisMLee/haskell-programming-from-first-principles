module BinaryToList where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

 --preorder b = preorder b [] where
 -- preorder Leaf acc = acc
 -- preorder (Node Leaf a Leaf) acc = a (++) (preorder Leaf acc) (++) (preorder Leaf acc)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left b right) = [b] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left b right) = (preorder left) ++ [b] ++ (preorder right) 

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left b right) = (preorder left) ++ (preorder right) ++ [b]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f z Leaf = z
foldTree f z (Node left y right) = f y (foldTree f (foldTree f z left) right)

-- standard foldr implementation
--foldr f z []     = z 
--foldr f z (x:xs) = f x (foldr f z xs) 


testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder
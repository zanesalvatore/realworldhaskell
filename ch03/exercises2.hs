import Data.List

-- 1. write a function that computes the number of elements in a list
--countList xs = countList 0 xs
--countList n [] = n
--countList n (_:xs) = countList (n + 1) xs

-- 2. add a type signature for your function
countList :: [a] -> Int
countList [] = 0
countList (_:xs) = 1 + countList xs

sumList [] = 0
sumList (x:xs) = x + sumList xs

-- 3. write a fn that computes the mean of a list
mean [] = 0
mean xs = (sumList xs) / fromIntegral (countList xs)

-- 4. turn a list into a palindrome
-- laziest possible solution
palindromify xs = xs ++ reverse xs

-- 5. write a fn that determines whether its input is a palindrome
is_palindrome xs
  | countList xs > 1  = if (head xs) == (last xs)
                        then is_palindrome ((tail . init) xs)
                        else False
  | countList xs <= 1 = True

-- 6. write a fn that sorts a list of lists based on the length of each sublist
sortByLengths :: [[a]] -> [[a]]
sortByLengths xs = let { cmpLists l r = compare (length l) (length r) }
                   in sortBy cmpLists xs

-- 7. join a list of lists together using a separator value
--  expected output:
--    ghci> intersperse ',' ["foo", "bar"]
--    "foo,bar"
--  inputs:
--    separator
--    list of lists
--  cases:
--    list of lists is empty
--    list of lists has one entry
--    list of lists has >1 entry
myIntersperse :: a -> [[a]] -> [a]
myIntersperse separator (x:xs)
  | (length xs) > 0 = x ++ separator : (myIntersperse separator xs)
  | otherwise = x


-- 8. find the height of a binary tree
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)


treeHeight (Node _ left right) = 1 + (max (treeHeight left) (treeHeight right))
treeHeight Empty = 0

-- 9
data Direction = DLeft | DRight | DStraight

-- 10
data Point = Point Double Double

angle (Point ax ay) (Point bx by) = let { y = by - ay;
                                          x = bx - ax }
                                    in atan $ y / x

direction :: Point -> Point -> Point -> Direction
direction p1 p2 p3
  | (angle p1 p2) < (angle p2 p3) = DLeft
  | (angle p1 p2) == (angle p2 p3) = DStraight
  | (angle p1 p2) > (angle p2 p3) = DRight
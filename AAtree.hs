--{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a
  get,           -- Ord a => a -> AATree a -> Maybe a
  insert,        -- Ord a => a -> AATree a -> AATree a
  inorder,       -- AATree a -> [a]
  remove,        -- Ord a => a -> AATree a -> AATree a
  size,          -- AATree a -> Int
  height,        -- AATree a -> Int
  checkTree      -- Ord a => AATree a -> Bool
 ) where

--------------------------------------------------------------------------------

-- AA search trees
data AATree a = Empty | Node (a,Int) (AATree a) (AATree a)
  deriving (Eq, Show, Read)


emptyTree :: AATree a
emptyTree = Empty 

get :: Ord a => a -> AATree a -> Maybe a
get _ Empty                     = Nothing
get val (Node ( n ,_ ) left right) 
    | val == n                  = Just val 
    | val <  n                  = get val left
    | val >  n                  = get val right

-- You may find it helpful to define
--   split :: AATree a -> AATree a
--   skew  :: AATree a -> AATree a
-- and call these from insert.
insert :: Ord a => a -> AATree a -> AATree a
insert = error "insert not implemented"

inorder :: AATree a -> [a]
inorder = error "inorder not implemented"

size :: AATree a -> Int
size = error "size not implemented"

height :: AATree a -> Int
height = error "height not implemented"

--------------------------------------------------------------------------------
-- Optional function

remove :: Ord a => a -> AATree a -> AATree a
remove = error "remove not implemented"

--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants

checkTree :: Ord a => AATree a -> Bool
checkTree root =
  isSorted (inorder root) &&
  all checkLevels (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x:nodes (leftSub x) ++ nodes (rightSub x)

-- True if the given list is ordered
isSorted :: Ord a => [a] -> Bool
isSorted []         = True
isSorted [x]        = True
isSorted (x:y:xs)
    | x <= y        = isSorted xs
    | x > y         = False 




-- Check if the invariant is true for a single AA node
-- You may want to write this as a conjunction e.g.
--   checkLevels node =
--     leftChildOK node &&
--     rightChildOK node &&
--     rightGrandchildOK node
-- where each conjunct checks one aspect of the invariant
checkLevels :: AATree a -> Bool
checkLevels Empty                       = True
checkLevels (Node (_,h) left right)     =   (leftChildOK h left)  && 
                                            (rightChildOK h right) &&
                                        rightChildOK h (rightSub right)
    
    where 

        leftChildOK _ Empty                           = True
        leftChildOK pHeight (Node (_,h) _ _ )         = (pHeight-1)   == h
        
        rightChildOK _  Empty                         = True
        rightChildOK pHeight (Node (_,h) left right) 
            | (isEmpty left && isEmpty right)         =  (pHeight -1) == h 
            | otherwise                               =   pHeight     == h

        rightGrandchildOK _ Empty                     = True
        rightGrandchildOK pHeight (Node (_,h) _ _ )   = (pHeight-1) == h

                        

        

isEmpty :: AATree a -> Bool
isEmpty Empty   = True
isEmpty _       = False 

leftSub :: AATree a -> AATree a
leftSub Empty = Empty
leftSub (Node _ left right) = left 

rightSub :: AATree a -> AATree a
rightSub Empty = Empty
rightSub (Node _ left right) = right 


--------------------------------------------------------------------------------

badtestTree   ::  Num a => a-> AATree a
badtestTree n        = Node (n,4) (left n)  (right n)

    where left  n = Node (n-2, 3) (Node (n-4, 2) Empty Empty) $Node (n-3, 1) Empty Empty
          right n = Node (n+2, 3) (Node (n+1, 2) Empty Empty) $Node (n+4, 1) Empty Empty


testAATree :: Num a => AATree a 
testAATree = Node (5,2)  (left)  (right)   
    where   left = Node (3,1) Empty Empty
            right = Node (4,2) (Node (2,1) Empty Empty) (Node (1,1) Empty Empty)

testAATree2 :: Num a => AATree a            
testAATree2  = Node (5,2) (Node (3,1) Empty Empty)(Node (4,1) Empty Empty)

    










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

split :: AATree a -> AATree a
split t = error "undef"

skew  :: AATree a -> AATree a
skew (Node (v,h) l r) = (Node (v'(l) ,h')  left' right' )
    where
        v' Empty                = error "cannot find empty tree."
        v' (Node (leftV,_) _ _) = leftV
        h'                      = getHeight l
        left'   = (leftSub l)
        right'  = (Node (v,h) (rightSub l) r)




insert :: Ord a => a -> AATree a -> AATree a
-- case insertion.
insert n Empty                      = (Node (n,1) Empty Empty)

-- could be fixed with a case expression
--insert n Empty Empty
insert n (Node (val,h) left right) 
    -- recursively finds the subtree to the left
    | n < val                   = 
        case left of Empty        -> skew (Node (val,h) (insert n Empty) right)
                     (Node _ _ _) -> (Node (val,h) (insert n left) right)
    -- recursively finds the subtree to the left
    | n > val                   = (Node (val,h) left (insert n right))
    -- returns the originalTree
    | n == val                  = (Node (val,h) right left)
    


inorder :: AATree a -> [a]
inorder Empty                      = []
inorder (Node (val,h) left right)  = inorder left ++ [val] ++ inorder right


size :: AATree a -> Int
size  Empty = 0
size (Node _ left right)  = 1 + size left + size right

height :: AATree a -> Int
height Empty = 0
height (Node  (_,h) _ _ ) = h



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

checkLevels ::Ord a => AATree a -> Bool
checkLevels (Node (val,h) left right) = leftChildOK left && rightChildOK right &&
                                        rightGrandchildOK right
    where
    leftChildOK Empty = isEmpty right
    leftChildOK (Node (_,h') _ _)  = (h-1) == h'

    rightChildOK Empty = isEmpty left  -- decided true if the left child is empty.
    rightChildOK (Node (_,h') left' right')
        | hasElems left' && hasElems right' = (h == h') -- where right has grandChilds
        | isEmpty left                      = False
        | otherwise                         = ((h-1) == h')

    rightGrandchildOK (Node _ gleft gright) -- dont have to look
        |  isEmpty gleft && isEmpty gleft = True
        |  hasElems left &&
           hasElems gleft &&
           hasElems gright                =  ((h-1) == getHeight gleft) && ((h-1) == getHeight gright)
        | otherwise                       = False



getHeight :: AATree a -> Int
getHeight Empty = 0
getHeight (Node (_,h) _ _)= h

hasElems :: AATree a -> Bool
hasElems Empty             = False
hasElems (Node _ _ _ )     = True


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




plsSkewMe :: Num a => AATree a
plsSkewMe = Node (10,2) leftSub rightSub
    where 
        leftSub   = (Node (5,2) (Node (1,1) Empty Empty) (Node (7,1) Empty Empty))
        rightSub  = (Node (11,1) Empty Empty)
    



emptyChildrenTree :: Num a => AATree a
emptyChildrenTree = Node (10,1) Empty Empty

leftChildEmptyTree :: Num a => AATree a
leftChildEmptyTree = Node (10,2) Empty (Node (12,1) Empty Empty )

rightChildEmptyTree :: Num a => AATree a
rightChildEmptyTree = Node (10,2) (Node (11,1) Empty Empty) Empty

noAAtestTree   ::  Num a => a-> AATree a
noAAtestTree n    = Node (n,4) (left)  (right)

    where left  = Node (n-2, 3) (Node (n-4, 2) Empty Empty) $Node (n-3, 1) Empty Empty
          right = Node (n+2, 3) (Node (n+1, 2) Empty Empty) $Node (n+4, 1) Empty Empty


testAATree :: Num a => AATree a
testAATree = Node (5,2)  (left)  (right)
    where   left = Node (3,1) Empty Empty
            right = Node (7,2) (Node (6,1) Empty Empty) (Node (10,1) Empty Empty)

testAATree2 :: Num a => AATree a
testAATree2  =                      Node (5,2)
                        (Node (3,1) Empty Empty) (Node (10,1) Empty Empty)

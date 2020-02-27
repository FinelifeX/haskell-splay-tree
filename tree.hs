data Tree a = None | Node a (Tree a) (Tree a) deriving (Eq, Show)

data Direction = L | R deriving (Eq, Show)

rotateL :: Ord a => Tree a -> Tree a -> Tree a
rotateL (Node p _ pr) (Node c cl cr) =
  Node c cl (Node p cr pr)

rotateR :: Ord a => Tree a -> Tree a -> Tree a
rotateR (Node p pl _) (Node c cl cr) =
  Node c (Node p pl cl) cr 

zigzigL :: Ord a => Tree a -> Tree a -> Tree a -> Tree a
zigzigL (Node g _ gr) (Node p _ pr) (Node c cl cr) =
  Node c cl (Node p cr (Node g pr gr))

zigzigR :: Ord a => Tree a -> Tree a -> Tree a -> Tree a
zigzigR (Node g gl _) (Node p pl _) (Node c cl cr) =
  Node c (Node p (Node g gl pl) cl) cr

zigzagLR :: Ord a => Tree a -> Tree a -> Tree a -> Tree a
zigzagLR (Node g _ gr) (Node p pl _) (Node c cl cr) =
  Node c (Node p pl cl) (Node g cr gr)

zigzagRL :: Ord a => Tree a -> Tree a -> Tree a -> Tree a
zigzagRL (Node g gl _) (Node p _ pr) (Node c cl cr) =
  Node c (Node g gl cl) (Node p cr pr)

splay :: Ord a => a -> Tree a -> Tree a
splay key tree = restructure $ path key tree [(undefined, tree)]
  where path key None ps = ps
        path key (Node k l r) ps = 
          case compare key k of
            EQ -> ps
            LT -> path key l ((L, l) : ps)
            GT -> path key r ((R, r) : ps)

        restructure :: Ord a => [(Direction, Tree a)] -> Tree a
        restructure ((_, t) : []) = t
        restructure ((L, c) : (_, p) : []) = rotateL p c
        restructure ((R, c) : (_, p) : []) = rotateR p c
        restructure ((L, c) : (L, p) : (direction, g) : ps) = 
          restructure $ (direction, zigzigL g p c) : ps
        restructure ((R, c) : (R, p) : (direction, g) : ps) =
          restructure $ (direction, zigzigR g p c) : ps
        restructure ((L, c) : (R, p) : (direction, g) : ps) =
          restructure $ (direction, zigzagRL g p c) : ps
        restructure ((R, c) : (L, p) : (direction, g) : ps) =
          restructure $ (direction, zigzagLR g p c) : ps

search :: Ord a => a -> Tree a -> Tree a
search _ None = None
search key n@(Node k l r) 
  | key == k = n
  | key < k = search key l 
  | key > k = search key r

insert :: Ord a => a -> Tree a -> Tree a
insert key None = Node key None None
insert key tree@(Node k l r) = case compare key k of
  EQ -> tree
  LT -> Node k (insert key l) r
  GT -> Node k l (insert key r)

-- mergeLR l r
--   | (Node _ _ _) Empty = l
--   | Empty (Node _ _ _) = r
--   | l@(Node kl ll rl) r@(Node kr lr rr) =
--     Node leastRKey (splay leastRKey r) l

-- delete :: Ord a => a -> Tree a -> Tree a
-- delete key (Node k l r) =  

testTree :: Tree Char
testTree = Node 'g' (Node 'f' (Node 'e' (Node 'd' None None) None) None) None

-- doSearch :: IO ()
-- doSearch = do
--   putStrLn "Enter key to search by: "
--   input <- getLine
--   let key = read input :: Char
--   putStrLn "Was node found?"
--   let result = search key testTree 
--   putStrLn $ show result
--   let tree = if result == True then splay key testTree else testTree
--   putStrLn "Tree after accessing node: "
--   putStrLn $ show tree

doInsert :: IO ()
doInsert = do
  putStrLn "Enter key to insert: "
  input <- getLine
  let key = read input :: Char
  let tree = insert key testTree
  let result = splay key tree
  putStrLn "Result: "
  putStrLn $ show result


data Elem = Elem Int Int deriving (Show)

instance Eq Elem where
  (Elem k1 _) == (Elem k2 _) = k1 == k2

instance Ord Elem where
  compare (Elem k1 _) (Elem k2 _) = compare k1 k2


-- k = 16777259
-- k = 65537
k = 131
-- n = 2 ^ 47 + 9
-- n = 10 ^ 9 + 7
n = 65537

makeElemList = [Elem ((k * x + 1001) `mod` n) x | x <- [1, 2..65537]]

buildTreeFromList (x:[]) = Right $ insert x None
buildTreeFromList (x:xs) = case search x tree of
  None -> Right $ splay x (insert x tree)
  (Node e@(Elem k v) _ _) -> Left (x, v)
  where Right tree = buildTreeFromList xs

test :: IO ()
test = do
  let elems = makeElemList
  print (buildTreeFromList elems)




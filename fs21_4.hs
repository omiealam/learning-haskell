-- Task 1.A
-- In this first part of the exercise you will implement a data type that models a set. The type is defined as:

data Set h = Empty | Elem h (Set h) (Set h)

-- The set should be implemented as a binary search tree. 
-- In other words, every set of the form Elem x lt rt should satisfy the search tree property: 
-- 1) all elements in lt are strictly smaller than x 
-- 2) all elements in rt are strictly greater than x
-- 3) the same holds for lt and rtrecursively. 
-- You may assume that the functions in this assignment will only be given arguments of type Set h that satisfy the search tree property (i.e. are already ordered).
-- For the following, recall that the typeclass Ord provides the operators: (==), (/=), (<=),(<), (>=), (>).

-- 1. Implement the function add that adds an element to the set and returns a well-formed binary search tree. 
-- The tree does not need to be balanced. It has the following signature:

add :: Ord h => Set h -> h -> Set h
add Empty el = Elem el Empty Empty
add (Elem h l r) el
  | el < h    = Elem h (add l el) r
  | otherwise = Elem h l (add r el)


-- 2. Implement the function query that checks set membership and has the following signature:

query :: Ord h => Set h -> h -> Bool
query Empty el = False
query (Elem h l r) el 
  | h == el   = True
  | otherwise = (query l el) || (query r el)
  
  
-- Task 1.B
-- Use the Set data type to implement a Bloom Filter. A Bloom Filter is a probabilistic representation of a set. 
-- Each Bloom Filter is parametrized by a collection of hash functions, and instead of storing elements directly, it stores their corresponding hash values in a single set.
-- The Bloom Filter type is defined for elements of type v and hash values of type h as follows

data BloomFilter v h = BloomFilter [v -> h] (Set h)

-- Note that the collection of hash functions is represented by a list of type [v -> h] and the underlying set of hash values by a set of type Set h.
-- 1. Implement the function insert that adds an element to a Bloom Filter. 
-- To add an element e, apply each of the hash functions to e and add the obtained outputs to the set of hash values.

insert :: Ord h => BloomFilter v h -> v -> BloomFilter v h
insert (BloomFilter lst set) el = BloomFilter lst (foldl (\acc el -> add acc el) Empty [f el | f <- lst])

-- Alternatively without foldl
insert :: Ord h => BloomFilter v h -> v -> BloomFilter v h
insert (BloomFilter lst set) el = BloomFilter lst (aux [f el | f <- lst] set)
  where
    aux [] s      = s
    aux (x:xs) s  = aux xs (add s x)
   
    
-- 2. Implement the function check that checks for the presence of an element in the filter.
-- This function should check whether every hash function’s output for the given element is in the underlying set (Set h).

check :: Ord h => BloomFilter v h -> v -> Bool
check (BloomFilter lst set) el = and (map (query set) [f el | f <- lst])


-- 3. Implement the function fromList that transforms a given list of elements (of type [v])into a Bloom Filter parametrized by a given list of hash functions (of type [v -> h]).

fromList :: Ord h => [v] -> [v -> h] -> BloomFilter v h
fromList els lst = BloomFilter lst (foldl (\acc el -> add acc el) Empty [f x | x <- els, f <- lst])

-- Alternatively without foldl
fromList :: Ord h => [v] -> [v -> h] -> BloomFilter v h
fromList els lst = BloomFilter lst (aux [f x | f <- lst, x <- els] Empty)
  where
    aux [] s      = s
    aux (x:xs) s  = aux xs (add s x)

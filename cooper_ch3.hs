--Heap Code.

-- Class of Heaps.
class (Ord e) => Heap e where 
    data Hp e :: *

    empty :: (Hp e)
    isEmpty :: (Hp e) -> Bool

    insert :: e -> (Hp e) -> (Hp e)
    merge :: (Hp e) -> (Hp e) -> (Hp e)

    findMin :: (Hp e) -> e
    deleteMin :: (Hp e) -> (Hp e)
     
-- Heap of Integers.
instance Heap Int where
    data Hp Int = E
                | T Int Int (Hp Int) (Hp Int)
                  deriving(Show)

    empty = E
    isEmpty E = True
    isEmpty _ = False

    merge h E = h
    merge E h = h
    merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
        let  rank E = 0
             rank (T r _ _ _) =  r
                                          
             makeT x a b = 
                 if (rank a) >= (rank b) then 
                     T ((rank b) + 1) x a b
                 else 
                     T ((rank a) + 1) x b a

        in if x <= y then 
               makeT x a1 (merge b1 h2)
           else 
               makeT y a2 (merge h1 b2)

    insert x h = merge (T 1 x E E) h
    findMin (T _ x a b) = x
    deleteMin (T _ x a b) = merge a b


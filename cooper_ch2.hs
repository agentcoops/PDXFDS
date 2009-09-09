-- EXERCISE 2.1: CPS SUFFIXES IMPLEMENTATION.

suffixes:: [a] -> [[a]]

suffixes [] = []
suffixes lst@(hd:tl) = suffixes' (\x -> lst:x) tl
    where
      suffixes' f [] = f []
      suffixes' f lst@(hd:tl) = suffixes' (\x -> f (lst:x)) tl


-- BINARY TREES.

data Tree e = E
            | T (Tree e) e (Tree e)
              deriving (Show)

member x E = False
member x (T l v r) = 
    if x < v then member x l
    else if x > v then member x r
    else True

insert x E = T E x E
insert x s@(T a y b) = 
    if x < y then T (insert x a) y b
    else if x > y then T a y (insert x b)
    else s

depth E = 0
depth (T l x r) = max (1+(depth l)) (1+(depth r)) 

--Exercise 2.2: only d+1 comparisons where d is the depth of the tree.
and_member x E = False
and_member x tree@(T l v r) = and_member' v tree 
    where and_member' last E = 
              if x == last then True
              else False
          and_member' last (T l v r) = 
              if x < v then and_member' last l
              else and_member' v r

--Exercise 2.3: Rewrite insert to not copy entire search path.
-- Written in CPS style with success and failure continuations! 
ex3_insert x t =
    let fail = t
        ex_insert' succ tr =
            case tr of
              E -> succ (T E x E)
              T l v r ->
                  if x < v then (ex_insert' (\tree -> T (succ tree) v r)
                                            l)
                  else if x > v then (ex_insert' (\tree -> T l v (succ tree))
                                                 r)
                  else fail
    in ex_insert' (\tree -> tree) t     

--Exercise 2.4: Combine previous two exercises. 
ex4_insert x t =
    let fail = t
        ex_insert' succ last tr =
            case tr of
              E ->
                  if x == last then fail
                  else succ (T E x E)
              T l v r ->
                  if x < v then (ex_insert' (\tree -> T (succ tree) v r)
                                            last 
                                            l)
                  else (ex_insert' (\tree -> T l v (succ tree))
                                   v 
                                   r)                    
   in case t of
        E -> T E x E
        T l v r -> ex_insert' (\tree -> tree) v t

--Exercise 2.5a: Make an unbalanced tree containing only value x.
complete x d = complete' 1 (T E x E) 
    where complete' level t =
              if level == d then t
              else complete' (level+1) (T E x t)

--Exercise 2.5b: Create a balanced tree containing only value x.
balancedComplete x d = complete' d (T E x E)
    where complete' 0 t = t
          complete' level t = complete' (level-1) (T t x t)

--Exercise 2.6.
data Map k v = ME 
             | MT (Map k v) k v (Map k v)
               deriving(Show)



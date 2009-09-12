-- Copyright © 2009 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the GNU GPL version 2.

import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity

data Tree a
  = Nil
  | Tree (Tree a) a (Tree a)
  deriving (Eq, Show)

ex3 :: (Ord a) => Tree a -> a -> Tree a
ex3 t x = maybe t id $ real_ex3 t x
  where
    real_ex3 :: (Ord a) => Tree a -> a -> Maybe (Tree a)
    real_ex3 Nil x                       = return $ Tree Nil x Nil
    real_ex3 (Tree l x' r) x | x == x'   = mzero -- or Nothing
                             | x < x'    = do l' <- real_ex3 l x; return (Tree l' x' r)
                             | otherwise = do r' <- real_ex3 r x; return (Tree l x' r')

runError :: ErrorT e Identity a -> (e -> a) -> a
runError m f = case (runIdentity $ runErrorT m) of
                 Left e  -> f e
                 Right a -> a

ex3E :: (Ord a) => Tree a -> a -> Tree a
ex3E t x = runError (real_ex3E t x) $ const t
  where
    real_ex3E :: (Ord a) => Tree a -> a -> ErrorT String Identity (Tree a)
    real_ex3E Nil x                       = return $ Tree Nil x Nil
    real_ex3E (Tree l x' r) x | x == x'   = throwError "Element already exists!"
                              | x < x'    = do l' <- real_ex3E l x; return (Tree l' x' r)
                              | otherwise = do r' <- real_ex3E r x; return (Tree l x' r')

ex3IO :: (Ord a) => Tree a -> a -> IO (Tree a)
ex3IO t x = catch (real_ex3IO t x) $ \_ -> return t
  where
    real_ex3IO :: (Ord a) => Tree a -> a -> IO (Tree a)
    real_ex3IO Nil x                       = return $ Tree Nil x Nil
    real_ex3IO (Tree l x' r) x | x == x'   = fail "Element already exists!"
                               | x < x'    = do l' <- real_ex3IO l x; return (Tree l' x' r)
                               | otherwise = do r' <- real_ex3IO r x; return (Tree l x' r')

ex3' :: (Ord a) => Tree a -> a -> Tree a
ex3' Nil x                         = Tree Nil x Nil
ex3' t@(Tree l x' r) x | x == x'   = t
                       | x < x'    = Tree (ex3' l x) x' r
                       | otherwise = Tree l x' (ex3' r x)

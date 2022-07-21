{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pretty
  (
    Pretty(..)
  , ByShow(..)
  ) where

class Show e => Pretty e where
    prettify :: e -> [String]

newtype ByShow s = BS { unBS :: s }
instance Show (ByShow s) => Pretty (ByShow s) where
    prettify sh = [show sh]
    
instance Show (ByShow String) where
    show bs = unBS bs

instance Pretty term => Pretty [term] where
    prettify terms = concatMap tabTerm terms
      where tabTerm t = map (" " ++) (prettify t)
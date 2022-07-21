module Pretty
  (Pretty(..)) where

class Show e => Pretty e where
  prettify :: e -> [String]
  
instance Pretty term => Pretty [term] where
    prettify terms = concatMap tabTerm terms
      where tabTerm t = map (" " ++) (prettify t)
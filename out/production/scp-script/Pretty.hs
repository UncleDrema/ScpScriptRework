module Pretty
  (Pretty(..)) where

class Show e => Pretty e where
  prettify :: e -> [String]
module PPrint ( module PPrint
              , module Text.PrettyPrint) where

import Text.PrettyPrint

class PPrint a where
  pprint :: a -> Doc

sepBy s [] =
  empty
sepBy s xs =
  hcat (map (\x -> x <> s) (init xs)) <> last xs

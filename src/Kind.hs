module Kind where

import PPrint
import Token
import Id

data Kind
  = KType
  | KVar Id
  | KFn Kind Kind

instance Show Kind where
  show k = render (pprint k)

instance PPrint Kind where
  pprint (KType) =
    text "*"
  pprint (KVar v) =
    pprint v
  pprint (KFn a b) =
    let isFactor KType = True
        isFactor (KVar _) = True
        isFactor _ = False
        pprintFactor k =
          if isFactor k
          then pprint k
          else pprint TkLParen <> pprint k <> pprint TkRParen
    in pprintFactor a <+> pprint TkArrow <+> pprint b

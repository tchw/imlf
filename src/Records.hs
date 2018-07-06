module Records ( module Data.Default
               , module Data.OverloadedRecords
               , module Data.OverloadedRecords.TH
               , module Control.Lens ) where

import Data.Default (Default(def))
import Data.OverloadedRecords hiding (Getter, Setter, Setter', Setting, set, set', setting)
import Data.OverloadedRecords.TH (overloadedRecord)
import Control.Lens

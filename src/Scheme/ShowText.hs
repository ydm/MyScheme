module Scheme.ShowText where

import qualified Data.Text as T (Text)

class ShowText a where
  showt :: a -> T.Text


instance ShowText T.Text where
  showt = id

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.PlutusTx.IsData.Instances where

import Language.PlutusTx.IsData.TH
import Prelude (Bool (..), Either (..), Maybe (..))

-- While these types should be stable, we really don't want them changing, so index
-- them explicitly to be sure.
makeIsDataIndexed ''Bool [('False, 0), ('True, 1)]
makeIsData ''()
makeIsData ''(,)
makeIsData ''(,,)
makeIsData ''(,,,)
makeIsDataIndexed ''Maybe [('Just, 0), ('Nothing, 1)]
makeIsDataIndexed ''Either [('Left, 0), ('Right, 1)]

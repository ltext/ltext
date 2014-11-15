module LText where

import LText.Internal.Types


-- | larger to smaller: eg) (a -> a) :: Int -> Int
coerceTo :: Type -> Type -> Maybe [(Label, Type)]
x `coerceTo` y = undefined

-- | smaller to larger: eg) (Int -> Int) :: a -> a
satisfies :: Type -> Type -> Maybe [(Label, Type)]
x `satisfies` y = undefined

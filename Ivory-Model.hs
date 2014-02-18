{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude hiding (init)

--------------------------------------------------------------------------------

----------------------------------------
-- Some monad
newtype Id a = Id a

instance Monad Id where
  return a = Id a
  Id a >>= f = f a
----------------------------------------

data RetEff   = forall r. Ret r | NoRet
data AllocEff = forall s. Scope s | NoAlloc
data Effects  = Effects AllocEff RetEff

type family   GetRet (effs :: Effects) :: RetEff
type instance GetRet ('Effects s r) = r

type family   GetAlloc (effs :: Effects) :: AllocEff
type instance GetAlloc ('Effects s r) = s

type FnEffects s r = 'Effects (Scope s) (Ret r)

----------------------------------------

newtype Ivory (eff :: Effects) a = Ivory (Id a)
  deriving Monad

newtype Body r = Body
  { runBody :: forall s. Ivory (FnEffects s r) () }

ret :: (GetRet eff ~ Ret r) => r -> Ivory eff ()
ret r = undefined -- Ivory (put Return)

i0 :: Body Integer
i0 = Body (ret 42)

----------------------------------------
data RefScope
  = Global
  | forall s. Stack s

data Ref (ref :: RefScope) a = Ref

-- XXX not saying what we're allocating
local :: (GetAlloc eff ~ Scope s) => Ivory eff (Ref (Stack s) a)
local = undefined

deref :: Ref ref a -> Ivory eff a
deref ref = undefined

----------------------------------------

i1 :: Num r => Body r
i1 = Body $ do
  l <- local
  v <- deref l
  ret v

i2 :: Ref (Stack s) a -> Body (Ref (Stack s) a)
i2 l = Body $ do
  v <- deref l
  ret l

-- Can't type-check
--i3 :: Body (Ref (Stack x) a)
--i3 = Body $ ret =<< local

--------------------------------------------------------------------------------

-- newtype Writer w a = Writer
--   { runWriter :: (a, [w]) }

-- instance Monad (Writer w) where
--   return a = Writer (a, [])
--   (Writer (a, ls)) >>= f = let (Writer (a', ls')) = f a in
--                            Writer (a', ls ++ ls')

-- put :: w -> Writer w ()
-- put w = Writer ((), [w])

----------------------------------------

-- data AST = Return
--          | Local

-- newtype Ivory (eff :: Effects) a = Ivory (Writer AST a)
--   deriving Monad


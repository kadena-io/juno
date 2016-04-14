{-# LANGUAGE RankNTypes , ScopedTypeVariables #-}

module Juno.Hoplite.Term where

import qualified Juno.Hoplite.Types as T
import Data.Text (Text)
-- import qualified
import   Bound as Bd
import Unsafe.Coerce (unsafeCoerce)


data HopliteTerm = V  Text
    | ELit T.Literal
    -- | PrimApp Text [a]
    -- | Force HopliteTerm  --- Force is a Noop on evaluate values,
    --                     --- otherwise reduces expression to applicable normal form
    --                     -- should force be more like seq a b, cause pure

    -- | Delay HopliteTerm  --- Delay is a Noop on Thunked values, otherwise creates a thunk
                        --- note: may need to change their semantics later?!
    | HopliteTerm :@ [HopliteTerm]
    | PrimApp  Text [HopliteTerm] -- not sure if this is needed, but lets go with it for now

    | Lam [Text]
          HopliteTerm
    | Let Text  HopliteTerm  HopliteTerm
  deriving (Eq,Ord,Read,Show)


-- data DTS a =DTV a
--     | DTLIt T.Literal
--     | DTForce (DTS a)  --- Force is a Noop on evaluate values,
--                         --- otherwise reduces expression to applicable normal form
--                         -- should force be more like seq a b, cause pure

--     | DTDelay (DTS a)  --- Delay is a Noop on Thunked values, otherwise creates a thunk
--                         --- note: may need to change their semantics later?!
--     | (DTS a) :?@ [DTS a]
--     | DTPrimApp  String [DTS a] -- not sure if this is needed, but lets go with it for now

--     | DTLam [String]
--           (Scope String DTS a)
--     | DTLet String  (DTS a)  (Scope String DTS a)

hopliteTerm2ScopedTerm :: HopliteTerm -> T.Exp () Text
hopliteTerm2ScopedTerm (V str) = T.V $  str
hopliteTerm2ScopedTerm (ELit l) = T.ELit l
-- hopliteTerm2ScopedTerm (Force e) = T.Force  $ hopliteTerm2ScopedTerm e
-- hopliteTerm2ScopedTerm (Delay e) = T.Delay $ hopliteTerm2ScopedTerm e
hopliteTerm2ScopedTerm (f :@ lse) = hopliteTerm2ScopedTerm f T.:@ map hopliteTerm2ScopedTerm lse
hopliteTerm2ScopedTerm (PrimApp nm ls) = T.PrimApp (T.PrimopId  nm) (map hopliteTerm2ScopedTerm ls)
hopliteTerm2ScopedTerm (Lam args bod) = let dtsBod = hopliteTerm2ScopedTerm bod
                                       in T.Lam  (map (\v -> (v,T.TVar (),T.Omega)) args) $  flip  abstract dtsBod (\var -> if  var `elem` args then Just $ var else Nothing)
hopliteTerm2ScopedTerm (Let nm rhs bod) = T.Let (Just nm) Nothing (hopliteTerm2ScopedTerm rhs) $ flip abstract (hopliteTerm2ScopedTerm bod) (\var -> if var == nm then Just (Just nm) else Nothing )

newtype PolyMF f = PolyMF { unPolyF :: forall a . Maybe (f a) }
newtype PolyF f = PolyF { unPolyF2 :: forall a . f a }

evaluableHopliteTerm :: HopliteTerm -> Maybe (PolyF (T.Exp ()))
evaluableHopliteTerm = hoistPolyMaybe . validateClosed
  where
    hoistPolyMaybe :: PolyMF f -> Maybe (PolyF f)
    hoistPolyMaybe pf = case unPolyF pf of
                          Nothing -> Nothing
                          (Just e) -> Just $ PolyF $ unsafeCoerce e

    validateClosed :: HopliteTerm -> PolyMF (T.Exp ())
    validateClosed tm = PolyMF (closed $ hopliteTerm2ScopedTerm tm)

{-# LANGUAGE GADTs #-}

module Rules where

import GHC.Core.TyCo.Rep
import GHC.Core.Map.Type (DeBruijn (..), deBruijnize, emptyCME)
import GHC.Core.Predicate
import GHC.Driver.Ppr (showPprUnsafe)
import GHC.Plugins (Outputable(ppr))

import Debug.Trace (trace)

data MyTypeApp where
  Ty :: DeBruijn Type -> MyTypeApp
  AddTy :: MyTypeApp -> MyTypeApp -> MyTypeApp
  EqTy :: MyTypeApp -> MyTypeApp -> MyTypeApp
  deriving (Eq)

hackPrint :: DeBruijn Type -> String
hackPrint (D _ t) = showPprUnsafe $ ppr t

instance Show MyTypeApp where
  show (Ty t) = "Ty " ++ hackPrint t
  show (AddTy lhs rhs) = "AddTy (" ++ show lhs ++ ") (" ++ show rhs ++ ")"
  show (EqTy lhs rhs) = "EqTy (" ++ show lhs ++ ") (" ++ show rhs ++ ")"

typeAddComm :: MyTypeApp -> Bool
typeAddComm (Ty _)  = True
typeAddComm (AddTy _ _) = True
typeAddComm (EqTy (AddTy x y) (AddTy y' x'))
  = trace (show x ++ "==" ++ show x' ++ " ? " ++ show (x == x'))
  $ x == x' && y == y'
typeAddComm _ = False

tryMkMyTypeApp :: Pred -> Maybe MyTypeApp
tryMkMyTypeApp (EqPred _ lhs rhs) = do
  lapp <- tryFromType lhs
  rapp <- tryFromType rhs
  return (EqTy lapp rapp)
  where
    tryFromType :: Type -> Maybe MyTypeApp
    tryFromType (TyConApp _ [lhs', rhs']) = Just $ AddTy (Ty $ deBruijnize lhs') (Ty $ deBruijnize rhs')
    tryFromType _ = Nothing
tryMkMyTypeApp _ = Nothing

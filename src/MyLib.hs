{-# LANGUAGE DataKinds #-}
module MyLib (someFunc, plugin) where

import System.IO

import GHC.Plugins (CommandLineOption, Outputable (ppr), Plugin (..), defaultPlugin)
import GHC.Tc.Types.Constraint-- (Ct (CDictCan))
import GHC.Utils.Outputable (($$), showPprUnsafe)

import qualified GHC.TcPlugin.API
import GHC.TcPlugin.API
import Data.List (intersperse)
import Data.Data (Data(dataTypeOf), dataTypeName)
import GHC.Core.TyCo.Rep
import Rules

data PluginCtx = PluginCtx
  { plArgs :: [String]
  , logHandle :: Handle
  } deriving (Show)

--plugin :: [CommandLineOption] -> TcPlugin
--plugin _ = TcPlugin
--  { tcPluginInit = pluginInit
--  , tcPluginSolve = pluginSolve
--  , tcPluginStop = pluginStop
--  }

plugin :: GHC.Plugins.Plugin
plugin = GHC.Plugins.defaultPlugin
  { GHC.Plugins.tcPlugin = Just . GHC.TcPlugin.API.mkTcPlugin . mkPlugin }

mkPlugin :: [String] -> GHC.TcPlugin.API.TcPlugin
mkPlugin args = TcPlugin
  { tcPluginInit = pluginInit args
  , tcPluginSolve = pluginSolve
  , tcPluginRewrite = const emptyUFM
  , tcPluginStop = pluginStop
  }

pluginInit :: [String] -> GHC.TcPlugin.API.TcPluginM Init PluginCtx
pluginInit args = do
  tcPluginTrace ("::pluginInit args=" ++ show args) (ppr ())
  h <- tcPluginIO $ openFile "/tmp/tc-plugin.log" WriteMode
  return PluginCtx { plArgs = args, logHandle = h }

pluginSolve :: PluginCtx -> [Ct] -> [Ct] -> TcPluginM Solve TcPluginSolveResult
pluginSolve ctx givens wanteds = do
  tcPluginTrace ("::pluginSolve " ++ show ctx) (ppr givens $$ ppr wanteds)
  tcPluginIO $ hPutStrLn (logHandle ctx) $ "::pluginSolve {"
  --tcPluginIO $ hPutStrLn (logHandle ctx) $ "  givens = " ++ showPprUnsafe (ppr givens) ++ show (map myInspect givens)
  --tcPluginIO $ hPutStrLn (logHandle ctx) $ "  wanteds = " ++ showPprUnsafe (ppr wanteds) ++ show (map myInspect wanteds)
  case (tryParseCt $ head wanteds) of
    Just app -> do
      tcPluginIO $ hPutStrLn (logHandle ctx) $ show $ app
      tcPluginIO $ hPutStrLn (logHandle ctx) $ show $ typeAddComm app
    _ -> pure ()
  tcPluginIO $ hPutStrLn (logHandle ctx) $ "}"
  return $ TcPluginOk [] []

pluginStop :: PluginCtx -> TcPluginM Stop ()
pluginStop ctx = do
  tcPluginTrace "::pluginStop" (ppr ())
  tcPluginIO $ hClose (logHandle ctx)
  return ()

myInspect :: Ct -> String
myInspect (CDictCan _ _ _ _) = "CDictCan"
myInspect (CIrredCan _ _) = "CIrredCan"
myInspect (CEqCan _ _ _ _) = "CEqCan"
myInspect (CQuantCan _) = "CQuantCan"
--myInspect (CNonCanonical ev) = myInspectEv ev
myInspect ct = myInspectPred $ (classifyPredType . ctPred) ct

myInspectPred :: Pred -> String
myInspectPred (ClassPred _ _) = "ClassPred"
myInspectPred (IrredPred _) = "IrredPred"
myInspectPred (ForAllPred _ _ _) = "ForAllPred"
myInspectPred (EqPred _ lhs rhs) = (showPprUnsafe . ppr $ lhs) ++ (myInspectType lhs) ++ " ;; " ++ (showPprUnsafe . ppr $ rhs) ++ (myInspectType rhs)
--myInspectPred (EqPred _ lhs rhs) = (f . splitAppTy_maybe $ lhs) ++ " ;; " ++ (f . splitAppTy_maybe $ rhs)
--  where
--    f :: Maybe (Type, Type) -> String
--    f Nothing = "Nothing"
--    f (Just (ta, tb)) = (showPprUnsafe . ppr $ ta) ++ "," ++ (showPprUnsafe . ppr $ tb)

myInspectType :: Type -> String
myInspectType (TyVarTy _) = "TyVarTy"
myInspectType (AppTy _ _) = "AppTy"
myInspectType (TyConApp con args) = "TyConApp, " ++ (showPprUnsafe . ppr $ con) ++ " " ++ show (map (showPprUnsafe . ppr) args)
myInspectType (ForAllTy _ _) = "ForAllTy"
myInspectType (FunTy _ _ _ _) = "FunTy"
myInspectType (LitTy _) = "LitTy"
myInspectType (CastTy _ _) = "CastTy"
myInspectType (CoercionTy _) = "CoercionTy"

tryParseCt :: Ct -> Maybe MyTypeApp
tryParseCt = tryMkMyTypeApp . classifyPredType . ctPred

someFunc :: IO ()
someFunc = putStrLn "someFunc"

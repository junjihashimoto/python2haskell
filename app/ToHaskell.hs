{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module ToHaskell where

import Language.Python.Common

import Language.Python.Version3 as V3
import System.Exit
import System.Environment
import Text.Pretty.Simple (pPrint)
import qualified Language.Haskell.TH as TH
import qualified Data.Map as M
-- import qualified GHC.Classes
import Control.Monad.Trans.State.Strict
import Data.Maybe (catMaybes)
import qualified Data.Text as T

data ModuleData =
  ModuleData
  { variables :: M.Map String TH.Name
  , statements :: [TH.Exp]
  } deriving (Show, Eq)

type QN a = StateT ModuleData TH.Q a

addExpr :: TH.Exp -> QN ()
addExpr expr = do
  modify (\v -> v { statements = (statements v) ++ [expr]})

addVar :: String -> QN TH.Name
addVar name = do
  let n = TH.mkName name
  modify (\v -> v { variables = M.insert name n (variables v) })
  return n

getVar :: String -> QN (Maybe TH.Name)
getVar name = do
  v <- get
  return (M.lookup name (variables v))

delVar :: String -> QN TH.Name
delVar name = do
  let n = TH.mkName name
  modify (\v -> v { variables = M.delete name (variables v) })
  return n

toHaskell' :: ModuleSpan -> IO T.Text
toHaskell' ast = do
  decs <- TH.runQ (toHaskell ast) :: IO [TH.Dec]
  TH.LitE (TH.StringL str_ast) <- TH.stringE (TH.pprint decs)
  let txt = T.pack str_ast
  return $
    T.replace "GHC.Num." "" $
    T.replace "GHC.Classes." "" $
    T.replace "System.IO." "" txt


toHaskell :: ModuleSpan -> TH.Q [TH.Dec]
toHaskell (Module module') = do
  let stat =
        ModuleData
        { variables = (M.fromList
                        [ ("print", 'print)
                        ])
        , statements = []
        }
  (stats,stat') <- flip runStateT stat $ do
    v <- mapM fromStatement module'
    return $ catMaybes v
  let m = TH.mkName "main"
      exprs = map TH.NoBindS (statements stat')
  if length exprs == 0
    then return stats
    else return $ stats ++ [ TH.ValD (TH.VarP m) (TH.NormalB (TH.DoE Nothing exprs)) [] ]

fromArguments :: Argument SrcSpan -> QN TH.Exp
fromArguments (ArgExpr expr _) = fromExpression expr

fromExpression :: Expr SrcSpan -> QN TH.Exp
fromExpression (Int value literal _) = do
  return $ TH.LitE ( TH.IntegerL value )
fromExpression (LongInt value literal _) = do
  return $ TH.LitE ( TH.IntegerL value )
fromExpression (Float value literal _) = do
  return $ TH.LitE ( TH.DoublePrimL (toRational value) )
fromExpression (Var name' _) = do
  let name = ident_string name'
  n <- getVar name
  case n of
    Just n' -> return $ TH.VarE n'
    Nothing -> fail $ "Cannot find '" ++ name ++ "'."
fromExpression (Call fn args _) = do
  fn' <- fromExpression fn
  args' <- mapM fromArguments args
  return  $ foldl TH.AppE fn' args'

fromExpression (BinaryOp v expl expr _) = do
  l <- fromExpression expl 
  r <- fromExpression expr
  let op = case v of
             (And _) -> '(&&)
             (Or _) -> '(||)
             -- (Not _) -> '(not)
             (Exponent _) -> '(**)
             (LessThan _) -> '(<)
             (GreaterThan _) -> '(>)
             (Equality _) -> '(==)
             (GreaterThanEquals _) -> '(>=)
             (LessThanEquals _) -> '(<=)
             (NotEquals _) -> '(/=)
--             (NotEqualsV2 _) -> '()
--             (In _) -> '()
--             (Is _) -> '()
--             (IsNot _) -> '()
--             (NotIn _) -> '()
--             (BinaryOr _) -> '()
--             (Xor _) -> '()
--             (BinaryAnd _) -> '()
--             (ShiftLeft _) -> '()
--             (ShiftRight _) -> '()
             (Multiply _) -> '(*)
             (Plus _) -> '(+)
             (Minus _) -> '(-)
             (Divide _) -> '(/)
--             (FloorDivide _) -> '()
--             (MatrixMult _) -> '()
--             (Invert _) -> '()
--             (Modulo _) -> '(%)
  return $ TH.InfixE ( Just l)  ( TH.VarE op ) ( Just r )

-- fromCondition :: (Expr SrcSpan, Suite SrcSpan) -> a -> QN TH.Exp
-- fromCondition (cond, [(Return (Just ret) _)]) a = do
--   cond' <- fromExpression cond
--   ret' <- fromExpression ret
--   return $ TH.CondE cond' ret'


fromConditions :: [(Expr SrcSpan, Suite SrcSpan)] -> Suite SrcSpan -> QN TH.Exp
fromConditions ((cond, ret):conds) final_ret = do
  cond' <- fromExpression cond
  ret' <- fromBody ret
  v <- fromConditions conds final_ret
  return $ TH.CondE cond' ret' v
fromConditions [] ret'' = do
  fromBody ret''
  
fromBody :: [Statement SrcSpan] -> QN TH.Exp
fromBody [Return (Just ret) _]  = do
  fromExpression ret
fromBody [Conditional conds else' _]  = do
  fromConditions conds else'

withName :: String -> (TH.Name -> QN a) -> QN a
withName name fn= do
  n <- addVar name
  v <- fn n
  delVar name
  return v


fromParam ::  Parameter SrcSpan -> QN TH.Pat
fromParam param = do
  n <- addVar (ident_string (param_name param))
  return $ TH.VarP n

withParams :: [Parameter SrcSpan] -> ([TH.Pat] -> QN a) -> QN a
withParams args fn = do
  let names = map (ident_string.param_name) args
  params <- mapM fromParam args
  v <- fn params
  mapM_ delVar names
  return v

fromStatement :: Statement SrcSpan -> QN (Maybe TH.Dec)
fromStatement (Fun name args result_annot body _) = do
  let str = ident_string name
  n <- addVar str
  
--  withName (ident_string name) $ \n -> do
  withParams args $ \params -> do
    expression <- fromBody body
    return $ Just $ TH.FunD n [ TH.Clause params (TH.NormalB expression) [] ]
fromStatement (StmtExpr sts _) = do
  s <- fromExpression sts
  addExpr s
  return Nothing

fromStatement a = do
  fail $ "Error:" ++ show a

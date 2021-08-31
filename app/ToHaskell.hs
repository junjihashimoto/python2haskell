{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ToHaskell where

-- import qualified GHC.Classes

import Control.Monad (forM)
import Control.Monad.Trans.State.Strict
import qualified Data.Map as M
-- import qualified GHC.Types
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Language.Haskell.TH as TH
import Language.Python.Common
import Text.Pretty.Simple (pShow)

show' :: Show a => a -> String
show' = T.unpack . TL.toStrict . pShow

data ModuleData = ModuleData
  { variables :: M.Map String TH.Name,
    statements :: [TH.Exp]
  }
  deriving (Show, Eq)

type QN a = StateT ModuleData TH.Q a

addExpr :: TH.Exp -> QN ()
addExpr expr = do
  modify (\v -> v {statements = statements v ++ [expr]})

addVar :: String -> QN TH.Name
addVar name = do
  let n = TH.mkName name
  modify (\v -> v {variables = M.insert name n (variables v)})
  return n

getVar :: String -> QN (Maybe TH.Name)
getVar name = M.lookup name . variables <$> get

delVar :: String -> QN ()
delVar name = do
  modify (\v -> v {variables = M.delete name (variables v)})

toHaskell' :: ModuleSpan -> IO T.Text
toHaskell' ast = do
  decs <- TH.runQ (toHaskell ast) :: IO [TH.Dec]
  TH.LitE (TH.StringL str_ast) <- TH.stringE (TH.pprint decs)
  let txt = T.pack str_ast
  return $
    T.replace "GHC.Num." "" $
      T.replace "GHC.Classes." "" $
        T.replace "GHC.Types." "" $
          T.replace "GHC.Show." "" $
            T.replace "System.IO." "" txt

toHaskell :: ModuleSpan -> TH.Q [TH.Dec]
toHaskell (Module module') = do
  let stat =
        ModuleData
          { variables =
              M.fromList
                [ ("print", 'print)
                ],
            statements = []
          }
  (stats, stat') <- flip runStateT stat $ do
    v <- mapM fromStatement module'
    return $ concat v
  let m = TH.mkName "main"
      exprs = map TH.NoBindS (statements stat')
  if null exprs
    then return stats
    else return $ stats ++ [TH.ValD (TH.VarP m) (TH.NormalB (TH.DoE Nothing exprs)) []]

fromArguments :: Argument SrcSpan -> QN TH.Exp
fromArguments arg =
  case arg of
    ArgExpr expr _ -> fromExpression expr
    ArgVarArgsPos _ _ -> fail $ "fromArguments: " ++ show' arg ++ "is not defined "
    ArgVarArgsKeyword _ _ -> fail $ "fromArguments: " ++ show' arg ++ "is not defined "
    ArgKeyword _ _ _ -> fail $ "fromArguments: " ++ show' arg ++ "is not defined "

fromExpression :: Expr SrcSpan -> QN TH.Exp
fromExpression v@(Imaginary _ _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression (Bool True _) = do
  return $ TH.ConE 'True
fromExpression (Bool False _) = do
  return $ TH.ConE 'False
fromExpression v@(None _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(Ellipsis _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(ByteStrings _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(Strings _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(UnicodeStrings _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(Subscript _ _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(SlicedExpr _ _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(CondExpr _ _ _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(UnaryOp _ _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(Dot _ _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(Lambda _ _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(Tuple _ _) = fail $ "fromExpression:" ++ show' v ++ "is not defined "
fromExpression v@(Yield _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(Generator _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(Await _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(ListComp _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(List _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(Dictionary _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(DictComp _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(Set _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(SetComp _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(Starred _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression (Paren expr _) = fromExpression expr
fromExpression v@(StringConversion _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression (Int value _ _) = do
  return $ TH.LitE (TH.IntegerL value)
fromExpression (LongInt value _ _) = do
  return $ TH.LitE (TH.IntegerL value)
fromExpression (Float value _ _) = do
  return $ TH.LitE (TH.DoublePrimL (toRational value))
fromExpression (Var name' _) = do
  let name = ident_string name'
  n <- getVar name
  case n of
    Just n' -> return $ TH.VarE n'
    Nothing -> fail $ "Cannot find '" ++ name ++ "'."
fromExpression (Call fn args _) = do
  fn' <- fromExpression fn
  args' <- mapM fromArguments args
  return $ foldl TH.AppE fn' args'
fromExpression (BinaryOp v expl expr _) = do
  l <- fromExpression expl
  r <- fromExpression expr
  op <- case v of
    (And _) -> return '(&&)
    (Or _) -> return '(||)
    (Not _) -> fail $ show' v ++ " is not defined."
    (Exponent _) -> return '(**)
    (LessThan _) -> return '(<)
    (GreaterThan _) -> return '(>)
    (Equality _) -> return '(==)
    (GreaterThanEquals _) -> return '(>=)
    (LessThanEquals _) -> return '(<=)
    (NotEquals _) -> return '(/=)
    (NotEqualsV2 _) -> fail $ show' v ++ " is not defined."
    (In _) -> fail $ show' v ++ " is not defined."
    (Is _) -> fail $ show' v ++ " is not defined."
    (IsNot _) -> fail $ show' v ++ " is not defined."
    (NotIn _) -> fail $ show' v ++ " is not defined."
    (BinaryOr _) -> fail $ show' v ++ " is not defined."
    (Xor _) -> fail $ show' v ++ " is not defined."
    (BinaryAnd _) -> fail $ show' v ++ " is not defined."
    (ShiftLeft _) -> fail $ show' v ++ " is not defined."
    (ShiftRight _) -> fail $ show' v ++ " is not defined."
    (Multiply _) -> return '(*)
    (Plus _) -> return '(+)
    (Minus _) -> return '(-)
    (Divide _) -> return '(/)
    (FloorDivide _) -> fail $ show' v ++ " is not defined."
    (MatrixMult _) -> fail $ show' v ++ " is not defined."
    (Invert _) -> fail $ show' v ++ " is not defined."
    (Modulo _) -> fail $ show' v ++ " is not defined."
  return $ TH.InfixE (Just l) (TH.VarE op) (Just r)

-- fromCondition :: (Expr SrcSpan, Suite SrcSpan) -> a -> QN TH.Exp
-- fromCondition (cond, [(Return (Just ret) _)]) a = do
--   cond' <- fromExpression cond
--   ret' <- fromExpression ret
--   return $ TH.CondE cond' ret'

fromConditions :: [(Expr SrcSpan, Suite SrcSpan)] -> Suite SrcSpan -> QN TH.Exp
fromConditions ((cond, ret : _) : conds) final_ret = do
  cond' <- fromExpression cond
  ret' <- fromBody ret
  v <- fromConditions conds final_ret
  return $ TH.CondE cond' ret' v
fromConditions [] (ret'' : _) = do
  fromBody ret''
fromConditions a v = fail $ "fromBody: condition=" ++ show' a ++ " return=" ++ show' v ++ " is not defined."

fromBody :: Statement SrcSpan -> QN TH.Exp
fromBody (Return (Just ret) _) = do
  fromExpression ret
fromBody (Conditional conds else' _) = do
  fromConditions conds else'
fromBody v@(Import _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(FromImport _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(While _ _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(For _ _ _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(AsyncFor _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Fun _ _ _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(AsyncFun _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Class _ _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Assign _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(AugmentedAssign _ _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(AnnotatedAssign _ _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Decorated _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Try _ _ _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Raise _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(With _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(AsyncWith _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Pass _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Break _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Continue _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Delete _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(StmtExpr _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Global _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(NonLocal _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Assert _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Print _ _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Exec _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Return Nothing (SpanCoLinear _ _ _ _)) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Return Nothing (SpanMultiLine _ _ _ _ _)) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Return Nothing (SpanPoint _ _ _)) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Return Nothing SpanEmpty) = fail $ "fromBody:" ++ show' v ++ "is not defined."

fromBodys :: [Statement SrcSpan] -> QN [TH.Exp]
fromBodys = mapM fromBody

withName :: String -> (TH.Name -> QN a) -> QN a
withName name fn = do
  n <- addVar name
  v <- fn n
  delVar name
  return v

fromParam :: Parameter SrcSpan -> QN TH.Pat
fromParam param = do
  n <- addVar (ident_string (param_name param))
  return $ TH.VarP n

withParams :: [Parameter SrcSpan] -> ([TH.Pat] -> QN a) -> QN a
withParams args fn = do
  let names = map (ident_string . param_name) args
  params <- mapM fromParam args
  v <- fn params
  mapM_ delVar names
  return v

type2type :: Expr SrcSpan -> QN TH.Name
type2type (Var (Ident "int" _) _) = return ''Int
type2type (Var (Ident "str" _) _) = return ''String
type2type (Var (Ident "float" _) _) = return ''Double
type2type v = fail $ "type2type: " ++ show' v ++ " is not defined."

types2types :: [Expr SrcSpan] -> QN [TH.Name]
types2types = mapM type2type

fromStatement :: Statement SrcSpan -> QN [TH.Dec]
fromStatement (Fun name args rettype (body : _) _) = do
  let str = ident_string name
      pretypes = map param_py_annotation args
  n <- addVar str
  if all isJust (pretypes ++ [rettype])
    then do
      types <- types2types (map fromJust pretypes)
      rtype <- type2type (fromJust rettype)
      let arrow a b = TH.AppT (TH.AppT TH.ArrowT a) b
      withParams args $ \params -> do
        expression <- fromBody body
        return
          [ TH.SigD n (foldr arrow (TH.ConT rtype) (map TH.ConT types)),
            TH.FunD n [TH.Clause params (TH.NormalB expression) []]
          ]
    else do
      withParams args $ \params -> do
        expression <- fromBody body
        return [TH.FunD n [TH.Clause params (TH.NormalB expression) []]]
fromStatement (StmtExpr sts _) = do
  s <- fromExpression sts
  addExpr s
  return []
fromStatement (Assign variables' value _) = do
  case variables' of
    (Var name _) : [] -> do
      n <- addVar (ident_string name)
      expression <- fromExpression value
      return [TH.ValD (TH.VarP n) (TH.NormalB expression) []]
    _ -> fail $ "fromStatement:" ++ show' variables' ++ "is not defined."
fromStatement (Class name _ body _) = do
  class_name' <- addVar (ident_string name)
  let filterAnn [] = []
      filterAnn ((AnnotatedAssign type' (Var name' _) _ _) : other) = (type', name') : filterAnn other
      filterAnn (_ : other) = filterAnn other
  vars' <- forM (filterAnn body) $ \(type', name') -> do
    n <- addVar (ident_string name')
    t <- type2type type'
    return (n, TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, TH.ConT t)
  return
    [ TH.DataD
        []
        class_name'
        []
        Nothing
        [TH.RecC class_name' vars']
        [ TH.DerivClause Nothing [TH.ConT ''Show, TH.ConT ''Eq]
        ]
    ]
fromStatement a = do
  fail $ "Error:" ++ show' a

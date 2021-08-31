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
-- import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
-- import qualified GHC.Types
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Language.Haskell.TH as TH
import Language.Python.Common
import Text.Pretty.Simple (pShow)
-- import Text.Pretty.Simple (pPrint)

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
--  pPrint decs
  TH.LitE (TH.StringL str_ast) <- TH.stringE (TH.pprint decs)
  let txt = T.pack str_ast
  return $
    T.replace "GHC.Num." "" $
    T.replace "GHC.Classes." "" $
    T.replace "GHC.Types." "" $
    T.replace "GHC.Maybe." "" $
    T.replace "GHC.Real." "" $
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
fromExpression (None _) = do
  return $ TH.ConE '()
fromExpression v@(Ellipsis _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(ByteStrings _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression (Strings v _) = do
  return $ TH.LitE (TH.StringL (concat v))
fromExpression v@(UnicodeStrings _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(Subscript _ _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(SlicedExpr _ _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(CondExpr _ _ _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression (UnaryOp (Minus _) v _) = TH.AppE (TH.VarE 'negate) <$> fromExpression v
fromExpression v@(UnaryOp _ _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression (Dot v' fn _) = do
  fn' <- getVar (ident_string fn)
  case fn' of
    Just fn'' -> do
      TH.AppE <$> (return (TH.VarE fn'')) <*> fromExpression v'
    Nothing -> do
      n' <- addVar (ident_string fn)
      TH.AppE <$> (return (TH.VarE n')) <*> fromExpression v'
      -- fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(Lambda _ _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression (Tuple exprs _) = do
  exprs' <- mapM fromExpression exprs
  return $ TH.TupE (map Just exprs')
fromExpression v@(Yield _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(Generator _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(Await _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression v@(ListComp _ _) = fail $ "fromExpression: " ++ show' v ++ "is not defined "
fromExpression (List exprs _) = do
  exprs' <- mapM fromExpression exprs
  return $ TH.ListE exprs'
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
    Nothing -> do
      --fail $ "Cannot find '" ++ name ++ "'."
      n' <- addVar name
      return $ TH.VarE n'
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
fromConditions ((cond, ret) : conds) final_ret = do
  cond' <- fromExpression cond
  ret' <- fromBodies ret
  v <- fromConditions conds final_ret
  return $ TH.CondE cond' ret' v
fromConditions [] ret'' = do
  fromBodies ret''
-- fromConditions a v = fail $ "fromBody: condition=" ++ show' a ++ " return=" ++ show' v ++ " is not defined."

data Body
  = Ret TH.Exp
  | Let (TH.Exp->TH.Exp)

fromBody :: Statement SrcSpan -> QN Body
fromBody (Return (Just ret) _) = do
  Ret <$> fromExpression ret
fromBody (Conditional conds else' _) = do
  Ret <$> fromConditions conds else'
fromBody v@(Import _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(FromImport _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(While _ _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(For _ _ _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(AsyncFor _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Fun _ _ _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(AsyncFun _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody v@(Class _ _ _ _) = fail $ "fromBody:" ++ show' v ++ "is not defined."
fromBody (Assign variables' value _) = do
  case variables' of
    (Var name _) : [] -> do
      n <- addVar (ident_string name)
      expression <- fromExpression value
      return $ Let $ \expr -> TH.LetE [TH.ValD (TH.VarP n) (TH.NormalB expression) []] expr
    (Tuple exprs _) : [] -> do
      vars <- forM exprs $ \(Var name _) -> do
        n <- addVar (ident_string name)
        return $ TH.VarP n
      expression <- fromExpression value
      return $ Let $ \expr -> TH.LetE [TH.ValD (TH.TupP vars) (TH.NormalB expression) []] expr
    _ -> fail $ "fromBody:" ++ show' variables' ++ "is not defined."
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

fromBodies :: [Statement SrcSpan] -> QN TH.Exp
fromBodies bodies = mapM fromBody bodies >>= bodies2body 

bodies2body :: [Body] -> QN TH.Exp
bodies2body [] = return $ TH.ConE '()
bodies2body ((Let fn):other) = do
  vv <- bodies2body other
  return $ fn vv
bodies2body ((Ret v):_) = return v
  

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

type2type :: Expr SrcSpan -> QN TH.Type
type2type (Var (Ident "int" _) _) = return $ TH.ConT ''Int
type2type (Var (Ident "str" _) _) = return $ TH.ConT ''String
type2type (Var (Ident "float" _) _) = return $ TH.ConT ''Double
type2type (Var (Ident "bool" _) _) = return $ TH.ConT ''Bool
type2type (Var (Ident "Optional" _) _) = return $ TH.ConT ''Maybe
type2type (Var (Ident "List" _) _) = return TH.ListT
-- type2type (Var (Ident "Tuple" _) _) = return TH.TupleT
type2type (Tuple exprs _) = do
  let len = length exprs
  exprs' <- mapM type2type exprs
  return $ foldl TH.AppT (TH.TupleT len) exprs'
type2type (Var (Ident v _) _) = do
  vv <- getVar v
  case vv of
    Just v' -> return $ TH.ConT v'
    Nothing -> do
      -- Should output a warning of a isolated type
      n <- addVar v
      return $ TH.ConT n
type2type (Subscript (Var (Ident "Tuple" _) _) v@(Tuple _ _) _) = do
  type2type v
type2type (Subscript var0 var1 _) = TH.AppT <$> type2type var0 <*> type2type var1
type2type v@(Dot _ _ _) = do
  let loop :: Expr SrcSpan -> String
      loop (Var ident _) = ident_string ident
      loop (Dot v0 v1 _) = loop v0 ++ "." ++ ident_string v1
      loop _ = ""
      str = loop v
  vv <- getVar str
  case vv of
    Just v' -> return $ TH.ConT v'
    Nothing -> do
      -- Should output a warning of a isolated type
      n <- addVar str
      return $ TH.ConT n
type2type v = fail $ "type2type: " ++ show' v ++ " is not defined."

types2types :: [Expr SrcSpan] -> QN [TH.Type]
types2types = mapM type2type

fromStatement :: Statement SrcSpan -> QN [TH.Dec]
fromStatement (Fun name args rettype bodies _) = do
  let str = ident_string name
      pretypes = map param_py_annotation args
  n <- addVar str
  if all isJust (pretypes ++ [rettype])
    then do
      types <- types2types (map fromJust pretypes)
      rtype <- type2type (fromJust rettype)
      let arrow a b = TH.AppT (TH.AppT TH.ArrowT a) b
      withParams args $ \params -> do
        expression <- fromBodies bodies
        return
          [ TH.SigD n (foldr arrow rtype types),
            TH.FunD n [TH.Clause params (TH.NormalB expression) []]
          ]
    else do
      withParams args $ \params -> do
        expression <- fromBodies bodies
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
    (Tuple exprs _) : [] -> do
      vars <- forM exprs $ \(Var name _) -> do
        n <- addVar (ident_string name)
        return $ TH.VarP n
      expression <- fromExpression value
      return [TH.ValD (TH.TupP vars) (TH.NormalB expression) []]
    _ -> fail $ "fromStatement:" ++ show' variables' ++ "is not defined."
fromStatement (Class name _ body _) = do
  class_name' <- addVar (ident_string name)
  let filterAnn [] = []
      filterAnn ((AnnotatedAssign type' (Var name' _) _ _) : other) = (type', name') : filterAnn other
      filterAnn (_ : other) = filterAnn other
  vars' <- forM (filterAnn body) $ \(type', name') -> do
    n <- addVar (ident_string name')
    t <- type2type type'
    return (n, TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, t)
  let dat = [ TH.DataD
              []
              class_name'
              []
              Nothing
              [TH.RecC class_name' vars']
              [ TH.DerivClause Nothing [TH.ConT ''Show, TH.ConT ''Eq]
              ]
            ]
  let filterFun [] = []
      filterFun (f@(Fun _ _  _ _ _) : other) = f : filterFun other
      filterFun (_ : other) = filterFun other
--   liftIO $ print $ filterFun body
  funcs <- mapM fromStatement $ filterFun body
  return $ dat ++ concat funcs

fromStatement a = do
  fail $ "Error:" ++ show' a

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Instant.Typings.Checker where

import Instant.Errors.Errors
import Instant.Errors.Def
import Instant.Typings.Def
import qualified Instant.Syntax as AST

import Data.Functor
import Data.Foldable
import qualified Data.Map as M
import Instant.Logs
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import Control.Applicative hiding (empty)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens

import Prelude hiding (LT, GT, EQ, (<>))

optimizeBody :: [AST.Arg] -> AST.Stmt 'AST.Typed -> AST.Stmt 'AST.Typed
optimizeBody _ s = s

raiseError :: Error -> Typechecker a
raiseError e = view loc >>= \a -> throwError $ ErrorPack $ pure (a, e)


raiseErrorAt :: MonadError ErrorPack m => AST.ASTMeta -> Error -> m a
raiseErrorAt a e = throwError $ ErrorPack $ pure (Just a, e)


raiseErrorNoLoc :: MonadError ErrorPack m => Error -> m a
raiseErrorNoLoc e = throwError $ ErrorPack $ pure (Nothing, e)


withASTMeta :: AST.ASTMeta -> Typechecker a -> Typechecker a
withASTMeta a = local (set loc (Just a))

withASTMetaOf :: AST.HasAnn h AST.ASTMeta => h -> Typechecker a -> Typechecker a
withASTMetaOf h = withASTMeta (h^.AST.ann)


getClassEntry :: AST.ClassId -> Typechecker ClassEntry
getClassEntry i = views classEnv (M.lookup i) >>= \case
  Nothing -> raiseError $ UndefinedClass i
  Just c -> pure c


tcVar :: AST.VarId -> Typechecker AST.Type
tcVar v = (M.lookup v) <$> view definedVars >>= \case
  Nothing | v == "this" -> do
              view currentClass >>= \case
                Nothing -> raiseError ThisNotInClass
                Just c -> pure $ AST.TClass c
  Nothing -> raiseError $ UndefinedVar v
  Just t -> pure t


tcFun :: AST.FunId -> Typechecker (AST.Type, [AST.Type])
tcFun v = (M.lookup v) <$> view definedFuns >>= \case
  Nothing -> raiseError $ UndefinedFun v
  Just t -> pure t


checkAccess :: AST.HasIdStr a String => AST.ClassMemberAccess -> a -> AST.ClassId -> Typechecker ()
checkAccess a x c = do
  myC <- view currentClass
  case a of
    AST.Public -> pure ()
    AST.Private -> when (myC /= Just c) $ raiseError $ BadPrivateAccess c (x^.AST.idStr)
    AST.Protected -> do
      maybe (pure False) (isSuper c) myC >>= \case
        False -> raiseError $ BadProtectedAccess c (x^.AST.idStr)
        True -> pure ()


tcField :: AST.ClassId -> AST.FieldId -> Typechecker AST.Type
tcField c m =
  let search sc = do
        ce <- getClassEntry sc
        case M.lookup m (ce^.fields) of
          Nothing -> case ce^.super of
            Nothing -> raiseError $ UndefinedField c m
            Just x -> search x
          Just (acc, x) -> do
            checkAccess acc m sc
            pure x
  in search c


tcMethod :: AST.ClassId -> AST.MethodId -> Typechecker (AST.Type, [AST.Type])
tcMethod c m =
  let search sc = do
        ce <- getClassEntry sc
        case M.lookup m (ce^.methods) of
          Nothing -> case ce^.super of
            Nothing -> raiseError $ UndefinedMethod c m
            Just x -> search x
          Just (acc, x) -> do
            checkAccess acc m sc
            pure x
  in search c


tcConstructor :: AST.ClassId -> Maybe AST.ConstructorId -> Typechecker [AST.Type]
tcConstructor c m = do
  ce <- getClassEntry c
  case M.lookup m (ce^.constructors) of
    Nothing -> raiseError $ UndefinedConstructor c m
    Just (_, ts) -> pure ts


matchTypes :: AST.Type -> AST.Type -> Typechecker ()
matchTypes want got = case (want, got) of
  (AST.TInt, AST.TInt) -> pure ()
  (AST.TVoid, AST.TVoid) -> pure ()
  (AST.TBool, AST.TBool) -> pure ()
  (AST.TString, AST.TString) -> pure ()
  (AST.TClass o1, AST.TClass o2) -> matchClass o1 o2
  _ -> raiseError $ TypeMatch want got


isSuper :: AST.ClassId -> AST.ClassId -> Typechecker Bool
isSuper cs c =
  let search i =
        if cs == i then pure True
        else getClassEntry i >>= \ce -> case ce^.super of
          Nothing -> pure False
          Just i' -> search i'
  in search c


matchClass :: AST.ClassId -> AST.ClassId -> Typechecker ()
matchClass c1 c2 = isSuper c1 c2 >>= \case
  True -> pure ()
  False -> raiseError $ ClassMatch c1 c2


getSuper :: Typechecker AST.ClassId
getSuper = do
  view currentClass >>= \case
    Nothing -> raiseError $ SuperNotInClass
    Just c -> getClassEntry c >>= \ce -> case ce^.super of
      Nothing -> raiseError $ NoSuperClass c
      Just x -> return x


assertType :: AST.Type -> AST.Expr 'AST.Typed -> Typechecker ()
assertType t e = matchTypes t (AST.getExprDec e)


tcType :: AST.Type -> Typechecker ()
tcType = \case
  AST.TClass cls -> do
    isDefined <- views classEnv (M.member cls)
    when (not isDefined) $ raiseError $ UndefinedClass cls
  _ -> return ()


tcOp :: AST.AnyOp -> AST.Type -> AST.Type -> Typechecker AST.Type
tcOp o l r =
  let tc t = matchTypes t r >> matchTypes t l
  in case o of
    AST.Op (AST.LT a)    -> withASTMeta a $ tc AST.TInt $> AST.TBool
    AST.Op (AST.LEQ a)   -> withASTMeta a $ tc AST.TInt $> AST.TBool
    AST.Op (AST.EQ a)    -> withASTMeta a $ matchTypes l r $> AST.TBool
    AST.Op (AST.NEQ a)   -> withASTMeta a $ matchTypes l r $> AST.TBool
    AST.Op (AST.GEQ a)   -> withASTMeta a $ tc AST.TInt $> AST.TBool
    AST.Op (AST.GT a)    -> withASTMeta a $ tc AST.TInt $> AST.TBool
    AST.Op (AST.Plus a)  -> withASTMeta a $ case (l, r) of
      (AST.TInt, AST.TInt) -> pure AST.TInt
      (AST.TString, AST.TString) -> pure AST.TString
      _ -> raiseError (OperatorTypeMatch o [(AST.TInt, AST.TInt), (AST.TString, AST.TString)] (l, r))
    AST.Op (AST.Minus a) -> withASTMeta a $ tc AST.TInt $> AST.TInt
    AST.Op (AST.Mult a)  -> withASTMeta a $ tc AST.TInt $> AST.TInt
    AST.Op (AST.Div a)   -> withASTMeta a $ tc AST.TInt $> AST.TInt
    AST.Op (AST.Mod a)   -> withASTMeta a $ tc AST.TInt $> AST.TInt
    AST.Op (AST.Or a)    -> withASTMeta a $ tc AST.TBool $> AST.TBool
    AST.Op (AST.And a)   -> withASTMeta a $ tc AST.TBool $> AST.TBool


tcUnOp :: AST.UnOp -> AST.Type -> Typechecker AST.Type
tcUnOp o t = case o of
  AST.Neg -> matchTypes AST.TInt t >> return AST.TInt
  AST.Not -> matchTypes AST.TBool t >> return AST.TBool


tcExpr :: AST.Expr 'AST.Untyped -> Typechecker (AST.Expr 'AST.Typed)
tcExpr = \case
  AST.ELit a () l -> withASTMeta a $ case l of
    AST.LInt i -> pure $ AST.ELit a AST.TInt (AST.LInt i)
    AST.LString s -> pure $ AST.ELit a AST.TString (AST.LString s)
    AST.LBool b -> pure $ AST.ELit a AST.TBool (AST.LBool b)
  AST.EVar a () v -> withASTMeta a $ tcVar v >>= \t -> pure $ AST.EVar a t v
  AST.EApp a () fname as -> withASTMeta a $ do
    (rett, argst) <- tcFun fname
    typedArgs <- mapM tcExpr as
    when (length as /= length argst) $
      raiseError $ ArgNumFun fname (length argst) (length as)
    forM_ (zip argst typedArgs) $ \(expected, typed) ->
      assertType expected typed
    pure $ AST.EApp a rett fname typedArgs
  AST.EUnOp a () o v -> withASTMeta a $ do
    vt <- tcExpr v
    rt <- tcUnOp o (AST.getExprDec vt)
    pure $ AST.EUnOp a rt o vt
  AST.EOp a () o l r -> withASTMeta a $ do
    tl <- tcExpr l
    tr <- tcExpr r
    tres <- tcOp o (AST.getExprDec tl) (AST.getExprDec tr)
    pure $ AST.EOp a tres o tl tr
  AST.EProj apr () (AST.ESuper a ()) i -> withASTMeta a $ do
    s <- getSuper
    t <- tcField s i
    pure $ AST.EProj apr t (AST.ESuper a (AST.TClass s)) i
  AST.EProj a () e i -> withASTMeta a $ do
    et <- tcExpr e
    case AST.getExprDec et of
      AST.TClass c -> do
        t <- tcField c i
        pure $ AST.EProj a t et i
      t -> raiseError $ NotAClass t
  AST.EMApp apr () (AST.ESuper a ()) i as -> withASTMeta a $ do
    s <- getSuper
    (rt, argst) <- tcMethod s i
    typedArgs <- mapM tcExpr as
    when (length as /= length argst) $
      raiseError $ ArgNumMethod s i (length argst) (length as)
    forM_ (zip argst typedArgs) $ \(expected, typed) ->
      assertType expected typed
    pure $ AST.EMApp apr rt (AST.ESuper a (AST.TClass s)) i typedArgs
  AST.EMApp a () e i as -> withASTMeta a $ do
    et <- tcExpr e
    case AST.getExprDec et of
      AST.TClass c -> do
        (rt, argst) <- tcMethod c i
        typedArgs <- mapM tcExpr as
        when (length as /= length argst) $
          raiseError $ ArgNumMethod c i (length argst) (length as)
        forM_ (zip argst typedArgs) $ \(expected, typed) ->
          assertType expected typed
        pure $ AST.EMApp a rt et i typedArgs
      t -> raiseError $ NotAClass t
  AST.ENew a () c i as -> withASTMeta a $ do
    argst <- tcConstructor c i
    typedArgs <- mapM tcExpr as
    when (length as /= length argst) $
      raiseError $ ArgNumConstructor c i (length argst) (length as)
    forM_ (zip argst typedArgs) $ \(expected, typed) ->
      assertType expected typed
    pure $ AST.ENew a (AST.TClass c) c i typedArgs
  AST.ESuper a () -> withASTMeta a $ raiseError BadSuperUse


currentRetType :: Typechecker AST.Type
currentRetType = maybe (error "fun env not in a funtion") id <$>
  view retType


newScope :: Typechecker a -> Typechecker a
newScope = local (set currentScopeVars S.empty)


tcStmt :: AST.Stmt 'AST.Untyped -> Typechecker (AST.Stmt 'AST.Typed)
tcStmt = \case
  AST.SDecl a t v k -> withASTMeta a $ do
    tcType t
    vars <- view currentScopeVars
    void $ flip runStateT vars $ forM_ v $ \(var, _) -> do
      myvars <- get
      when (S.member var myvars) (lift $ raiseError $ DuplicateVar var)
      modify (S.insert var)
    oldenv <- view definedVars
    (newEnv, tv) <-
      foldrM ( \(var, mval) (penv, pdecls) -> do
                 case mval of
                   Nothing -> return (M.insert var t penv, (var, Nothing):pdecls)
                   Just e -> do
                     et <- tcExpr e
                     assertType t et
                     return (M.insert var t penv, (var, Just et):pdecls)
             ) (oldenv, []) v
    local ( set definedVars newEnv .
            over currentScopeVars (S.union $ S.fromList (fmap fst v))) $
      AST.SDecl a t tv <$> tcStmt k
  AST.SAssg a v e k -> withASTMeta a $ do
    et <- tcExpr e
    vt <- tcVar v
    assertType vt et
    AST.SAssg a v et <$> tcStmt k
  AST.SFieldAssg a b f e k -> withASTMeta a $ do
    bt <- tcExpr b
    et <- tcExpr e
    case AST.getExprDec bt of
      AST.TClass c -> do
        t <- tcField c f
        assertType t et
        AST.SFieldAssg a bt f et <$> tcStmt k
      t -> raiseError $ NotAClass t
  AST.SIncr a v k -> withASTMeta a $ do
    vt <- tcVar v
    matchTypes AST.TInt vt
    AST.SIncr a v <$> tcStmt k
  AST.SDecr a v k -> withASTMeta a $ do
    vt <- tcVar v
    matchTypes AST.TInt vt
    AST.SDecr a v <$> tcStmt k
  AST.SCond a c t k -> withASTMeta a $ do
    ct <- tcExpr c
    assertType AST.TBool ct
    tt <- newScope $ tcStmt t
    AST.SCond a ct tt <$> tcStmt k
  AST.SCondElse a c t e k -> withASTMeta a $ do
    ct <- tcExpr c
    assertType AST.TBool ct
    tt <- newScope $ tcStmt t
    et <- newScope $ tcStmt e
    AST.SCondElse a ct tt et <$> tcStmt k
  AST.SWhile a c b k -> withASTMeta a $ do
    ct <- tcExpr c
    assertType AST.TBool ct
    bt <- newScope $ tcStmt b
    AST.SWhile a ct bt <$> tcStmt k
  AST.SExp a e k -> withASTMeta a $ do
    et <- tcExpr e
    AST.SExp a et <$> tcStmt k
  AST.SRet a e k -> withASTMeta a $ do
    et <- tcExpr e
    rt <- currentRetType
    assertType rt et
    AST.SRet a et <$> tcStmt k
  AST.SVRet a k -> withASTMeta a $ do
    rt <- currentRetType
    matchTypes rt AST.TVoid
    AST.SVRet a <$> tcStmt k
  AST.SBlock a b k -> withASTMeta a $
    AST.SBlock a <$> (newScope $ tcStmt b) <*> tcStmt k
  AST.SEmpty a -> withASTMeta a $ pure $ AST.SEmpty a


isReturning :: AST.Stmt a -> Bool
isReturning = \case
  AST.SDecl _a _t _decls k -> isReturning k
  AST.SAssg _a _v _e k -> isReturning k
  AST.SFieldAssg _a _ee _v _e k -> isReturning k
  AST.SIncr _a _v k -> isReturning k
  AST.SDecr _a _v k -> isReturning k
  AST.SCond _a _c _t k -> isReturning k
  AST.SCondElse _a _c t e k ->
    (isReturning t && isReturning e) || isReturning k
  AST.SWhile _a (AST.ELit _ _ (AST.LBool True)) b _k -> isReturning b
  AST.SWhile _a _c _b k -> isReturning k
  AST.SExp _a (AST.EApp _ _ (AST.FunId "error") _) _k -> True
  AST.SExp _a _e k -> isReturning k
  AST.SRet _a _e _ -> True
  AST.SVRet _a _ -> True
  AST.SBlock _a b k ->
    isReturning b || isReturning k
  AST.SEmpty _ -> False


buildClassEntry :: AST.ClassDef 'AST.Untyped -> ClassEntry
buildClassEntry c = ClassEntry
  { _ceSuper = c ^. AST.super
  , _ceFields = M.fromList
                [(i, t) | (AST.CMField f) <- c^.AST.body
                        , let t = (f^.AST.access, f^.AST.ty)
                        , (i, _) <- NE.toList (f^.AST.assignments)
                        ]
  , _ceMethods = M.fromList
                 [(i, (acc, (rt, as))) | (AST.CMMethod m) <- c^.AST.body
                          , let as = fmap (^.AST.ty) $ m^.AST.args
                                rt = m^.AST.retType
                                i = m^.AST.name
                                acc = m^.AST.access
                          ]
  , _ceConstructors = M.fromList
    [(i, (acc, as)) | (AST.CMConstructor co) <- c^.AST.body
             , let as = fmap (^.AST.ty) $ co^.AST.args
                   i = co^.AST.name
                   acc = co^.AST.access
             ]
  }


buildInitialEnv :: [AST.TopDef 'AST.Untyped] -> TopTypechecker TypecheckerEnv
buildInitialEnv defs = do
  --when (null [() | AST.TDFun fdef <- defs, fdef^.AST.name == "main"]) $
  --  raiseErrorNoLoc NoMain
  env <- foldM (\prev d ->
           case d of
             AST.TDFun fdef -> do
               when (fdef^.AST.name == "main" && (fdef^.AST.retType /= AST.TInt || not (null $ fdef^.AST.args))) $
                 raiseErrorAt (fdef^.AST.meta) MainType
               when (fdef^.AST.name `elem` (M.keys $ prev^.definedFuns)) $
                 raiseErrorAt (fdef^.AST.meta) $ DuplicateFun (fdef^.AST.name)
               pure $
                 over definedFuns (M.insert (fdef^.AST.name) (fdef^.AST.retType, fmap (^.AST.ty) (fdef^.AST.args))) prev
             AST.TDClass cd -> do
               when (cd^.AST.name `elem` (M.keys $ prev^.classEnv)) $
                 raiseErrorAt (cd^.AST.meta) $ DuplicateClass (cd^.AST.name)
               let ce = buildClassEntry cd
                   checkDups :: (Ord s)
                             => [(AST.ASTMeta, x)] -> (x -> s) -> (s -> Error) -> TopTypechecker ()
                   checkDups vals getname err = flip evalStateT S.empty $ forM_ vals $ \(a, f) -> do
                     jeb <- gets $ S.member (getname f)
                     when jeb $ raiseErrorAt a $ err (getname f)
                     modify $ S.insert (getname f)
               checkDups [(fd^.AST.meta, f) | AST.CMField fd <- cd^.AST.body, (f, _) <- NE.toList $ fd^.AST.assignments ] id (DuplicateField (cd^.AST.name))
               checkDups [(m^.AST.meta, m) | AST.CMMethod m <- cd^.AST.body ] (^.AST.name) (DuplicateMethod (cd^.AST.name))
               checkDups [(c^.AST.meta, c) | AST.CMConstructor c <- cd^.AST.body ] (^.AST.name) (DuplicateConstructor (cd^.AST.name))
               pure $ over classEnv (M.insert (cd^.AST.name) ce) prev
        ) initialEnv defs
  forM_ [cd | AST.TDClass cd <- defs] $ \cd ->
    forM_ (cd^.AST.super) $ \sup ->
      when (not $ M.member sup (env^.classEnv))
        (raiseErrorAt (cd^.AST.meta) (UndefinedClass sup))
  return env


tcTopDef :: AST.TopDef 'AST.Untyped -> Typechecker (AST.TopDef 'AST.Typed)
tcTopDef = \case
  AST.TDFun fdef -> do
    tcType (fdef^.AST.retType)
    forM_ (fmap (^.AST.ty) $ fdef^.AST.args) tcType
    let addArgEnv =
          flip M.union (M.fromList $ fmap (\a -> (a^.AST.name, a^.AST.ty)) (fdef^.AST.args))
    let bodyEnv = (over definedVars addArgEnv . set retType (Just $ fdef^.AST.retType))
    tbody <- do
      tb <- optimizeBody (fdef^.AST.args) <$> local bodyEnv (tcStmt $ fdef^.AST.body)
      when (not $ (fdef^.AST.retType == AST.TVoid) || (fdef^.AST.name=="main") || isReturning tb) $
        raiseErrorAt (fdef^.AST.meta) NoReturn
      return tb
    pure $ AST.TDFun $ AST.FunDef (fdef^.AST.meta) (fdef^.AST.retType) (fdef^.AST.name) (fdef^.AST.args) tbody
  AST.TDClass cdef -> do
    let this = AST.Arg { AST._argAnn = AST.emptyMeta, AST._argName = "this", AST._argTy = AST.TClass (cdef^.AST.name) }

        tcMember :: AST.ClassMember 'AST.Untyped -> Typechecker (AST.ClassMember 'AST.Typed)
        tcMember = \case
          AST.CMMethod mdef -> do
            tcType $ mdef^.AST.retType
            forM_ (fmap (^.AST.ty) $ mdef^.AST.args) tcType
            let addArgEnv =
                  flip M.union (M.fromList $ fmap (\a -> (a^.AST.name, a^.AST.ty)) (mdef^.AST.args))
            let setBodyEnv = over definedVars addArgEnv .
                             set retType (Just $ mdef^.AST.retType) .
                             set currentScopeName (Just $ mdef^.AST.name.AST.idStr) .
                             set currentClass (Just $ cdef^.AST.name)
            tbody <- case mdef^.AST.body of
              Nothing -> pure Nothing
              Just b -> Just <$>
                do tb <- optimizeBody (this : mdef^.AST.args) <$> local setBodyEnv (tcStmt b)
                   when (not $ (mdef^.AST.retType == AST.TVoid) || isReturning tb) $
                     raiseErrorAt (mdef^.AST.meta) NoReturn
                   return tb
            pure $ AST.CMMethod $ AST.Method
              { AST._methodMeta = mdef^.AST.meta
              , AST._methodAccess = mdef^.AST.access
              , AST._methodPlace = mdef^.AST.place
              , AST._methodRetType = mdef^.AST.retType
              , AST._methodName = mdef^.AST.name
              , AST._methodArgs = mdef^.AST.args
              , AST._methodBody = tbody
              }
          AST.CMField fdef -> do
            tcType $ fdef^.AST.ty
            tassgs <- forM (fdef^.AST.assignments) $ \(i, mv) -> do
              case mv of
                Nothing -> return (i, Nothing)
                Just v -> do
                  let setBodyEnv = set currentClass (Just $ cdef^.AST.name)
                  vt <- local setBodyEnv (tcExpr v)
                  assertType (fdef^.AST.ty) vt
                  return (i, Just vt)
            pure $ AST.CMField $ AST.Field
              { AST._fieldMeta = fdef^.AST.meta
              , AST._fieldAccess = fdef^.AST.access
              , AST._fieldPlace = fdef^.AST.place
              , AST._fieldTy = fdef^.AST.ty
              , AST._fieldAssignments = tassgs
              }
          AST.CMConstructor codef -> do
            forM_ (codef^.AST.args) $ \a -> withASTMetaOf a $ do
              when (a^.AST.name == "this") $
                raiseError $ ThisArgName
              tcType (a^.AST.ty)
            let addArgEnv =
                  flip M.union (M.fromList $ fmap (\a -> (a^.AST.name, a^.AST.ty)) (codef^.AST.args))
            let setBodyEnv =
                  over definedVars addArgEnv .
                  set retType (Just $ AST.TClass (cdef^.AST.name)) .
                  set currentScopeName (fmap (^.AST.idStr) (codef^.AST.name) <|> Just "unnamed constructor") .
                  set currentClass (Just $ cdef^.AST.name)
            tbody <- optimizeBody (this : codef^.AST.args) <$>
                     local setBodyEnv (tcStmt (codef^.AST.body))
            pure $ AST.CMConstructor $ AST.Constructor
              { AST._constructorMeta    = codef^.AST.meta
              , AST._constructorAccess = codef^.AST.access
              , AST._constructorName   = codef^.AST.name
              , AST._constructorArgs   = codef^.AST.args
              , AST._constructorBody   = tbody
              }
    members <- mapM tcMember (cdef^.AST.body)
    pure $ AST.TDClass $ AST.ClassDef (cdef^.AST.meta) (cdef^.AST.name) (cdef^.AST.super) members


tcProgram :: AST.InstantProgram -> Typechecker ([AST.TopDef 'AST.Typed])
tcProgram prog = mapM tcTopDef $ AST.statements prog

typecheckx :: AST.InstantProgram -> Either ErrorPack (ClassEnv, [AST.TopDef 'AST.Typed])
typecheckx p = do
  env <- runExcept $ buildInitialEnv $ AST.statements p
  flip runReader env . runExceptT $ ((,) (env^.classEnv) <$> tcProgram p)

typecheck :: AST.InstantProgram -> InstantPipeline (Either ErrorPack (ClassEnv, [AST.TopDef 'AST.Typed]))
typecheck p = do
    return $ typecheckx p
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reporting.Errors.Messages where

import Typings.Types as Type
import Data.List
import Reporting.Errors.Base
import Reporting.Errors.Def
import Utils.Similarity

import Program.Syntax as Syntax
import Reporting.Errors.Position

import Typings.Debug
import Control.Lens
import Data.Generics.Product


decodeTypeContext :: Reporting.Errors.Def.TypeContext -> Maybe (String, Position, [(String, Maybe Position)])
decodeTypeContext (TypeInFunctionReturn (Syntax.FunctionDef pos tret (Syntax.Ident _ funName) args b)) =
    Just $ ("Function '" ++ funName ++ "' return type", pos, [
        ("Function '" ++ funName ++ "' was defined here", Just $ pos)
    ])
decodeTypeContext (TypeInClassParent (Syntax.ClassDef pos (Syntax.Ident namePos className) (Syntax.Name parentPos _) decls)) =
    Just $ ("Parent class of '" ++ className ++ "'", parentPos, [
        ("Class '" ++ className ++ "' was defined here", Just $ namePos)
    ])
decodeTypeContext (TypeInClassField (Syntax.FieldDecl pos t id)) =
    Just $ (
        "Class field '" ++ printi 0 id ++ "'", pos, []
    )
decodeTypeContext (TypeInMethodReturn (Syntax.MethodDecl pos tret id@(Syntax.Ident _ methodName) args b)) =
    Just $ ("Class method '" ++ methodName ++ "' return type", pos, [
        ("Method '" ++ methodName ++ "' was defined here", Just $ pos)
    ])
decodeTypeContext (TypeInVarDecl (Syntax.NoInit pos id)) = 
    Just $ ("Variable declaration for '" ++ printi 0 id ++ "'", pos, [])
decodeTypeContext (TypeInVarDecl (Syntax.Init pos id e)) = 
    Just $ ("Variable declaration for '" ++ printi 0 id ++ "'", pos, [])
decodeTypeContext (TypeInFunctionArgDecl (Syntax.FunctionDef pos tret id@(Syntax.Ident _ funName) args b) (Syntax.Arg argPos _ argID)) = 
    Just $ ("Function '" ++ funName ++ "' parameter declaration '" ++ printi 0 argID ++ "'", argPos, [
        ("Function '" ++ funName ++ "' was defined here", Just $ pos)
    ])
decodeTypeContext (TypeInMethodArgDecl (Syntax.MethodDecl pos tret id@(Syntax.Ident _ methodName) args b) (Syntax.Arg argPos _ argID)) =
    Just $ ("Class method '" ++ methodName ++ "' parameter declaration '" ++ printi 0 argID ++ "'", argPos, [
        ("Method '" ++ methodName ++ "' was defined here", Just $ pos)
    ])
decodeTypeContext (TypeInCast (Syntax.Cast pos t e)) = 
    Just $ ("Explicit cast expression", pos, [])
decodeTypeContext (TypeInNew (Syntax.NewObj pos t m)) = 
    Just $ ("New object constructor", pos, [])
decodeTypeContext _ = Nothing

decodeError :: Error -> SimpleError
decodeError (UnknownFailure env msg) = SimpleError {
    _errorName = "Unknown fatal error"
    , _errorDescription = "This is a generic case for an error. It should never ever happen in practice. It's likely a problem with compiler itself. Oopsie!\n\n" ++ show env
    , _errorSugestions = [
        "Ask author why this happened???"
    ]
    , _errorLocation = Nothing
    , _errorContexts = [
        ("Details:\n" ++ msg, Nothing)
    ]
    , _errorHelp = Nothing
    , _errorMarkers = NoMarker
}
-- UnknownVariable
decodeError (IllegalTypeUsed env typeContext typeName) = 
    case decodeTypeContext typeContext of
        (Just (msg, pos, contexts)) -> SimpleError {
            _errorName = "Invalid type"
            , _errorDescription = msg ++ " requires usage of valid type. Type that you've used i.e. " ++ printi 0 typeName ++ " is not valid in this context."
            , _errorSugestions = [
                "Change the used type '" ++ printi 0 typeName ++ "' to some other valid type"
            ]
            , _errorLocation = Just $ pos
            , _errorContexts = contexts
            , _errorHelp = Nothing
            , _errorMarkers = NoMarker
        }
        Nothing -> SimpleError {
            _errorName = "Invalid type"
            , _errorDescription = "Type " ++ printi 0 typeName ++ " cannot be used in this context."
            , _errorSugestions = [
                "Change the used type '" ++ printi 0 typeName ++ "' to some other valid type"
            ]
            , _errorLocation = Nothing
            , _errorContexts = []
            , _errorHelp = Nothing
            , _errorMarkers = NoMarker
        }
decodeError (MissingReturnValue env pos expectedReturnType fn) =
    SimpleError {
        _errorName = "Invalid return"
        , _errorDescription = "Used return with no value, where it's illegal.\nFunction '" ++ stringName fn ++ "' expects return value of type " ++ printi 0 expectedReturnType
        , _errorSugestions = [
            "Change '" ++ stringName fn ++ "' return type to void"
            , "Add value to the return statement"
        ]
        , _errorLocation = Just $ pos
        , _errorContexts = [
            ("'" ++ stringName fn ++ "' return type was declared here", Just $ Type.location fn)
        ]
        , _errorHelp = Just $ ("Maybe you wanted to change return type of '" ++ stringName fn ++ "' to void?", Type.location fn)
        , _errorMarkers = NoMarker
    }
decodeError (UnknownType env name) =
    SimpleError {
        _errorName = "Unknown symbol"
        , _errorDescription = "Used unknown type name '" ++ (stringName name) ++ "'"
        , _errorSugestions = [
            "Make sure the naming is correct and there's no misspell"
        ]
        , _errorLocation = Just $ Type.location name
        , _errorContexts = []
        , _errorHelp = Nothing
        , _errorMarkers = NoMarker
    }
decodeError (UnknownVariable env name) = let
        candidates = (env <--? QueryVarExact (stringName name)) ++ []
    in case candidates of
        [] ->
            let fuzzCandidates = env <--? QueryVar (stringName name) in
            case fuzzCandidates of
                [] ->
                    SimpleError {
                        _errorName = "Unknown symbol"
                        , _errorDescription = "Used unknown variable '" ++ (stringName name) ++ "'\nUnfortunately I couldn't find any variables to use as a suggestion."
                        , _errorSugestions = [
                            "Make sure you've used a correct variable and there's no misspelling"
                        ]
                        , _errorLocation = Just $ Type.location name
                        , _errorContexts = []
                        , _errorHelp = Nothing
                        , _errorMarkers = NoMarker
                    }
                l ->
                    SimpleError {
                        _errorName = "Unknown symbol"
                        , _errorDescription = "Used unknown variable '" ++ (stringName name) ++ "'"
                        , _errorSugestions = [
                            "Make sure you've used a correct variable and there's no misspelling"
                        ]
                        , _errorLocation = Just $ Type.location name
                        , _errorContexts = []
                        , _errorHelp = Just ("Maybe that was a typo? Similar variable names in this context are:" ++ (printVars "" $ map (\(_, _, varName, varType) -> (varName, varType)) l), Type.location name)
                        , _errorMarkers = NoMarker
                    }
        l -> let (False, blockPos, varName, varType) = head l in
            SimpleError {
                _errorName = "Unknown symbol"
                , _errorDescription = "Used unknown variable '" ++ (stringName name) ++ "'"
                , _errorSugestions = [
                    "Make sure you've used a correct variable and there's no misspelling"
                ]
                , _errorLocation = Just $ Type.location name
                , _errorContexts = [
                     ("There's variable '" ++ stringName varName ++ "' of type " ++ printi 0 varType ++ " declared in nested scope prior to the usage.", Just $ Type.location varName)
                     , ("The visibility scope of mentioned variable starts here", Just $ blockPos)
                ]
                , _errorHelp = Just $ ("Maybe you want to use variable '" ++ stringName varName ++ "', but the variable was declared in nested scope inaccessible in the place of usage. Did you wanted to move the variable to upper scope?", Type.location varName)
                , _errorMarkers = NoMarker
            }
decodeError (InvalidMainReturn main@(Type.Fun _ retType _ _) _) = SimpleError {
    _errorName = "Invalid entrypoint"
    , _errorDescription = "Main function has invalid return type"
    , _errorSugestions = [
        "Change main() return type to 'int'"
    ]
    , _errorLocation = Just $ Type.location main
    , _errorContexts = []
    , _errorHelp = Just $ ("Change main() return type from '" ++ (printi 0 retType) ++ "' to 'int'", Type.location main)
    , _errorMarkers = NoMarker
}
decodeError (MainHasArgs main@(Type.Fun _ _ _ _) _) = SimpleError {
    _errorName = "Invalid entrypoint"
    , _errorDescription = "Main function cannot accept any parameters"
    , _errorSugestions = [
        "Remove all main() parameters"
    ]
    , _errorLocation = Just $ Type.location main
    , _errorContexts = []
    , _errorHelp = Just $ ("Remove all main parameters", Type.location main)
    , _errorMarkers = NoMarker
}
-- decodeError MainType = SimpleError {
--     _errorName = "Invalid main type"
--     , _errorDescription = "Main function should be defined as int main()"
--     , _errorSugestions = [
--         "Changing return type of main to int"
--     ]
--     , _errorOocontext = Nothing
-- }
--IncompatibleTypesAssign
--Syntax.Init pos id e
decodeError (IncompatibleTypesInit env (Syntax.Init pos id e) rightType leftType) = SimpleError {
    _errorName = "Incompatible type"
    , _errorDescription = "Declaration of variable '" ++ printi 0 id ++ "' has type " ++ printi 0 leftType ++ " which is incompatible with the value that is being assigned."
    , _errorSugestions = []
    , _errorLocation = Just $ pos
    , _errorContexts = [
        ("The value on the right side has different type " ++ printi 0 rightType, Just $ Syntax.getPos e)
    ]
    , _errorHelp = Nothing
    , _errorMarkers = combineMarkers [formatInferenceTrace env, formatFunctionContext env]
}
decodeError (IncompatibleTypesAssign env (Syntax.Assignment pos _ right) rightType leftType) = SimpleError {
    _errorName = "Incompatible type"
    , _errorDescription = "Left side of assignment has type " ++ printi 0 leftType ++ " which is incompatible with the value that is being assigned."
    , _errorSugestions = []
    , _errorLocation = Just $ pos
    , _errorContexts = [
        ("The value on the right side has different type " ++ printi 0 rightType, Just $ Syntax.getPos right)
    ]
    , _errorHelp = Nothing
    , _errorMarkers = combineMarkers [formatInferenceTrace env, formatFunctionContext env]
}
decodeError (IncompatibleTypesReturn env (Syntax.ReturnValue pos _) contextFn actualType expectedType) = SimpleError {
    _errorName = "Incompatible type"
    , _errorDescription = "Return, returns value of type: " ++ printi 0 actualType ++ ". However the parent function should return value of type " ++ printi 0 expectedType
    , _errorSugestions = []
    , _errorLocation = Just $ pos
    , _errorContexts = [
        ("Defintion of function '" ++ stringName contextFn ++ "'", Just $ Type.location contextFn)
    ]
    , _errorHelp = Nothing
    , _errorMarkers = combineMarkers [formatInferenceTrace env, formatFunctionContext env]
}
decodeError (DuplicateFunctionArgument env fn (Syntax.Arg pos argType argID) otherArgs) =
    let argMessages = map (argMessage argType) otherArgs in
    let duplciatesCount = length argMessages  in
    let exactDuplicatesCount = length (filter (== True) $ map snd argMessages)  in
    let typeConflictsCount = duplciatesCount - exactDuplicatesCount in
        SimpleError {
            _errorName = "Duplicate definition"
            , _errorDescription = case (exactDuplicatesCount, typeConflictsCount) of
                (0, c) -> "Function parameter '" ++ printi 0 argID ++ "' of type " ++ printi 0 argType ++ " has conflicting " ++ (show c) ++ " declarations with the same name."
                (c, 0) -> "Function parameter '" ++ printi 0 argID ++ "' of type " ++ printi 0 argType ++ " has " ++ (show c) ++ " exact duplicate declarations."
                _ -> "Function parameter '" ++ printi 0 argID ++ "' of type " ++ printi 0 argType ++ " has conflicting declarations with the same name, but different type and also exact duplicate declarations."
            , _errorSugestions = []
            , _errorLocation = Just $ pos
            , _errorContexts = map fst argMessages
            , _errorHelp = case (exactDuplicatesCount, typeConflictsCount) of
                (0, c) -> Just $ ("Rename parameter '" ++ printi 0 argID ++ "' to something else to prevent conflicts with " ++ (show c) ++ " other variables with the same name, but different types", Type.location argID)
                (c, 0) -> Just $ ("Remove " ++ (show c) ++ " copied duplicate parameter declarations", Type.location argID)
                (_, _) -> Just $ ("Rename parameter '" ++ printi 0 argID ++ "' to something else and remove " ++ (show duplciatesCount) ++ " conflicting parameters with the same name", Type.location argID)
                --Just $ ("Rename parameter '" ++ printi 0 argID ++ "' to something else", Type.location argID)
            , _errorMarkers = NoMarker
        }
    where
        argMessage :: (Syntax.Type Position) -> (Syntax.Arg Position) -> ((String, Maybe Position), Bool)
        argMessage argType (Syntax.Arg _ conflictType conflictID) =
            case similar argType conflictType of
                True -> (("Exact duplicated declaration", Just $ Type.location conflictID), True)
                False -> (("Conflicting parameter declaration for '" ++ printi 0 conflictID ++ "' of type " ++ printi 0 conflictType ++ " ", Just $ Type.location conflictID), False)
decodeError (VariableRedeclared env sourcePos (newName, newType) (oldName, oldType)) = SimpleError {
    _errorName = "Duplicate definition"
    , _errorDescription = "Variable '" ++ stringName newName ++ "' of type " ++ printi 0 newType ++ " has duplicate declaration."
    , _errorSugestions = []
    , _errorLocation = Just $ sourcePos
    , _errorContexts = [
        ("Previous conflicting definition of variable '" ++ stringName oldName ++ "' of type " ++ printi 0 oldType ++ " ", Just $ Type.location oldName)
    ]
    , _errorHelp = Just $ ("Rename new variable to something else", Type.location newName)
    , _errorMarkers = NoMarker
}
decodeError (NoMain env) = let
        candidates = (env <--? QueryFn "main" (Just ((Syntax.IntT Undefined), []))) ++ (env <--? QueryFn "main" Nothing)
    in case candidates of
        [] ->
            SimpleError {
                _errorName = "No entrypoint"
                , _errorDescription = "There is no main function defined."
                , _errorSugestions = [
                    "Defining main function: int main() {}"
                ]
                , _errorLocation = Nothing
                , _errorContexts = []
                , _errorHelp = Nothing
                , _errorMarkers = NoMarker
            }
        firstCandidate:_ ->
            SimpleError {
                _errorName = "No entrypoint"
                , _errorDescription = "There is no main function defined."
                , _errorSugestions = [
                    "Defining main function: int main() {}"
                    , "Trying to fix spelling of '" ++ (stringName firstCandidate) ++ "'"
                ]
                , _errorLocation = Nothing
                , _errorContexts = []
                , _errorHelp = Just $ ("Probably you made misspelling and this function should be called 'main'?", Type.namePosition firstCandidate)
                , _errorMarkers = NoMarker
            }
--InheritanceLinearizationProblem
decodeError (InheritanceLinearizationProblem cls msg) = SimpleError {
    _errorName = "Invalid inheritance"
    , _errorDescription = "There's a problem with C3 linearization of the class inheritance chain\n" ++ msg
    , _errorSugestions = [
        "Check the inheritance chain of the class '" ++ stringName cls ++ "'"
    ]
    , _errorLocation = Nothing
    , _errorContexts = [
        (msg, Just $ Type.extendsPosition cls)
    ]
    , _errorHelp = Nothing
    , _errorMarkers = NoMarker
}
decodeError (CyclicInheritance cls chain msg) = SimpleError {
    _errorName = "Invalid inheritance"
    , _errorDescription = "Classes extends each other forming a cycle\n\n" ++ msg
    , _errorSugestions = [
        "Redefine classes in a such way that no cycle is formed"
    ]
    , _errorLocation = Just $ Type.location cls
    , _errorContexts = map (\other -> ("Class '" ++ (stringName other) ++ "' forming a cycle " ++ (intercalate " <- " $ map (\c -> if stringName c == stringName other then "[" ++ stringName c ++ "]" else stringName c) (cls:chain ++ [cls])), Just $ Type.location other)) chain
    , _errorHelp = Just ("Specify other class to extend or remove 'extends' clause", Type.extendsPosition cls)
    , _errorMarkers = NoMarker
}
decodeError (CyclicInheritanceSelf cls) = SimpleError {
    _errorName = "Invalid inheritance"
    , _errorDescription = "Class cannot extend itself"
    , _errorSugestions = [
        "Try to check if that is a spelling error and class should inherit from other one"
    ]
    , _errorLocation = Just $ Type.location cls
    , _errorContexts = []
    , _errorHelp = Just ("Remove '" ++ (stringName cls) ++ "' from the inheritance list", Type.extendsPosition cls)
    , _errorMarkers = NoMarker
}
decodeError (UnknownParent cls parent) = SimpleError {
    _errorName = "Unknown symbol"
    , _errorDescription = "Class '" ++ (stringName cls) ++ "' extends unknown class '" ++ parent ++ "'"
    , _errorSugestions = [
        "Try to check if that is a spelling error and class should inherit from other one"
    ]
    , _errorLocation = Just $ Type.location cls
    , _errorContexts = []
    , _errorHelp = Nothing
    , _errorMarkers = NoMarker
}
decodeError (DuplicateMembersInChain cls member others) = SimpleError {
    _errorName = "Duplicate definition"
    , _errorDescription = "Class '" ++ stringName cls ++ "' has duplicate defition for member '" ++ stringName member ++ "' in its inheritance chain."
    , _errorSugestions = []
    , _errorLocation = Just $ Type.location member
    , _errorContexts = map (uncurry $ getContext (stringName cls) member) others
    , _errorHelp = Nothing
    , _errorMarkers = NoMarker
}
    where
        getContext originalClassName (Type.Method _ _ _ _) parentCls parentMember@(Type.Method _ _ _ _) =
            ("Method '" ++ stringName parentMember ++ "' declared in parent class '" ++ stringName parentCls ++ "' has incompatible type with method in the child class '" ++ originalClassName ++ "'", Just $ Type.location parentMember)
        getContext originalClassName (Type.Method _ _ _ _) parentCls parentMember@(Type.Field _ _ _) =
            ("There's a field '" ++ stringName parentMember ++ "' declared in parent class '" ++ stringName parentCls ++ "' whose name conflicts with the method in the child class '" ++ originalClassName ++ "'", Just $ Type.location parentMember)
        getContext originalClassName (Type.Field _ _ _) parentCls parentMember@(Type.Field _ _ _) =
            ("There's a field '" ++ stringName parentMember ++ "' declared in parent class '" ++ stringName parentCls ++ "' which conflicts with the field in the child class '" ++ originalClassName ++ "'", Just $ Type.location parentMember)
        getContext originalClassName (Type.Field _ _ _) parentCls parentMember@(Type.Method _ _ _ _) =
            ("There's a method '" ++ stringName parentMember ++ "' declared in parent class '" ++ stringName parentCls ++ "' whose name conflicts with the field in the child class '" ++ originalClassName ++ "'", Just $ Type.location parentMember)


-- decodeError (TypeMatch t1 t2) = SimpleError {
--     _errorName = "Type mismatch"
--     , _errorDescription = "Value has type " ++ (simplePretty t1) ++ " but expected type " ++ (simplePretty t2)
--     , _errorSugestions = [
--         "Checking the return type of the operation"
--         , "Checking the types of arguments"
--     ]
--     , _errorOocontext = Nothing
-- }
-- decodeError (OperatorTypeMatch o goods (l, r)) = SimpleError {
--     _errorName = "Type mismatch"
--     , _errorDescription = "Operator '" ++ (simplePretty o) ++ "' cannot be used to compute " ++ (simplePretty l) ++ " " ++ (simplePretty o) ++ " " ++ (simplePretty r)
--     , _errorSugestions = map (\(gl, gr) -> "Using operator " ++ (simplePretty gl) ++ " " ++ (simplePretty o) ++ " " ++ (simplePretty gr)) goods
--     , _errorOocontext = Nothing
-- }
-- decodeError (ArgNumFun f expected got) = SimpleError {
--     _errorName = "Invalid parameters count"
--     , _errorDescription = "Function " ++ (simplePretty f) ++ " expected " ++ (show expected) ++ " but got " ++ (show got) ++ " parameters"
--     , _errorSugestions = [
--         "Checking if you missed some call parameters"
--     ]
--     , _errorOocontext = Nothing
-- }
-- decodeError (ArgNumMethod cls method expected got) = SimpleError {
--     _errorName = "Invalid parameters count"
--     , _errorDescription = "Method " ++ (simplePretty method) ++ " expected " ++ (show expected) ++ " but got " ++ (show got) ++ " parameters"
--     , _errorSugestions = [
--         "Checking if you missed some call parameters"
--     ]
--     , _errorOocontext = Just $ "Class " ++ simplePretty cls
-- }
-- decodeError (ArgNumConstructor cls (Just method) expected got) = SimpleError {
--     _errorName = "Invalid parameters count"
--     , _errorDescription = "Constructor" ++ (simplePretty method) ++ " expected " ++ (show expected) ++ " but got " ++ (show got) ++ " parameters"
--     , _errorSugestions = [
--         "Checking if you missed some call parameters"
--     ]
--     , _errorOocontext = Just $ "Class " ++ simplePretty cls
-- }
-- decodeError (ArgNumConstructor cls Nothing expected got) = SimpleError {
--     _errorName = "Invalid parameters count"
--     , _errorDescription = "Default constructor expected " ++ (show expected) ++ " but got " ++ (show got) ++ " parameters"
--     , _errorSugestions = [
--         "Checking if you missed some call parameters"
--     ]
--     , _errorOocontext = Just $ "Class " ++ simplePretty cls
-- }

-- decodeError (DuplicateVar i) = SimpleError {
--     _errorName = "Duplicate definition"
--     , _errorDescription = "Variable '" ++ (simplePretty i) ++ "' was defined twice"
--     , _errorSugestions = []
--     , _errorOocontext = Nothing
-- }
-- decodeError (DuplicateFun i) = SimpleError {
--     _errorName = "Duplicate definition"
--     , _errorDescription = "Function '" ++ (simplePretty i) ++ "' was defined twice"
--     , _errorSugestions = []
--     , _errorOocontext = Nothing
-- }
decodeError (DuplicateClass cls others) = SimpleError {
    _errorName = "Duplicate definition"
    , _errorDescription = "Class '" ++ (stringName cls) ++ "' was defined twice"
    , _errorSugestions = [
        "Remove other class definition"
        , "Rename the classes to have unique names"
    ]
    , _errorLocation = Just $ Type.location cls
    , _errorContexts = map (\other -> ("Duplicate class definition", Just $ Type.location other)) others
    , _errorHelp = case cls <? others of
        (Just same) -> Just $ ("Those class definitions are exactly the same, you can remove it", Type.location cls)
        Nothing -> Just $ ("Two classes differ, you can rename one to '" ++ "ABC" ++ "'", Type.namePosition cls)
    , _errorMarkers = NoMarker
}
decodeError (DuplicateMember cls member others) = SimpleError {
    _errorName = "Duplicate definition"
    , _errorDescription = "Method '" ++ (stringName member) ++ "' in class " ++ (stringName cls) ++ " was defined twice"
    , _errorSugestions = []
    , _errorLocation = Just $ Type.location cls
    , _errorContexts = map (\other -> ("Duplicate member definition", Just $ Type.location other)) others
    , _errorHelp = case member <? others of
        (Just same) -> Just $ ("Those member definitions are exactly the same, you can remove it", Type.location member)
        Nothing -> Just $ ("Two member differ, you can rename one to '" ++ "ABC" ++ "'", Type.namePosition member)
    , _errorMarkers = NoMarker
}
decodeError (DuplicateFun fn others) = SimpleError {
    _errorName = "Duplicate definition"
    , _errorDescription = "Function '" ++ (stringName fn) ++ "' was defined twice"
    , _errorSugestions = []
    , _errorLocation = Just $ Type.location fn
    , _errorContexts = map (\other -> ("Duplicate function definition", Just $ Type.location other)) others
    , _errorHelp = case fn <? others of
        (Just same) -> Just $ ("Those function definitions are exactly the same, you can remove it", Type.location fn)
        Nothing -> Just $ ("Two function differ, you can rename one to '" ++ "ABC" ++ "'", Type.namePosition fn)
    , _errorMarkers = NoMarker
}
-- decodeError (DuplicateConstructor cls (Just constr)) = SimpleError {
--     _errorName = "Duplicate definition"
--     , _errorDescription = "Constructor '" ++ (simplePretty constr) ++ "' in class " ++ (simplePretty cls) ++ " was defined twice"
--     , _errorSugestions = []
--     , _errorOocontext = Just $ "Class " ++ simplePretty cls
-- }
-- decodeError (DuplicateConstructor cls Nothing) = SimpleError {
--     _errorName = "Duplicate definition"
--     , _errorDescription = "Unnamed constructor in class " ++ (simplePretty cls) ++ " was defined twice"
--     , _errorSugestions = []
--     , _errorOocontext = Just $ "Class " ++ simplePretty cls
-- }
-- decodeError (ClassMatch clsa clsb) = SimpleError {
--     _errorName = "Type mismatch"
--     , _errorDescription = "Type " ++ (simplePretty clsa) ++ " should have " ++ (simplePretty clsb) ++ " in inheritance chain. Unfortunately it did not inherit from " ++ (simplePretty clsb)
--     , _errorSugestions = []
--     , _errorOocontext = Nothing
-- }
-- decodeError (UndefinedVar i) = SimpleError {
--     _errorName = "Invalid symbol used"
--     , _errorDescription = "Value is undefined: " ++ (simplePretty i)
--     , _errorSugestions = []
--     , _errorOocontext = Nothing
-- }
-- decodeError (UndefinedFun i) = SimpleError {
--     _errorName = "Invalid symbol used"
--     , _errorDescription = "Function is undefined: " ++ (simplePretty i)
--     , _errorSugestions = []
--     , _errorOocontext = Nothing
-- }
-- decodeError (NoReturn) = SimpleError {
--     _errorName = "Type mismatch"
--     , _errorDescription = "Function declares non-void return type, but have no return statement"
--     , _errorSugestions = []
--     , _errorOocontext = Nothing
-- }
-- decodeError (UndefinedClass cls) = SimpleError {
--     _errorName = "Invalid symbol used"
--     , _errorDescription = "The class '" ++ (simplePretty cls) ++ "' is not known"
--     , _errorSugestions = []
--     , _errorOocontext = Just $ "Class " ++ simplePretty cls
-- }
-- decodeError (NotAClass t) = SimpleError {
--     _errorName = "Invalid symbol used"
--     , _errorDescription = "Cannot use '" ++ (simplePretty t) ++ "' here, expected a class."
--     , _errorSugestions = []
--     , _errorOocontext = Nothing
-- }
-- decodeError (UndefinedField cls i) = SimpleError {
--     _errorName = "Invalid symbol used"
--     , _errorDescription = "Unknown field '" ++ (simplePretty i) ++ "' was used"
--     , _errorSugestions = []
--     , _errorOocontext = Just $ "Class " ++ simplePretty cls
-- }
-- decodeError (UndefinedMethod cls i) = SimpleError {
--     _errorName = "Invalid symbol used"
--     , _errorDescription = "Unknown method '" ++ (simplePretty i) ++ "' was used"
--     , _errorSugestions = []
--     , _errorOocontext = Just $ "Class " ++ simplePretty cls
-- }
-- decodeError (UndefinedConstructor cls (Just i)) = SimpleError {
--     _errorName = "Invalid symbol used"
--     , _errorDescription = "Unknown constructor '" ++ (simplePretty i) ++ "' was used"
--     , _errorSugestions = []
--     , _errorOocontext = Just $ "Class " ++ simplePretty cls
-- }
-- decodeError (UndefinedConstructor cls Nothing) = SimpleError {
--     _errorName = "Invalid symbol used"
--     , _errorDescription = "Default class constructor does not exist"
--     , _errorSugestions = []
--     , _errorOocontext = Just $ "Class " ++ simplePretty cls
-- }
-- decodeError ThisNotInClass = SimpleError {
--     _errorName = "Invalid symbol used"
--     , _errorDescription = "Usage of keyword this outside the valid class body context"
--     , _errorSugestions = []
--     , _errorOocontext = Nothing
-- }
-- decodeError SuperNotInClass = SimpleError {
--     _errorName = "Invalid symbol used"
--     , _errorDescription = "Usage of keyword super(...) outside the valid class body context"
--     , _errorSugestions = []
--     , _errorOocontext = Nothing
-- }
-- decodeError ThisArgName = SimpleError {
--     _errorName = "Invalid symbol used"
--     , _errorDescription = "Cannot use reserved name 'this'"
--     , _errorSugestions = []
--     , _errorOocontext = Nothing
-- }
-- decodeError (BadPrivateAccess cls f) = SimpleError {
--     _errorName = "Access denied"
--     , _errorDescription = "Cannot access the field '" ++ f ++ "' that is private"
--     , _errorSugestions = []
--     , _errorOocontext = Just $ "Class " ++ simplePretty cls
-- }
-- decodeError (BadProtectedAccess cls f) = SimpleError {
--     _errorName = "Access denied"
--     , _errorDescription = "Cannot access the field '" ++ f ++ "' that is protected"
--     , _errorSugestions = []
--     , _errorOocontext = Just $ "Class " ++ simplePretty cls
-- }
-- decodeError (NoSuperClass cls) = SimpleError {
--     _errorName = "Invalid symbol used"
--     , _errorDescription = "Cannot use super if the class inherits from no other class"
--     , _errorSugestions = []
--     , _errorOocontext = Just $ "Class " ++ simplePretty cls
-- }
-- decodeError (NoSuperClass cls) = SimpleError {
--     _errorName = "Invalid symbol used"
--     , _errorDescription = "Cannot use super if the class inherits from no other class"
--     , _errorSugestions = []
--     , _errorOocontext = Just $ "Class " ++ simplePretty cls
-- }
-- decodeError BadSuperUse = SimpleError {
--     _errorName = "Invalid symbol used"
--     , _errorDescription = "Usage of keyword super(...) outside the valid class body context"
--     , _errorSugestions = []
--     , _errorOocontext = Nothing
-- }
-- decodeError err = SimpleError {
--     _errorName = "Unknown error"
--     , _errorDescription = "Unknown problem has occurred: "
--     , _errorSugestions = []
--     , _errorOocontext = Nothing
-- }


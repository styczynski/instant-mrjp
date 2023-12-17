{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
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

decodeError :: Error -> SimpleError
decodeError (UnknownFailure msg) = SimpleError {
    _errorName = "Unknown fatal error"
    , _errorDescription = "This is a generic case for an error. It should never ever happen in practice. It's likely a problem with compiler itself. Oopsie!"
    , _errorSugestions = [
        "Ask author why this happened???"
    ]
    , _errorLocation = Nothing
    , _errorContexts = [
        ("Details:\n" ++ msg, Nothing)
    ]
    , _errorHelp = Nothing
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
}
-- decodeError MainType = SimpleError {
--     _errorName = "Invalid main type"
--     , _errorDescription = "Main function should be defined as int main()"
--     , _errorSugestions = [
--         "Changing return type of main to int"
--     ]
--     , _errorOocontext = Nothing
-- }
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
}
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Instant.Errors.Messages where

import Instant.Errors.Base
import Instant.Errors.Def
import Instant.Syntax

decodeError :: Error -> SimpleError
decodeError MainType = SimpleError {
    _errorName = "Invalid main type"
    , _errorDescription = "Main function should be defined as int main()"
    , _errorSugestions = [
        "Changing return type of main to int"
    ]
    , _errorOocontext = Nothing
}
decodeError NoMain = SimpleError {
    _errorName = "No entrypoint"
    , _errorDescription = "There is no main function defined."
    , _errorSugestions = [
        "Defining main function: int main() {}"
    ]
    , _errorOocontext = Nothing
}
decodeError (TypeMatch t1 t2) = SimpleError {
    _errorName = "Type mismatch"
    , _errorDescription = "Value has type " ++ (simplePretty t1) ++ " but expected type " ++ (simplePretty t2)
    , _errorSugestions = [
        "Checking the return type of the operation"
        , "Checking the types of arguments"
    ]
    , _errorOocontext = Nothing
}
decodeError (OperatorTypeMatch o goods (l, r)) = SimpleError {
    _errorName = "Type mismatch"
    , _errorDescription = "Operator '" ++ (simplePretty o) ++ "' cannot be used to compute " ++ (simplePretty l) ++ " " ++ (simplePretty o) ++ " " ++ (simplePretty r)
    , _errorSugestions = map (\(gl, gr) -> "Using operator " ++ (simplePretty gl) ++ " " ++ (simplePretty o) ++ " " ++ (simplePretty gr)) goods
    , _errorOocontext = Nothing
}
decodeError (ArgNumFun f expected got) = SimpleError {
    _errorName = "Invalid parameters count"
    , _errorDescription = "Function " ++ (simplePretty f) ++ " expected " ++ (show expected) ++ " but got " ++ (show got) ++ " parameters"
    , _errorSugestions = [
        "Checking if you missed some call parameters"
    ]
    , _errorOocontext = Nothing
}
decodeError (ArgNumMethod cls method expected got) = SimpleError {
    _errorName = "Invalid parameters count"
    , _errorDescription = "Method " ++ (simplePretty method) ++ " expected " ++ (show expected) ++ " but got " ++ (show got) ++ " parameters"
    , _errorSugestions = [
        "Checking if you missed some call parameters"
    ]
    , _errorOocontext = Just $ "Class " ++ simplePretty cls
}
decodeError (ArgNumConstructor cls (Just method) expected got) = SimpleError {
    _errorName = "Invalid parameters count"
    , _errorDescription = "Constructor" ++ (simplePretty method) ++ " expected " ++ (show expected) ++ " but got " ++ (show got) ++ " parameters"
    , _errorSugestions = [
        "Checking if you missed some call parameters"
    ]
    , _errorOocontext = Just $ "Class " ++ simplePretty cls
}
decodeError (ArgNumConstructor cls Nothing expected got) = SimpleError {
    _errorName = "Invalid parameters count"
    , _errorDescription = "Default constructor expected " ++ (show expected) ++ " but got " ++ (show got) ++ " parameters"
    , _errorSugestions = [
        "Checking if you missed some call parameters"
    ]
    , _errorOocontext = Just $ "Class " ++ simplePretty cls
}

decodeError (DuplicateVar i) = SimpleError {
    _errorName = "Duplicate definition"
    , _errorDescription = "Variable '" ++ (simplePretty i) ++ "' was defined twice"
    , _errorSugestions = []
    , _errorOocontext = Nothing
}
decodeError (DuplicateFun i) = SimpleError {
    _errorName = "Duplicate definition"
    , _errorDescription = "Function '" ++ (simplePretty i) ++ "' was defined twice"
    , _errorSugestions = []
    , _errorOocontext = Nothing
}
decodeError (DuplicateClass cls) = SimpleError {
    _errorName = "Duplicate definition"
    , _errorDescription = "Class '" ++ (simplePretty cls) ++ "' was defined twice"
    , _errorSugestions = []
    , _errorOocontext = Just $ "Class " ++ simplePretty cls
}
decodeError (DuplicateField cls i) = SimpleError {
    _errorName = "Duplicate definition"
    , _errorDescription = "Method '" ++ (simplePretty i) ++ "' in class " ++ (simplePretty cls) ++ " was defined twice"
    , _errorSugestions = []
    , _errorOocontext = Just $ "Class " ++ simplePretty cls
}
decodeError (DuplicateConstructor cls (Just constr)) = SimpleError {
    _errorName = "Duplicate definition"
    , _errorDescription = "Constructor '" ++ (simplePretty constr) ++ "' in class " ++ (simplePretty cls) ++ " was defined twice"
    , _errorSugestions = []
    , _errorOocontext = Just $ "Class " ++ simplePretty cls
}
decodeError (DuplicateConstructor cls Nothing) = SimpleError {
    _errorName = "Duplicate definition"
    , _errorDescription = "Unnamed constructor in class " ++ (simplePretty cls) ++ " was defined twice"
    , _errorSugestions = []
    , _errorOocontext = Just $ "Class " ++ simplePretty cls
}
decodeError (ClassMatch clsa clsb) = SimpleError {
    _errorName = "Type mismatch"
    , _errorDescription = "Type " ++ (simplePretty clsa) ++ " should have " ++ (simplePretty clsb) ++ " in inheritance chain. Unfortunately it did not inherit from " ++ (simplePretty clsb)
    , _errorSugestions = []
    , _errorOocontext = Nothing
}
decodeError (UndefinedVar i) = SimpleError {
    _errorName = "Invalid symbol used"
    , _errorDescription = "Value is undefined: " ++ (simplePretty i)
    , _errorSugestions = []
    , _errorOocontext = Nothing
}
decodeError (UndefinedFun i) = SimpleError {
    _errorName = "Invalid symbol used"
    , _errorDescription = "Function is undefined: " ++ (simplePretty i)
    , _errorSugestions = []
    , _errorOocontext = Nothing
}
decodeError (NoReturn) = SimpleError {
    _errorName = "Type mismatch"
    , _errorDescription = "Function declares non-void return type, but have no return statement"
    , _errorSugestions = []
    , _errorOocontext = Nothing
}
decodeError (UndefinedClass cls) = SimpleError {
    _errorName = "Invalid symbol used"
    , _errorDescription = "The class '" ++ (simplePretty cls) ++ "' is not known"
    , _errorSugestions = []
    , _errorOocontext = Just $ "Class " ++ simplePretty cls
}
decodeError (NotAClass t) = SimpleError {
    _errorName = "Invalid symbol used"
    , _errorDescription = "Cannot use '" ++ (simplePretty t) ++ "' here, expected a class."
    , _errorSugestions = []
    , _errorOocontext = Nothing
}
decodeError (UndefinedField cls i) = SimpleError {
    _errorName = "Invalid symbol used"
    , _errorDescription = "Unknown field '" ++ (simplePretty i) ++ "' was used"
    , _errorSugestions = []
    , _errorOocontext = Just $ "Class " ++ simplePretty cls
}
decodeError (UndefinedMethod cls i) = SimpleError {
    _errorName = "Invalid symbol used"
    , _errorDescription = "Unknown method '" ++ (simplePretty i) ++ "' was used"
    , _errorSugestions = []
    , _errorOocontext = Just $ "Class " ++ simplePretty cls
}
decodeError (UndefinedConstructor cls (Just i)) = SimpleError {
    _errorName = "Invalid symbol used"
    , _errorDescription = "Unknown constructor '" ++ (simplePretty i) ++ "' was used"
    , _errorSugestions = []
    , _errorOocontext = Just $ "Class " ++ simplePretty cls
}
decodeError (UndefinedConstructor cls Nothing) = SimpleError {
    _errorName = "Invalid symbol used"
    , _errorDescription = "Default class constructor does not exist"
    , _errorSugestions = []
    , _errorOocontext = Just $ "Class " ++ simplePretty cls
}
decodeError ThisNotInClass = SimpleError {
    _errorName = "Invalid symbol used"
    , _errorDescription = "Usage of keyword this outside the valid class body context"
    , _errorSugestions = []
    , _errorOocontext = Nothing
}
decodeError SuperNotInClass = SimpleError {
    _errorName = "Invalid symbol used"
    , _errorDescription = "Usage of keyword super(...) outside the valid class body context"
    , _errorSugestions = []
    , _errorOocontext = Nothing
}
decodeError ThisArgName = SimpleError {
    _errorName = "Invalid symbol used"
    , _errorDescription = "Cannot use reserved name 'this'"
    , _errorSugestions = []
    , _errorOocontext = Nothing
}
decodeError (BadPrivateAccess cls f) = SimpleError {
    _errorName = "Access denied"
    , _errorDescription = "Cannot access the field '" ++ f ++ "' that is private"
    , _errorSugestions = []
    , _errorOocontext = Just $ "Class " ++ simplePretty cls
}
decodeError (BadProtectedAccess cls f) = SimpleError {
    _errorName = "Access denied"
    , _errorDescription = "Cannot access the field '" ++ f ++ "' that is protected"
    , _errorSugestions = []
    , _errorOocontext = Just $ "Class " ++ simplePretty cls
}
decodeError (NoSuperClass cls) = SimpleError {
    _errorName = "Invalid symbol used"
    , _errorDescription = "Cannot use super if the class inherits from no other class"
    , _errorSugestions = []
    , _errorOocontext = Just $ "Class " ++ simplePretty cls
}
decodeError (NoSuperClass cls) = SimpleError {
    _errorName = "Invalid symbol used"
    , _errorDescription = "Cannot use super if the class inherits from no other class"
    , _errorSugestions = []
    , _errorOocontext = Just $ "Class " ++ simplePretty cls
}
decodeError BadSuperUse = SimpleError {
    _errorName = "Invalid symbol used"
    , _errorDescription = "Usage of keyword super(...) outside the valid class body context"
    , _errorSugestions = []
    , _errorOocontext = Nothing
}
decodeError err = SimpleError {
    _errorName = "Unknown error"
    , _errorDescription = "Unknown problem has occurred: "
    , _errorSugestions = []
    , _errorOocontext = Nothing
}


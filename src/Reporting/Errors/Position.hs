
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
module Reporting.Errors.Position where

import GHC.Generics (Generic)
import Control.DeepSeq

data TokenUID =
    TokenUID Int
    | NoUID 
  deriving (Eq, Ord, Generic, NFData)

data Position = Position TokenUID String Int Int 
              | BuiltIn
              | Undefined
  deriving (Eq, Ord, Generic, NFData)

instance Show TokenUID where
  show NoUID = "#?"
  show (TokenUID id) = "#" ++ (show id)

instance Show Position where
    show (Position id file line col) = show id --"{" ++ show line ++ "," ++ show col ++ "}"--"\""++file++"\", line: "++ show line++", column: "++show col
    show BuiltIn = "inside standard library"
    show Undefined = "(undefined)"

positionLC :: Position -> (Int, Int)
positionLC (Position _ _ l c) = (l, c)
positionLC _ = (0, 0)

positionSrc :: Position -> String
positionSrc (Position _ src _ _) = src
positionSrc _ = ""

isPositionFrom :: Maybe String -> Position -> Bool
isPositionFrom (Just expectedSrc) (Position _ src _ _) = expectedSrc == src
isPositionFrom Nothing (Position _ src _ _) = True
isPositionFrom _ _ = False

module Reporting.Errors.Position where

data Position = Position String Int Int 
              | BuiltIn
              | Undefined
  deriving (Eq, Ord)

instance Show Position where
    show (Position file line col) = "\""++file++"\", line: "++ show line++", column: "++show col
    show BuiltIn = "inside standard library"
    show Undefined = "(undefined)"

positionLC :: Position -> (Int, Int)
positionLC (Position _ l c) = (l, c)

positionSrc :: Position -> String
positionSrc (Position src _ _) = src
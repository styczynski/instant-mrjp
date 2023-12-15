module Parser.Types where

import qualified Parser.Gen.AbsLatte as AST
import qualified Parser.Gen.LexLatte as Syntax
import qualified Reporting.Errors.Position as P

data RawProgram = 
    ProgramParseError ParseError
    | RawProgram AST.Program [Syntax.Token]

data ParseError
  = ParseError String String (Maybe (Int, Int))

findTokenEnd :: RawProgram -> P.Position -> P.Position
findTokenEnd (ProgramParseError _) p = p
findTokenEnd (RawProgram _ tokens) (P.Position filename line col) =
  (uncurry (P.Position filename)) (Syntax.tokenLineCol . head . (filter (\t -> let (l, c) = Syntax.tokenLineCol t in (l > line || (l == line && c > col)))) $ tokens)
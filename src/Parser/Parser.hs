module Parser.Parser where

import Reporting.Logs
--import qualified Program.Syntax as AST
import Parser.Gen.ParLatte
import qualified Parser.Gen.AbsLatte as AST
import Prelude
import Reporting.Errors.Base
import Reporting.Errors.Errors
import Parser.Gen.PrintLatte (Print)
import Parser.Types
import qualified Parser.Gen.LexLatte as Syntax
import qualified Reporting.Errors.Position as P


instance Errorable ParseError where
  describe (ParseError src msg loc) = SimpleError {
        _errorName = "Parsing error"
        , _errorDescription = "Could not load the input text due to " ++ msg
        , _errorSugestions = []
        , _errorContexts = []
        , _errorLocation = case loc of 
          Nothing -> Nothing
          Just (l, c) -> Just $ P.Position src l c
        , _errorHelp = Nothing
    }
  -- getOrigin (ParseError _ _ loc) = loc
  -- getSourceName (ParseError src _ _) = src

createParseError :: String -> String -> [Syntax.Token] -> ParseError
createParseError file msg tokens = case tokens of
    []      -> ParseError file "parsing error at the end of the file" Nothing
    [Syntax.Err _] -> ParseError file "lexer error" Nothing
    t:_     -> let (Syntax.Pn _ l c) = Syntax.tokenPosn t in ParseError file ("parsing error near '" ++ (Syntax.prToken t) ++ "'") (Just (l, c))

parseLatte:: String -> String -> LattePipeline RawProgram
parseLatte file textContent = do
    let tokens = myLexer textContent
    case pProgram tokens of
        Left msg -> do
            --mapM_ (putStrV v . showPosToken . mkPosToken) ts
            return $ ProgramParseError $ createParseError file msg tokens
        Right ast -> do
            return $ RawProgram ast tokens

    
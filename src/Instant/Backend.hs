{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Instant.Backend where

import Instant.Syntax
import qualified Data.Text as T
import Instant.Logs

type InstantBackendFn = String -> ICode -> InstantPipeline (Either String String)

data InstantBackend = InstantBackend
  {
    name :: String,
    run :: InstantBackendFn
  }

runBackend :: String -> (ASTNode 'InstantProgram) -> InstantBackend -> InstantPipeline (Either String String)
runBackend fileName ast backend = do
    normalizedAST <- do
        printLogInfo $ "Normalizing AST"
        return $ fromAST ast
    printLogInfo $ "Running correct compiler backend: " <> (T.pack $ name backend)
    (run backend) fileName normalizedAST

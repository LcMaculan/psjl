module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Exception as Exception

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)

import Data.Either (Either(..))

import PsJl.Compiler as PsJl

main :: Effect Unit
main = do
    contents <- readTextFile UTF8 "ps/corefn.json"
    julia <- case PsJl.compile contents of
      Right x -> pure x
      Left x -> Exception.throw (show x)
    writeTextFile UTF8 "jl/corefn.jl" julia
    log "(:"
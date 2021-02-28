module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Exception as Exception

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile, readdir, stat, mkdir)
import Node.FS.Stats (isDirectory)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable as Traversable
import Data.Array as Array

import PsJl.Compiler as PsJl

compile :: String -> Effect String
compile fp = do
  stats <- stat fp
  if isDirectory stats
    then compileDir fp
    else compileFile fp

compileFile :: String -> Effect String
compileFile fp =
  case Array.last (String.split (String.Pattern "/") fp) of
    Nothing -> pure $ "skipped: " <> fp <> "\n"
    Just ext -> if ext == "corefn.json" then compileFunction fp
                                        else pure $ "skipped: " <> fp <> "\n"
  where
    compileFunction :: String -> Effect String
    compileFunction sourceFile = do
      contents <- readTextFile UTF8 sourceFile
      julia <- case PsJl.compile contents of
        Right x -> pure x
        Left x -> Exception.throw (show x)
      targetFile <- pure $ String.replace (String.Pattern ".json") (String.Replacement ".jl") (getTargetPath fp) 
      writeTextFile UTF8 targetFile julia
      pure $ "compiled: " <> targetFile <> "\n"

compileDir :: String -> Effect String
compileDir fp = do
  mkdir $ getTargetPath fp
  files <- readdir fp
  compiledFiles <- Traversable.traverse (\x -> compileFunction fp x) files
  pure $ "Compiled Dir: " <> fp <> "\n" <> String.joinWith "" compiledFiles
  where
    compileFunction :: String -> String -> Effect String
    compileFunction fd ffp = do
      compile $ fd <> "/" <> ffp

getTargetPath :: String -> String
getTargetPath fd =
  case String.stripPrefix (String.Pattern "ps") fd of
    Just nfd -> "jl" <> nfd
    Nothing -> fd

main :: Effect Unit
main = do
  compiledFiles <- compile "ps"
  log compiledFiles 
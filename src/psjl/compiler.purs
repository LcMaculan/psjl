module PsJl.Compiler where

import Prelude

import PsJl.CoreFnUtils (getModuleComments, getModuleDecls, getModuleExports, getModuleForeign, getModuleImports, getModuleName, getModulePath)

import Control.Monad.Except (runExcept)

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Data.String as String
import Data.Array as Array

import CoreFn.FromJSON (moduleFromJSON)
import CoreFn.Module (Module, Version(..), FilePath(..), ModuleImport(..))
import CoreFn.Ann (Ann(..), Comment(..))
import CoreFn.Expr (Expr(..), Bind(..), CaseAlternative(..))
import CoreFn.Names (ModuleName(..), ProperName(..), Qualified(..))
import CoreFn.Ident (Ident(..))
import CoreFn.Literal (Literal(..))
import CoreFn.Binders (Binder(..))

compile :: String -> Either String String
compile code = case runExcept (moduleFromJSON code) of
    Right module_ -> Right (compileModule module_)
    Left e -> Left "error in compile function"

formatError :: String -> String
formatError errorMessage =
  "error(\"" <> errorMessage <> "\")"

class Compile a where
  compile_ :: a -> String

instance compileVersion :: Compile Version where
  compile_ (Version v) = v

instance compileModuleName :: Compile ModuleName where
  compile_ (ModuleName properNames) = String.joinWith "." (map compile_ properNames)

instance compileProperName :: Compile ProperName where
  compile_ (ProperName name) = name

instance compileQualified :: Compile a => Compile (Qualified a) where
  compile_ (Qualified m a) =
    String.joinWith "." $ Array.filter compileFilter [ compile_ m, compile_ a ]

instance compileIndent :: Compile Ident where
  compile_ (Ident x) = x
  compile_ (GenIdent w z) = case w of
    Nothing -> ""
    Just x -> "\t" <> x
  compile_ UnusedIdent = ""

instance compileComment :: Compile Comment where
  compile_ (LineComment x) = "#" <> x
  compile_ (BlockComment x) = "#" <> x

instance compileFilePath :: Compile FilePath where
  compile_ (FilePath x) = x

instance compileAnn :: Compile Ann where
  compile_ (Ann ann) =
    ""

instance compileBind :: (Compile a) => Compile (Bind a) where
  compile_ (NonRec a i e) =
    case e of
      Constructor a1 t c fs -> String.joinWith " " $ Array.filter compileFilter [ compile_ a, compile_ (Constructor a1 t c fs) ]
      _ -> String.joinWith " " $ Array.filter compileFilter [ compile_ a, compile_ i, "=", compile_ e ]
  compile_ (Rec b) = 
    formatError "(Rec b)"

instance compileLiteral :: Compile a => Compile (Literal a) where
  compile_ (NumericLiteral e) = either compile_ compile_ e
  compile_ (StringLiteral s) =
    compile_ s
  compile_ (CharLiteral c) =
    compile_ c
  compile_ (BooleanLiteral b) =
    compile_ b
  compile_ (ArrayLiteral a) =
    "[ " <> (replaceSepartor (compile_ a) ", ") <> " ]"
  compile_ (ObjectLiteral o) =
    "Dict([" <> (replaceSepartor (compile_ o) ", ") <> "])"

instance compileExpr :: (Compile a) => Compile (Expr a) where
  compile_ (Literal a l) =
    compile_ a <> compile_ l
  compile_ (Constructor a t c fs) =
    let compiled_fs = compile_ fs
    in case compiled_fs of
    "" -> compile_ a <> "struct " <> compile_ c <> " end"
    _ -> compile_ a <> "struct " <> compile_ c <> "\n" <> compiled_fs <> "\n" <> "end"
  compile_ (Accessor a s e) =
    compile_ e <> "[" <> compile_ s <> "]" 
  compile_ (ObjectUpdate a e fs) =
    formatError "(ObjectUpdate a e fs)"
  compile_ (Abs a i e) =
    compile_ a <> "(" <> compile_ i <> " -> " <> compile_ e <> ")"
  compile_ (App a e1 e2) =
    String.joinWith "" $ Array.filter compileFilter [ compile_ a, compile_ e1, "(", compile_ e2, ")" ]
  compile_ (Var a q) =
    String.joinWith " " $ Array.filter compileFilter [ compile_ a, compile_ q ]
  compile_ (Case a es cs) =
    let compiledCase = String.replaceAll (String.Pattern "#value;") (String.Replacement (compile_ es)) (compile_ cs)
    in formatSectionSpaces $ formatIf compiledCase
    where
      formatIf s = String.replace (String.Pattern "elseif") (String.Replacement "if") s
      formatSectionSpaces s = "\n" <> s <> "\nend"
  compile_ (Let a bs e) =
    formatError "(Let a bs e)"

instance compileCaseAlternative :: Compile a => Compile (CaseAlternative a) where
  compile_ (CaseAlternative { caseAlternativeBinders, caseAlternativeResult }) =
    compile_ caseAlternativeBinders <> "\n" <> compile_ caseAlternativeResult

instance compileBinder :: Compile a => Compile (Binder a) where
  compile_ (NullBinder a) =
    "else"
  compile_ (LiteralBinder a ls) =
    "elseif #value; == " <> compile_ ls
  compile_ (VarBinder a i) =
    formatError "(VarBinder a i)"
  compile_ (ConstructorBinder a t c bs) =
    formatError "(ConstructorBinder a t c bs)"
  compile_ (NamedBinder a i b) =
    formatError "(NamedBinder a i b)"

instance compileString :: Compile String where
  compile_ x = "\"" <> x <> "\""

instance compileInt :: Compile Int where
  compile_ x = (show x)

instance compileNumber :: Compile Number where
  compile_ x = (show x)

instance compileChar :: Compile Char where
  compile_ x = (show x)

instance compileBoolean :: Compile Boolean where
  compile_ x = (show x)

instance compileArray :: (Compile a) => Compile (Array a) where
  compile_ array = String.joinWith "&sep;" (map compile_ array)

instance compileTuple :: (Compile a, Compile b) => Compile (Tuple a b) where
  compile_ (Tuple a b) =
    "(" <> compile_ a <> ", " <> compile_ b <> ")"

instance compileModuleImport :: Compile ModuleImport where
  compile_ (ModuleImport import_) =
    compile_ import_.moduleName

instance compileMaybe :: (Compile a) => Compile (Maybe a) where
  compile_ Nothing = ""
  compile_ (Just x) = compile_ x

instance compileEither :: (Compile a, Compile b) => Compile (Either a b) where
  compile_ (Left a) = compile_ a
  compile_ (Right b) = compile_ b


compileFilter :: String -> Boolean
compileFilter elem =
  not (elem == "")

replaceSepartor :: String -> String -> String
replaceSepartor content replacement = 
  String.replaceAll (String.Pattern "&sep;") (String.Replacement replacement) content

compileModule :: { version :: Version, module :: Module Ann } -> String
compileModule module_ = 
  let
    versionSection =
      compile_ module_.version
    pathSection =
      compile_ $ (\x -> getModulePath x) module_.module
    commentSection =
      compile_ $ (\x -> getModuleComments x) module_.module
    moduleNameSection =
      compile_ $ (\x -> getModuleName x) module_.module

    splittedImport = String.split (String.Pattern "&sep;") (compile_ $ (\x -> getModuleImports x) module_.module)
    importSection = String.joinWith "\n" $
        map (\x -> if x == "Prim" then "using " <> x else "include(\"../" <> x <> "/corefn.jl\")") $ Array.filter (\x -> (not (x == "")) && (not (x == "Prim"))) splittedImport

    exportSection =
      replaceSepartor (compile_ $ (\x -> getModuleExports x) module_.module) ", "

    splittedForeign = String.split (String.Pattern "&sep;") (compile_ $ (\x -> getModuleForeign x) module_.module)
    foreignSection = String.joinWith ", " $ map (\x -> moduleNameSection <> "." <> x) $ Array.filter (\x -> not (x == "")) splittedForeign
  in
    String.joinWith ""
    [ "# ", (show ((\x -> getModuleDecls x) module_.module)), "\n"
    , "# version ", versionSection, "\n"
    , "# path ", pathSection, "\n"
    , commentSection
    , attachSection moduleNameSection "module  " "\n"
    , attachSection importSection "" "\n"
    , attachSection foreignSection "import " "\n"
    , attachSection exportSection "export " "\n"
    , replaceSepartor (compile_ $ (\x -> getModuleDecls x) module_.module) "\n", "\n"
    , "end"
    ]
  where
    attachSection :: String -> String -> String -> String
    attachSection section start end =
      if section == "" then "" else start <> section <> end
module PsJl.CoreFnUtils where

import Control.Monad.Except (runExcept)

import Data.Either (Either(..), either)

import CoreFn.FromJSON (moduleFromJSON)
import CoreFn.Module (Module(..), Version(..), FilePath(..), ModuleImport(..))
import CoreFn.Ann (Ann(..), Comment(..))
import CoreFn.Expr (Expr(..), Bind(..), CaseAlternative(..))
import CoreFn.Names (ModuleName(..), ProperName(..), Qualified(..))
import CoreFn.Ident (Ident(..))
import CoreFn.Literal (Literal(..))
import CoreFn.Binders (Binder(..))

getModule :: String -> Either String ({ module :: Module Ann, version :: Version })
getModule code = case runExcept (moduleFromJSON code) of
    Right module_ -> Right module_
    Left e -> Left "error in getModule function"

getModuleName :: Module Ann -> ModuleName
getModuleName (Module m) = m.moduleName

getModuleComments :: Module Ann -> Array Comment
getModuleComments (Module m) = m.moduleComments

getModulePath :: Module Ann ->  FilePath
getModulePath (Module m) = m.modulePath

getModuleImports :: Module Ann -> Array ModuleImport
getModuleImports (Module m) = m.moduleImports

getModuleExports :: Module Ann -> Array Ident
getModuleExports (Module m) = m.moduleExports

getModuleForeign :: Module Ann -> Array Ident
getModuleForeign (Module m) = m.moduleForeign

getModuleDecls :: Module Ann -> Array (Bind Ann)
getModuleDecls (Module m) = m.moduleDecls
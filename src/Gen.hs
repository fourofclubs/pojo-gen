{-# LANGUAGE TemplateHaskell #-}

module Gen where

import Control.Lens
import Control.Monad (join)
import Data.Char (toLower)
import qualified Data.List as L

data Field = Field {_fieldName :: String, _fieldType :: Either String Class, _fieldDescription :: String} deriving (Eq, Show)

data Class = Class {_className :: String, _classFields :: [Field]} deriving (Eq, Show)

makeLenses ''Class
makeLenses ''Field

outerClassLines :: Class -> [String]
outerClassLines = classLines False

innerClassLines :: Class -> [String]
innerClassLines = classLines True

classLines :: Bool -> Class -> [String]
classLines inner clss =
  ["public " ++ (if inner then "static " else "") ++ "final class " ++ clss ^. className ++ " extends Data<" ++ _className clss ++ "> {"]
    ++ fmap indent (classBody inner clss)
    ++ ["}"]

classBody :: Bool -> Class -> [String]
classBody inner clss =
  staticDeclarations clss
    ++ "" :
  fieldDeclarations (clss ^. classFields)
    ++ "" :
  constructor clss
    ++ "" :
  factoryMethod clss
    ++ if not inner then "" : innerClasses clss else []

innerClasses :: Class -> [String]
innerClasses clss = toListOf (classFields . folded . fieldType . _Right) clss >>= innerClassLines

indent :: String -> String
indent = ("\t" ++)

fieldTypeName :: Field -> String
fieldTypeName = either id (^. className) . (^. fieldType)

fieldDeclaration :: Field -> [String]
fieldDeclaration fld =
  [ "\\** " ++ _fieldDescription fld ++ " *\\",
    "public final " ++ fieldTypeName fld ++ " " ++ _fieldName fld ++ ";"
  ]

fieldDeclarations :: [Field] -> [String]
fieldDeclarations = (>>= fieldDeclaration)

fieldParams :: [Field] -> String
fieldParams = join . L.intersperse ", " . fmap fieldParam
  where
    fieldParam fld = "final " ++ fieldTypeName fld ++ " " ++ fld ^. fieldName

fieldList :: [Field] -> String
fieldList = L.intercalate ", " . fmap (^. fieldName)

constructor :: Class -> [String]
constructor clss =
  [ "private final " ++ clss ^. className ++ "(" ++ fieldParams (clss ^. classFields) ++ ") {",
    indent "super(" ++ equalName clss ++ ", " ++ hashName clss ++ ", " ++ showName clss ++ ");"
  ]
    ++ fmap indent fieldSetters
    ++ ["}"]
  where
    fields = clss ^. classFields
    fieldSetter fld = "this." ++ fld ^. fieldName ++ "=" ++ fld ^. fieldName ++ ";"
    fieldSetters = fmap fieldSetter fields

staticDeclarations :: Class -> [String]
staticDeclarations clss = [equalDeclaration, hashDeclaration, showDeclaration] >>= ($ clss)

equalDeclaration :: Class -> [String]
equalDeclaration clss =
  [ "/** The {@link " ++ clss ^. className ++ "} {@link Equal} instance */",
    "public static final Equal<" ++ clss ^. className ++ "> " ++ equalName clss ++ ";"
  ]

hashDeclaration :: Class -> [String]
hashDeclaration clss =
  [ "/** The {@link " ++ clss ^. className ++ "} {@link Hash} instance */",
    "public static final Hash<" ++ clss ^. className ++ "> " ++ hashName clss ++ ";"
  ]

showDeclaration :: Class -> [String]
showDeclaration clss =
  [ "/** The {@link " ++ clss ^. className ++ "} {@link Show} instance */",
    "public static final Show<" ++ clss ^. className ++ "> " ++ showName clss ++ ";"
  ]

equalName :: Class -> String
equalName = (++ "Equal") . toLowerHead . (^. className)

hashName :: Class -> String
hashName = (++ "Hash") . toLowerHead . (^. className)

showName :: Class -> String
showName = (++ "Show") . toLowerHead . (^. className)

toLowerHead :: String -> String
toLowerHead [] = []
toLowerHead (s : ss) = toLower s : ss

factoryMethod :: Class -> [String]
factoryMethod clss =
  [ "public static final " ++ name ++ " " ++ toLowerHead name ++ "(" ++ fieldParams (clss ^. classFields) ++ "){",
    indent ("return " ++ constructorCall ++ ";"),
    "}"
  ]
  where
    name = clss ^. className
    constructorCall = "new " ++ name ++ "(" ++ fieldList (clss ^. classFields) ++ ")"
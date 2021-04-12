module Gen where

import Control.Monad (join)
import Data.Char (toLower)
import qualified Data.List as L

data Field = Field {fieldName :: String, fieldType :: Either String Class, description :: String}

data Class = Class {className :: String, classFields :: [Field]}

classLines :: Bool -> Class -> [String]
classLines static clss =
  ["public " ++ if static then "static" else "" ++ "final class " ++ className clss ++ " extends Data<" ++ className clss ++ "> {"]
    ++ fmap indent (classBody clss)
    ++ ["}"]

classBody :: Class -> [String]
classBody clss =
  staticDeclarations clss
    ++ fieldDeclarations (classFields clss)
    ++ "" : constructor clss
    ++ "" : factoryMethod clss

indent :: String -> String
indent = ("\t" ++)

fieldTypeName :: Field -> String
fieldTypeName = either id className . fieldType

fieldDeclaration :: Field -> [String]
fieldDeclaration fld =
  [ "\\** " ++ description fld ++ " *\\",
    "public final " ++ fieldTypeName fld ++ " " ++ fieldName fld ++ ";"
  ]

fieldDeclarations :: [Field] -> [String]
fieldDeclarations = (>>= fieldDeclaration)

fieldParams :: [Field] -> String
fieldParams = join . L.intersperse ", " . fmap fieldParam
  where
    fieldParam fld = "final " ++ fieldTypeName fld ++ " " ++ fieldName fld

fieldList :: [Field] -> String
fieldList = L.intercalate ", " . fmap fieldName

constructor :: Class -> [String]
constructor clss =
  [ "private final " ++ className clss ++ "(" ++ fieldParams (classFields clss) ++ ") {",
    indent "super(" ++ equalName clss ++ ", " ++ hashName clss ++ ", " ++ showName clss ++ ");"
  ]
    ++ fmap indent fieldSetters
    ++ ["}"]
  where
    fields = classFields clss
    fieldSetter fld = "this." ++ fieldName fld ++ "=" ++ fieldName fld ++ ";"
    fieldSetters = fmap fieldSetter fields

staticDeclarations :: Class -> [String]
staticDeclarations clss = [equalDeclaration, hashDeclaration, showDeclaration] >>= ($ clss)

equalDeclaration :: Class -> [String]
equalDeclaration clss =
  [ "/** The {@link " ++ className clss ++ "} {@link Equal} instance */",
    "public static final Equal<" ++ className clss ++ "> " ++ equalName clss ++ ";"
  ]

hashDeclaration :: Class -> [String]
hashDeclaration clss =
  [ "/** The {@link " ++ className clss ++ "} {@link Hash} instance */",
    "public static final Hash<" ++ className clss ++ "> " ++ hashName clss ++ ";"
  ]

showDeclaration :: Class -> [String]
showDeclaration clss =
  [ "/** The {@link " ++ className clss ++ "} {@link Show} instance */",
    "public static final Show<" ++ className clss ++ "> " ++ showName clss ++ ";"
  ]

equalName :: Class -> String
equalName = (++ "Equal") . toLowerHead . className

hashName :: Class -> String
hashName = (++ "Hash") . toLowerHead . className

showName :: Class -> String
showName = (++ "Show") . toLowerHead . className

toLowerHead :: String -> String
toLowerHead [] = []
toLowerHead (s : ss) = toLower s : ss

factoryMethod :: Class -> [String]
factoryMethod clss =
  [ "public static final " ++ name ++ " " ++ toLowerHead name ++ "(" ++ fieldParams (classFields clss) ++ "){",
    indent ("return " ++ constructorCall ++ ";"),
    "}"
  ]
  where
    name = className clss
    constructorCall = "new " ++ name ++ "(" ++ fieldList (classFields clss) ++ ")"
module Gen
  ( Field,
    Class,
  )
where

import Control.Monad (join)
import qualified Data.List as L

data Field = Field {fieldName :: String, fieldType :: Either String Class, description :: String}

data Class = Class {className :: String, classFields :: [Field]}

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

constructor :: Class -> [String]
constructor clss =
  ["private final " ++ className clss ++ "(" ++ params ++ ") {"]
    ++ fmap indent fieldSetters
    ++ ["}"]
  where
    fieldParam fld = "final " ++ fieldTypeName fld ++ " " ++ fieldName fld
    fields = classFields clss
    fieldSetter fld = "this." ++ fieldName fld ++ "=" ++ fieldName fld ++ ";"
    params = join $ L.intersperse ", " (fmap fieldParam fields)
    fieldSetters = fmap fieldSetter fields

fieldParams :: [Field] -> String
fieldParams fields = join (L.intersperse ", " (fmap fieldParam fields))
    where fieldParam fld = "final " ++ fieldTypeName fld ++ " " ++ fieldName fld
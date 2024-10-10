{-# LANGUAGE TemplateHaskell #-}

module Gen where

import Control.Lens
import Control.Monad (join)
import Data.Char (toLower)
import qualified Data.List as L
import System.IO

type Name = String

type Description = String

type TypeName = String

data Field = Field {_fieldName :: Name, _fieldType :: Either TypeName Class, _fieldDescription :: Description} deriving (Eq, Show)

data Class = Class {_className :: Name, _classFields :: [Field], _classDescription :: Description} deriving (Eq, Show)

makeLenses ''Class
makeLenses ''Field

fld :: Name -> TypeName -> Description -> Field
fld name typeName = Field name (Left typeName)

fld' :: Name -> Class -> Description -> Field
fld' name clss = Field name (Right clss)

writeClass :: Class -> IO ()
writeClass clss = writeFile ("generated/" ++ clss ^. className ++ ".java") (classString clss)

classString :: Class -> String
classString = unlines . outerClassLines

outerClassLines :: Class -> [String]
outerClassLines clss = standardImports ++ [""] ++ classLines False clss

innerClassLines :: Class -> [String]
innerClassLines = classLines True

standardImports :: [String]
standardImports =
  [ "import static fj.Equal.*;",
    "import static fj.Hash.*;",
    "import static fj.Show.*;",
    "import static fj.data.optic.Lens.lens;",
    "import static ca.cnphi.util.fj.DataConfigBuilder.buildDataConfig;",
    "import ca.cnphi.util.fj.Data;",
    "import ca.cnphi.util.fj.DataConfig;",
    "import fj.*;",
    "import fj.data.*;",
    "import fj.data.optic.Lens;"
  ]

classLines :: Bool -> Class -> [String]
classLines inner clss =
  classDoc clss
    ++ ["public " ++ (if inner then "static " else "") ++ "final class " ++ clss ^. className ++ " extends Data<" ++ _className clss ++ "> {"]
    ++ fmap indent (classBody inner clss)
    ++ ["}"]

classDoc :: Class -> [String]
classDoc clss = ["/** " ++ clss ^. classDescription ++ " */"]

classBody :: Bool -> Class -> [String]
classBody inner clss =
  staticDeclarations clss
    ++ staticInitializer clss
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
indent = ("    " ++)

fieldTypeName :: Field -> String
fieldTypeName = either id (^. className) . (^. fieldType)

fieldDeclaration :: Field -> [String]
fieldDeclaration fld =
  [ "/** " ++ _fieldDescription fld ++ " */",
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
  [ "private " ++ clss ^. className ++ "(" ++ fieldParams (clss ^. classFields) ++ ") {",
    indent "super(" ++ equalName clss ++ ", " ++ hashName clss ++ ", " ++ showName clss ++ ");"
  ]
    ++ fmap indent fieldSetters
    ++ ["}"]
  where
    fields = clss ^. classFields
    fieldSetter fld = "this." ++ fld ^. fieldName ++ "=" ++ fld ^. fieldName ++ ";"
    fieldSetters = fmap fieldSetter fields

staticDeclarations :: Class -> [String]
staticDeclarations clss = [equalDeclaration, hashDeclaration, showDeclaration, classLensLines] >>= ($ clss)

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
  factoryMethodDoc clss
    ++ [ "public static final " ++ name ++ " " ++ toLowerHead name ++ "(" ++ fieldParams (clss ^. classFields) ++ "){",
         indent ("return " ++ constructorCall ++ ";"),
         "}"
       ]
  where
    name = clss ^. className
    constructorCall = "new " ++ name ++ "(" ++ fieldList (clss ^. classFields) ++ ")"

factoryMethodDoc :: Class -> [String]
factoryMethodDoc clss =
  ["/**"]
    ++ fmap (\fld -> " * @param " ++ fld ^. fieldName ++ " " ++ fld ^. fieldDescription) (clss ^. classFields)
    ++ [" * @return A new {@link " ++ clss ^. className ++ "} instance"]
    ++ ["*/"]

classLensLines :: Class -> [String]
classLensLines clss = clss ^. classFields . traversed . to (fieldLens clss)

fieldLens :: Class -> Field -> [String]
fieldLens clss fld =
  [ "/** {@link Lens} to the {@link #" ++ fld ^. fieldName ++ "} field" ++ " */",
    "public static final Lens<" ++ clss ^. className ++ ", " ++ fieldTypeName fld ++ "> _" ++ fld ^. fieldName
      ++ " = "
      ++ "lens(s -> s."
      ++ fld ^. fieldName
      ++ ", a -> s -> new "
      ++ clss ^. className
      ++ "("
      ++ constructorParams
      ++ "));"
  ]
  where
    constructorParams = L.intercalate ", " $ clss ^.. classFields . traversed . fieldName . to fieldVariable
    fieldVariable n = if n == fld ^. fieldName then "a" else "s." ++ n

staticInitializer :: Class -> [String]
staticInitializer clss = ["static {"] ++ fmap indent (staticInitializerBody clss) ++ ["}"]

staticInitializerBody :: Class -> [String]
staticInitializerBody clss =
  [ "// @formatter:off",
    "final DataConfig<" ++ n ++ "> c = buildDataConfig(" ++ n ++ ".class)"
  ]
    ++ fmap indent (dataConfigBody clss)
    ++ [ "// @formatter:on",
         equalName clss ++ " = c.equal;",
         hashName clss ++ " = c.hash;",
         showName clss ++ " = c.show;"
       ]
  where
    n = clss ^. className

dataConfigBody :: Class -> [String]
dataConfigBody clss = fmap dataConfigField (clss ^. classFields) ++ [".build();"]

dataConfigField :: Field -> String
dataConfigField fld = ".withField(\"" ++ n ++ "\", s -> s." ++ n ++ ")"
  where
    n = fld ^. fieldName
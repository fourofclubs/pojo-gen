import Gen

main :: IO ()
main = putStrLn . unlines . classLines False $ class1

field1 :: Field
field1 = Field "field1" (Left "Option<String>") "The first test field"

field2 :: Field
field2 = Field "field2" (Left "Integer") "The second test field"

field3 :: Field
field3 = Field "field3" (Left "String") "A string field"

field4 :: Field
field4 = Field "field4" (Left "Double") "A double field"

field5 :: Field 
field5 = Field "field5" (Right class2) "An inner class field"

class1 :: Class
class1 = Class "TestClass" [field1, field2, field5]

class2 :: Class
class2 = Class "InnerClass" [field3, field4]
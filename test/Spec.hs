import Gen

main :: IO ()
main = putStrLn . unlines . classLines False $ class1

field1 = Field "field1" (Left "Option<String>") "The first test field"

field2 = Field "field2" (Left "Integer") "The second test field"

class1 = Class "TestClass" [field1, field2]
module Main where
import Data.Char (toUpper)

main :: IO ()
main = do
  putStrLn t1
  putStrLn t2
  
  
data TestResult = Pass | Fail
  deriving Eq

makeResult :: String -> TestResult -> String
makeResult testName testResult
  | testResult == Pass = concat ["[",testName,"]: ", "Pass"]
  | otherwise = concat ["[",testName,"]: ", "Fail"]

isEqual :: Eq a => [Char] -> a -> a -> [Char]
isEqual testName a b = if a == b 
                       then makeResult testName Pass
                       else makeResult testName Fail

isNotEqual :: Eq a => [Char] -> a -> a -> [Char]
isNotEqual testName a b = if a /= b
                          then makeResult testName Pass
                          else makeResult testName Fail
-- isNotEqual testName a b = if a /= b then "[" ++ testName ++ "]: " ++ "True" else "[" ++ testName ++ "]: " ++ "False"

t1In :: [Char]
t1In = "Whatever."


t1 :: [Char]
t1 = isNotEqual "t1" (map toUpper t1In) t1In

t2In :: [Char]
t2In = "WATCH OUT!"

t2 :: [Char]
t2 = isNotEqual "t2" (map toUpper t2In) t2In

-- "Whoa, chill out!"

{- ----------------------------------- -}

cleanString :: [Char] -> [Char]
cleanString xs = [i | i <- xs, i `notElem` lst]
  where lst = ['.','!','?',',','\'','%','^','*','@','#','$',':','(',')']

keep = ['A'..'Z'] ++ ['a'..'z'] ++ [' ', '-']
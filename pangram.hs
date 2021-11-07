module Bob (responseFor) where
import Data.Char (toUpper)


upperCaseString :: [Char] -> [Char]
upperCaseString = map toUpper

removePunctuation :: [Char] -> [Char]
removePunctuation str = [i | i <- str, i `elem` ['A'..'Z'] ++ ['a'..'z'] ++ " "]

cleanText :: [Char] -> [Char]
cleanText str = 

isAllCaps :: [Char] -> Bool
isAllCaps text = cleanText == upperCaseText
  where cleanText = removePunctuation text
        upperCaseText = upperCaseString cleanText
    
    
-- s = (removePunctuation . upperCaseString) text

endsWithQuestionMark :: [Char] -> Bool
endsWithQuestionMark str = last str == '?'

hasNoLetters :: [Char] -> Bool
hasNoLetters str = null [i | i <- str, i `elem` ['a'.. 'z']]

responseFor :: String -> String
responseFor xs
    -- 'Sure.' if you ask him a question. (ends w/ '?')
    | not (isAllCaps xs) && endsWithQuestionMark xs = "Sure."
    -- 'Whoa, chill out!' if YELL in all caps
    | isAllCaps xs && not (endsWithQuestionMark xs) = "Whoa, chill out!"
    -- 'Calm down, I know what I'm doing!' YELL A QUESTION '?'
    | isAllCaps xs && endsWithQuestionMark xs = "Calm down, I know what I'm doing!"
    -- 'Fine. Be that way!' if address w/p words, "", "  "
    | hasNoLetters xs = "Fine. Be that way!"
    -- 'Whatever.' for otherwise
    | otherwise = "Whatever."
    

    -- "1, 2, 3" -> "Whatever."

-- 'Sure.' if you ask him a question. (ends w/ '?')
t1 xs = not (isAllCaps xs) && endsWithQuestionMark xs
-- 'Whoa, chill out!' if YELL in all caps
t2 xs = isAllCaps xs && not (endsWithQuestionMark xs)
-- 'Calm down, I know what I'm doing!' YELL A QUESTION '?'
t3 xs = isAllCaps xs && endsWithQuestionMark xs
-- 'Fine. Be that way!' if address w/p words, "", "  "
t4 xs = hasNoLetters xs
-- 'Whatever.' for otherwise
--t5 xs = otherwise = "Whatever."
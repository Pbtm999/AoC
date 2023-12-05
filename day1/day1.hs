import Data.Char

listLast :: [a] -> a
listLast [] = undefined
listLast [x] = x
listLast (_:xs) = listLast xs

getDigitNonNumeric :: String -> Int
getDigitNonNumeric xs
    | take 3 xs== "one" = 1
    | take 3 xs == "two" = 2
    | take 5 xs == "three" = 3
    | take 4 xs == "four" = 4
    | take 4 xs == "five" = 5
    | take 3 xs == "six" = 6
    | take 5 xs == "seven" = 7
    | take 5 xs == "eight" = 8
    | take 4 xs == "nine" = 9
    | otherwise = 0

getOnlyNumbers :: [Char] -> [Int]
getOnlyNumbers [] = []
getOnlyNumbers (x:xs)
    | isDigit x = (digitToInt x):(getOnlyNumbers xs)
    | digit /= 0 = digit:(getOnlyNumbers xs) 
    | otherwise = getOnlyNumbers xs
    where digit = getDigitNonNumeric (take 5 (x:xs))


getNumberInLine :: [Char] -> Int
getNumberInLine [] = 0
getNumberInLine [x]
    | isDigit x = (digitToInt x)*10+(digitToInt x)
    | otherwise = 0
getNumberInLine xs 
    | nums == [] = n*10 + n 
    | otherwise = (n*10 + listLast nums)
    where (n:nums) = getOnlyNumbers xs

sumAll :: [String] -> Int
sumAll [] = 0
sumAll (x:xs) = getNumberInLine x + sumAll xs


main :: IO()
main = do
    filecontent <- readFile "input.txt"
    print (sumAll (lines filecontent))
    
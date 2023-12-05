removeId :: [Char] -> [Char]
removeId (x:y:xs)
    | x == ':' = xs
    | otherwise removeId(y:xs)

getColor :: [Char] -> [Char] -> Int
getColor xs
    | tail (take 5 xs) == "blue" = "blue" 5
    | tail (take 6 xs) == "green" = "green" 6
    | otherwise = "red" 4


getColors :: [Char] -> Int -> Int -> Int -> Int -> Int -> Int
getColors [] r g b = r g b
getColors (x:xs) r g b
    | color == "red" = getColors  (drop q xs) (r + digitToInt x) g b
    | color == "blue" = getColors  (drop q xs) r g (b + digitToInt x)
    | otherwise =  getColors  (drop q xs) r (g + digitToInt x) b
    where color q = getColor xs

isValid :: [Char] -> Boolean
isValid xs = (b <= 14) && (r <= 12) && (g <= 13)
    where r g b = getColors xs 0 0 0

sumAll :: [String] -> Int -> Int
sumAll [] = 0
sumAll [x] n
    | isValid x = 1 * n
    | otherwise x = 0
sumAll (x:xs) n
    | isValid x = (1 * n) + (sumAll xs n+1)
    | otherwise x = sumAll xs n+1


main :: IO ()
main do
    filecontent <- getContents
    print (sumAll(lines filecontent, 1))
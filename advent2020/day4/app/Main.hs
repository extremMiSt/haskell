module Main where
import Text.Read (readMaybe)

data Record = Record{byr::String, iyr::String, eyr::String, hgt::String, hcl::String, ecl::String, pid::String}
    deriving (Show,Eq)

splitRecords :: String -> [String]
splitRecords [] = []
splitRecords [x] = [[x]]
splitRecords (x:y:ys) | x /= '\n'  || y /= '\n' = (x:h) :t
                      | x == '\n'  && y == '\n' =  [] : splitRecords ys
                      where
                        t = tail (splitRecords (y:ys))
                        h = head (splitRecords (y:ys))

fromString :: String -> Record
fromString s = fromArgs args
    where 
        args = map (split ':') (concatMap words (lines s))
        fromArgs [] = Record "" "" "" "" "" "" ""
        fromArgs ((k,v):xs) = case k of
            "byr" -> (fromArgs xs){byr = v}
            "iyr" -> (fromArgs xs){iyr = v}
            "eyr" -> (fromArgs xs){eyr = v}
            "hgt" -> (fromArgs xs){hgt = v}
            "hcl" -> (fromArgs xs){hcl = v}
            "ecl" -> (fromArgs xs){ecl = v}
            "pid" -> (fromArgs xs){pid = v}
            x     -> fromArgs xs
  
split :: (Eq a) => a -> [a] -> ([a],[a])
split a (x:xs) = if x == a 
    then ([], xs) 
    else case split a xs of
        (a,b) -> (x:a, b)

isPresent :: Record -> Bool
isPresent r = not (null (byr r)) &&
             not (null (iyr r)) &&
             not (null (eyr r)) &&
             not (null (hgt r)) &&
             not (null (hcl r)) &&
             not (null (ecl r)) &&
             not (null (pid r)) 

isValid :: Record -> Bool
isValid r = isByrValid r &&
            isIyrValid r &&
            isEyrValid r &&
            isHgtValid r &&
            isHclValid r &&
            isEclValid r &&
            isPidValid r

isByrValid :: Record -> Bool
isByrValid r = case readMaybe (byr r) of
    Nothing -> False
    Just r -> 1920 <= r && r <= 2002

isIyrValid :: Record -> Bool
isIyrValid r = case readMaybe (iyr r) of
    Nothing -> False
    Just r -> 2010 <= r && r <= 2020

isEyrValid :: Record -> Bool
isEyrValid r = case readMaybe (eyr r) of
    Nothing -> False
    Just r -> 2020 <= r && r<= 2030

isHgtValid :: Record -> Bool
isHgtValid r = case last (hgt r) of
    'm' -> length (hgt r) == 5 && case readMaybe (take 3 (hgt r)) of
        Nothing -> False
        Just s -> 150<=s && s<=193
    'n' -> length (hgt r) == 4 && case readMaybe (take 2 (hgt r)) of
        Nothing -> False
        Just s -> 59<=s && s<=76
    x -> False
    
isHclValid :: Record -> Bool
isHclValid r =  length (hcl r) == 7 &&
                head (hcl r) == '#' &&
                isHex (tail (hcl r))
    where
        isHex [] = True
        isHex (x:xs) = x `elem` ['0','1','2','3','4','5','6','7','8','9',
                                'a','b','c','d','e','f'] &&
                       isHex xs
isEclValid :: Record -> Bool
isEclValid r = case ecl r of
    "amb" -> True
    "blu" -> True
    "brn" -> True
    "gry" -> True
    "grn" -> True
    "hzl" -> True
    "oth" -> True
    x -> False

isPidValid :: Record -> Bool
isPidValid r = length (pid r) == 9 &&
    case readMaybe (pid r)::Maybe Integer of
        Nothing -> False
        Just _ -> True

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    let rs = splitRecords f
    let present = filter isPresent (map Main.fromString rs)
    print (length present)
    let valid = filter isValid present
    print (length valid)
    return ()

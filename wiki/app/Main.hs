{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.HTTP.Req
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup.Match
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Control.Concurrent
import Network.URI.Encode
import qualified Data.Set as S

wikiLinks :: String -> IO [String]
wikiLinks  site = runReq defaultHttpConfig $ do
    bs <- req GET (https "de.wikipedia.org" /: "wiki" /: T.pack (decode site)) NoReqBody bsResponse mempty
    let article = [x | x@(TagBranch "div" attr _) <- universeTree (parseTree (responseBody bs))]
    return $ map (drop 6) $ filter (startsWith "/wiki/") [B.unpack b | (TagBranch "a" [("href",b), ("title",_)] _) <- universeTree article]

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x:xs) (y:ys) | x == y = startsWith xs ys
                         | otherwise = False


loop :: [[String]] -> [Char] -> S.Set String-> IO [String]
loop (l:ls) t known = do
    print l
    if t == head l then do
        return l
    else do
        news <- wikiLinks (head l)
        let nnews = map (:l) (filterWiki news known)
        loop (ls++nnews) t (S.union known (S.fromList news))

filterWiki :: [String] -> S.Set String-> [String]
filterWiki l known = res 
    where
        res = r5
        r0 = filter (\x -> not (x `S.member` known)) l
        r1 = filter (not . startsWith "Benutzer:") r0
        r2 = filter (not . startsWith "Wikipedia:") r1
        r3 = filter (not . startsWith "Kategorie:") r2
        r4 = filter (not . startsWith "Hilfe:") r3
        r5 = filter (not . startsWith "Portal:") r4

main :: IO ()
main = do 
    r <- loop [["Thomas_Tallis"]] "James_Callis" S.empty
    print r
    
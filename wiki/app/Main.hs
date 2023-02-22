{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.HTTP.Req
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup.Match
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B

wikiLinks :: IO()
wikiLinks = runReq defaultHttpConfig $ do
    bs <- req GET (https "de.wikipedia.org" /: "wiki" /: "Chromium_(Browser)") NoReqBody bsResponse mempty--http://de.wikipedia.org/wiki/Chromium_(Browser)
    let article = [x | x@(TagBranch "div" attr _) <- universeTree (parseTree (responseBody bs))]
    let a = map (drop 6) $ filter (startsWith "/wiki/") [B.unpack b | (TagBranch "a" [("href",b), ("title",_)] _) <- universeTree article]
    liftIO $ print a

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x:xs) (y:ys) | x == y = startsWith xs ys
                         | otherwise = False

main :: IO ()
main = do
    wikiLinks
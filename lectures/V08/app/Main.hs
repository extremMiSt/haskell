module Main where
import Test.QuickCheck (sample')
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen



copyFile :: String -> String -> IO ()
copyFile source target = readFile source >>= \xs -> writeFile target xs

copyFileDo :: String -> String -> IO ()
copyFileDo s t = do
    xs <- readFile s
    writeFile t xs

doTwice :: IO b -> IO (b,b)
doTwice io = io >>= \a1 -> io >>= \a2 -> return (a1,a2)

doTwiceDo :: IO b -> IO (b, b)
doTwiceDo io = do
    a <- io
    b <- io
    let x = 2*2
    return (a,b)

doNot :: IO a -> IO ()
doNot io = return ()

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    x <- sample (choose (1::Integer,5))
    print x

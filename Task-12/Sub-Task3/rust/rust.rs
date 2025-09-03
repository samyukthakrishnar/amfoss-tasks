import System.IO

rust :: Int -> IO ()
rust n = do
    mapM_ putStrLn [row i | i <- [1..n-1]]
    putStrLn (replicate (2*n - 1) '*')
  where
    row 1 = replicate (n-1) ' ' ++ "*"
    row i = replicate (n-i) ' ' ++ "*" ++ replicate (2*i - 3) ' ' ++ "*"

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let n = read (head (lines contents)) :: Int
    rust n

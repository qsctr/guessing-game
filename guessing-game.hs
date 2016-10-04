import Data.Char

data Tree = Branch String Tree Tree | Leaf String deriving (Show, Read)

main :: IO ()
main = putStrLn "Enter file to read" >> getLine >>= run
    where   run file = do
                putStrLn "Think of a thing that is in the topic"
                readFile file >>= play . (read :: String -> Tree) >>= writeFile file . show
                yesOrNo "Play again" (run file) (return ())

play :: Tree -> IO Tree
play (Branch b n y) = yesOrNo b (Branch b n <$> play y) (flip (Branch b) y <$> play n)
play (Leaf l) = yesOrNo ("Is it " ++ l) (return (Leaf l)) $ do
    newLeaf <- putStrLn "What is it?" >> getLine
    newBranch <- putStrLn ("What makes it different from " ++ l ++ "?") >> getLine
    return (Branch newBranch (Leaf l) (Leaf newLeaf))

yesOrNo :: String -> IO a -> IO a -> IO a
yesOrNo question y n = putStrLn (question ++ "? (Y/N)") >> map toUpper <$> getLine >>= yn
    where   yn 'Y' = y
            yn 'N' = n
            yn _ = putStrLn "You can only answer Y/N" >> yesOrNo question y n
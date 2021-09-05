module Main where

import System.IO

main :: IO ()
main = do
    mainLoop []
    return ()

mainLoop :: Stack -> IO Stack
mainLoop sk = do
    (action, args, iarg) <- getUserInput
    if (action == "exit")
        then return []
    else do
        let (newsk, itms) = if (action == "add")
            then (addToStack sk args, [])
            else popStack sk iarg
        if (length itms > 0)
            then putStrLn $ unwords itms
            else return ()
        mainLoop newsk

prompt :: String -> IO String
prompt str = do
    putStr str
    hFlush stdout
    getLine

getUserInput :: IO (String, [String], Int)
getUserInput = do
    str <- prompt "stackcmd> "
    let
        ws = words str
        f = if (length str > 0) then head ws else ""
        xs = if (length str > 0) then tail ws else []
        iarg = getIntArg xs
    if (shouldExit f xs || f == "add")
        then return (f, xs, 0)
    else if (f == "take" && iarg > -1)
        then return (f, [], iarg)
    else do
        if (f == "help")
            then putStrLn "[commands]      [description]                       [example]\n- add [item]    add following items to stack        add 1 2 3 4\n- exit          exit program\n- help          list commands\n- take n        take n items off stack and print    take 4"
            else putStrLn $ "** Invalid command '" ++ str ++ "' **\n    try 'help' for full list of commands"
        getUserInput

shouldExit :: String -> [String] -> Bool
shouldExit f xs = f == "exit" && xs == []

getIntArg :: [String] -> Int
getIntArg xs
    | length xs /= 1 = -1
    | otherwise = int
    where
        r = reads (head xs) :: [(Int, String)]
        (x, str) = if r == [] then (-1, "") else head r
        int = if str /= "" then -1 else x

type Stack = [String]

addToStack :: Stack -> [String] -> Stack
addToStack [] as = reverse as
addToStack xs as = (reverse as) ++ xs

popStack :: Stack -> Int -> (Stack, [String])
popStack [] _ = ([], [])
popStack xs i = (drop i xs, take i xs)

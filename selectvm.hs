module Main where
import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import System.Process (spawnCommand, runCommand)
--import Data.List.Split
append = (++)
rofify :: String -> String
rofify name = foldl1 append [tail . snd . break ('-' ==) . takeBaseName $ name, "\0icon\x1f", name, "\n"]
options = [ "-Linux.svg", "-Windows.svg", "-Power.svg"]


sshHack = "sshpass -pstudent ssh student@localhost DISPLAY=:0 "
fullScreenVM = "VirtualBoxVM --fullscreen --startvm "

makeCom :: String -> String
makeCom x
    | x == "Linux"     = fullScreenVM ++ "Linux"
    | x == "Windows"   = fullScreenVM ++ "\"УВМ\\ -\\ Windows\\ v1\""
    | x == "Выключить" = "systemctl poweroff"
    | otherwise = ""

main :: IO ()
main = do
    args <- getArgs
    if length args == 0
        then putStrLn . foldl1 append . map rofify $ options
        else do 
            d <- spawnCommand . (sshHack ++) . (++" &") . makeCom . head $ args
            runCommand "pkill rofi" -- idiotic way
            putStrLn ""

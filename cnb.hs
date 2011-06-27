import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit

server  = "irc.epd-me.net"
port    = 6667
chan    = "#jeena"
nick    = "cnb"

main = do
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  write h "NICK" nick
  write h "USER" (nick ++ " 0 * :clynx Nerv-Bot")
  write h "JOIN" chan
  listen h
  
write :: Handle -> String -> String -> IO ()
write h s t = do
  hPrintf h "%s %s\r\n" s t
  printf    "> %s %s\n" s t
  
listen :: Handle -> IO ()
listen h = forever $ do
    t <- hGetLine h
    let s = init t
    if ping s then pong s else eval h (clean s)
    putStrLn s
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write h "PONG" (':' : drop 6 x)
    
eval :: Handle -> String -> IO ()
eval h "!quit" = write h "QUIT" ":Exiting" >> exitWith ExitSuccess
eval h "!jump" = privmsg h "The quick brown clynx jumps over the lazy oak."
eval h "!implode" = write h "QUIT" ":whhooooooshhhh ..."
eval h "!explode" = write h "QUIT" ":kaBOOooOOOOoommm ..."
eval h x | (nick ++ ":") `isPrefixOf` x = privmsg h "Halt die Fresse!"
eval h x | and [(not (isInfixOf "@clynx" x)), (isInfixOf "clynx" x)] =
  privmsg h "Der clynx kann nix!"
eval _ _ = return ()

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++s)

import Network
import System.IO
import Text.Printf
import Data.List
import Data.Char
import System.Exit
import Control.Monad.State
import Text.Regex.Posix
import Data.Char (isSpace)

server  = "irc.epd-me.net"
port    = 6667
chan    = "#selfhtml"
nick    = "anna"
rname   = "Boten Anna"

type Nick = String
data Type = Kick | Invite | Privmsg | Unknown
  deriving (Show)
  
type Text = String
data Message = Message Nick Type Text
  deriving (Show)

type Key = String
type Value = String
type Store = [(Key, Value)]

type MyState = (Store, Handle)
type MyStateM = StateT MyState IO

main :: IO ()
main = do
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  let state = (startStore, h)
  runStateT (login >> listen) state >> return ()
  
login :: MyStateM ()
login = do
  write "NICK" nick
  write "USER" (nick ++ " 0 * :" ++ rname)
  joinChan
  return ()
  
joinChan :: MyStateM ()
joinChan = do
  write "JOIN" chan
  
getHandle :: MyStateM (Handle)
getHandle = do
  (_, h) <- get
  return h

putHandle :: Handle -> MyStateM ()
putHandle h = do
  (s, _) <- get
  put (s, h)
  return ()
  
getStore :: MyStateM (Store)
getStore = do
  (s, _) <- get
  return s
  
putStore :: Store -> MyStateM ()
putStore s = do
  (_, h) <- get
  put (s, h)
  return ()
  
putKeyValue :: String -> String -> MyStateM ()
putKeyValue k v = do
  s <- getStore
  putStore (s ++ [(k,v)])
  return ()
  
deleteKeyValue :: String -> MyStateM ()
deleteKeyValue key = do
  s <- getStore
  let sn = [(k,v) | (k,v) <- s, k /= key]
  putStore (sn)
  
match :: String -> MyStateM (Maybe Value)
match s = do
  st <- getStore
  case (find (\(key, value) -> s =~ key) st) of
    Nothing -> return Nothing
    Just (k,v) -> return (Just v)
  
write :: String -> String -> MyStateM ()
write s t = do
  h <- getHandle
  liftIO $ hPrintf h "%s %s\r\n" s t
  
listen :: MyStateM ()
listen = forever $ do
    h <- getHandle
    t <- liftIO $ hGetLine h
    let s = init t
    if ping s
      then pong s
      else do eval (parse s)
    -- liftIO $ putStrLn s
    return ()
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)
    
eval :: Maybe Message -> MyStateM ()
eval (Just m@(Message n Privmsg te)) = evalPrivmsg (Just m)
eval (Just (Message _ Invite _)) = invited
eval _ = return ()

evalPrivmsg :: Maybe Message -> MyStateM ()
evalPrivmsg Nothing = return ()
evalPrivmsg (Just m@(Message n _ te)) = do
  let pat = nick ++ ":(.*)"
  let r = te =~ pat :: [[String]]
  case r of
    [] -> do m <- match te
             case m of
               Just v -> privmsg v
               Nothing -> return ()
    a  -> do let s = last $ head a
             action (trim s) m
             return ()
             
action :: String -> Message -> MyStateM ()
action s (Message n _ _) | "add " `isPrefixOf` s = do
  let (k,v) = parseKeyVal $ rest "add" s
  putKeyValue k v
  msg (k ++ "~" ++ v ++ " added") n
  return ()
action s (Message n _ _) | "delete " `isPrefixOf` s = do
  let k = rest "delete" s
  deleteKeyValue k
  msg (k ++ " deleted") n
  return ()  
action "list" (Message n _ _) = do
  st <- getStore
  mapM (\(k,v) -> msg (k ++ "~" ++ v) n) st
  return () 
action "leave" (Message n _ _) = do
  leave ("Fuck you " ++ n ++ ", I'm quitting!")
action "implode" (Message n _ _) = do
    leave ("whoooooooshhhhh ...")
action "explode" (Message n _ _) = do
    leave ("KaaboooOOOOOooooommm ...")
action "help" (Message n _ _) = help n
action "cleanup" (Message n _ _) = cleanup n
action s (Message n _ _) = do
  case (length s) of
    0 -> privmsg "What?!"
    _ -> privmsg ("Fuck you " ++ n ++ "! " ++
                  sanit (capitalize s) ++ " yourself!")
  where
    capitalize (x:xs) = toUpper x : xs
    sanit xs = case [(last xs)] `isInfixOf` ".:!?" of
                    True -> take (length xs - 1) xs
                    False -> xs
  
rest :: String -> String -> String
rest k s = drop (length k + 1) s

privmsg :: String -> MyStateM ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

msg :: String -> Nick -> MyStateM ()
msg s n = write "PRIVMSG" (n ++ " :" ++ s)

leave :: String -> MyStateM ()
leave s = do
  privmsg s
  write "PART" (chan ++ " :")
  
invited :: MyStateM ()
invited = joinChan

parse :: String -> Maybe Message
parse s = do
          -- :Jeena!~Jeena@host-3AB762DD.defunced.de PRIVMSG #jeena :test
  let pat = ":([a-zA-Z0-9].+)!.+ ([A-Z].+) " ++ chan ++ " :(.*)"
  let q = s =~ pat :: [[String]]
           -- :Jeena!~Jeena@host-3AB762DD.defunced.de INVITE cnb :#jeena
  let pat2 = ":([a-zA-Z0-9].+)!.+ ([A-Z].+) " ++ nick ++ " :" ++ chan
  let q2 = s =~ pat2 :: [[String]]
  case q of
      [] -> do case q2 of
                 [] -> Nothing
                 r2 -> do let a2 = tail $ head r2
                          case (head $ tail a2) of
                                "INVITE" -> Just (Message (head a2) Invite (last a2))
                                _ -> Nothing
      r -> do let a = tail $ head r
              case (head $ tail a) of
                    "KICK" -> Just (Message (head a) Kick (last a))
                    "PRIVMSG" -> Just (Message (head a) Privmsg (last a))
                    _ -> Nothing

parseKeyVal :: String -> (Key, Value)
parseKeyVal s = do
  let a = wordsBy (=='~') s
  (head a, unwords $ tail a)
  
wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy sep str = s_skip str
   where s_skip []     = []
         s_skip (c:cs) = if sep c then s_skip cs else s_word cs [c]
         s_word []     w = [reverse w]
         s_word (c:cs) w = if sep c then reverse w : s_skip cs
			  else s_word cs (c:w)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
   
help :: Nick -> MyStateM ()
help n = do
  privmsg "Heaven helps those who help themselves."
  msg ("usage:") n
  msg (" " ++ nick ++ ": help -> this help") n
  msg (" " ++ nick ++ ": add regex~value -> adds a value for a regex") n
  msg (" " ++ nick ++ ": delete regex -> deletes the regex-value pair") n
  msg (" " ++ nick ++ ": list -> lists all available regex-value pairs") n
  msg (" " ++ nick ++ ": cleanup -> restores the list to the default") n  
  msg (" " ++ nick ++ ": leave -> bot leaves the chanel") n
  msg (" " ++ nick ++ ": explode -> bot leaves the chanel") n
  msg (" " ++ nick ++ ": implode -> bot leaves the chanel") n
  msg (" /invite " ++ nick ++ " " ++ chan ++ " -> invites the bot back to the chanel") n
  
cleanup :: Nick -> MyStateM ()
cleanup n = do
  putStore startStore
  privmsg "Ok, I've cleaned up the bloody list."
  
startStore :: Store
startStore = [
    ("^jump$", "The quick brown clynx jumps over the lazy oak."),
    ("^ok$", "ok"),
    ("(J|j)eena,? arbeitest du", "Jeena, sag doch mal."),
    -- (nick, "Keine ungefragten queries!"),
    ("(php|PHP)", "'PHP' <- Ha ha!</nelson>"),
    ("(E|e)rlang", "Oh yeah!"),
    ("(H|h)askell", "Ich bin in Haskell geschrieben."),
    ("danke", "np"),
    ("^np$", "schmück dich nicht mit fremden Federn."),
    ("^hehe$", "was gibt es da zu lachen?")
  ]
  
-- ü
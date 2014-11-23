{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C

main :: IO () 
main = do
	b <- getTweet "https://twitter.com/YELLEtweets/status/535845076316860417"
	case b of
		Nothing -> putStrLn "nothing to pring"
		Just body -> putStrLn body

getTweet :: String -> Maybe String
getTweet url = do
	initReq <- parseUrl url
	let req = initReq { secure = True } -- Turn on https
	response <- withManager $ httpLbs req
	body <- responseBody response
	case body of
		empty -> return Nothing
		_ -> return Just (L.unpack body)

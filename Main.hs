{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import CSV
import System.Environment
import qualified System.IO as S
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Char8 (pack, unpack)
import Network.HTTP.Types.Status
import Data.ByteString(ByteString)

import Https
import HtmlTransform

-- | Reads from the command line. Usage: tx-update $file
main :: IO ()
main = do
  args <- getArgs
  case args of
   [f] -> processFile f     
   _ -> do     
     accounts <- accountsPage
     files <- case accounts of
           OK200 html cookie _ -> accountCSVs html cookie
           Error response -> return []
     case files of
        [] -> putStrLn "Failed to update"
        fs -> mapM_ processFile fs
     putStrLn "Usage: tx-update $file"

-- 2. http
-- type CsvFile = [Char]
type AccountId = String

-- | Returns pages of account states given account page
accountCSVs :: Html -> CookieJar -> IO [FilePath]
accountCSVs accountPage cookie = do
            putStrLn $ "Pidiendo " ++ (show . length $ accountIds accountPage) ++ " cuentas ..."
            let accounts = accountIds accountPage
            mapM (accountState cookie) accounts
                
getLocation :: IO (Maybe Request)
getLocation = do
      request <- parseUrl "https://www.sucursalelectronica.com/redir/redirect.go"
      response <- withManager $ \manager -> httpLbs request manager
      return $ getRedirectedRequest request (responseHeaders response) (responseCookieJar response) (statusCode . responseStatus $ response)
      
prependHost :: String -> String
prependHost s = "https://bwww.sucursalelectronica.com" ++ s

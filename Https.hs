{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--https://github.com/snoyberg/http-client/blob/ae82e09c4658babf0aca7edd98669f0b941beeae/http-client/Network/HTTP/Client/Types.hs
module Https where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Data.ByteString(ByteString)
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import HtmlTransform

-- Control
import Data.Maybe(fromJust)
import Data.Time.Clock

-- Data
import Data.Conduit
import Data.Conduit.Binary(sinkFile)
import qualified Data.String.Utils as String

import Network.HTTP.Client.MultipartFormData

-- 2. http
type CsvFile = String
type AccountId = String
type HttpUrl = String
type HttpParam = (ByteString, Maybe ByteString)
type HttpParams = [HttpParam]

data Page a = OK200 { html :: Html, cookie :: CookieJar, result :: a } | Error { errorResponse :: Response Html }
                         
type SessionWith a = Page a

type History resType = HistoriedResponse resType
type HistoryPages = [(Request, Response C.ByteString)]
type Inquire = (Request, CookieJar) --as a noun, not a verb

makeHistory :: Request -> IO (History BodyReader)
makeHistory r = newManager tlsManagerSettings >>= responseOpenHistory r

historyInquire :: (History a -> Bool) -> History a -> Maybe Inquire
historyInquire f h | f h = return (fst . last . pages $ h, getCookieJar h)
                   | otherwise = Nothing
  
pages :: History body -> HistoryPages
pages = hrRedirects 

printHistory :: History a -> IO ()
printHistory h = let paths = map (path . fst) (pages h) in  mapM_ B.putStrLn paths

getCookieJar :: History a -> CookieJar
getCookieJar history = let cj = responseCookieJar $ snd $ last $ pages $ history in createCookieJar [head $ destroyCookieJar cj]

jSession :: CookieJar -> String
jSession c = case filter (\c -> cookie_name c == (pack "JSESSIONID")) (destroyCookieJar c) of
     [js] -> unpack $cookie_value js
     _ -> ""
     
-- | <p><strong>Step 0. </strong>
-- Credentials
-- </p>
accountsPageParams :: [(ByteString, Maybe ByteString)]
accountsPageParams = [
    ("country", Just "CR"),
    ("exchangeRateCurrency", Just "USD"),
    ("product", Just "<username>"),
    ("pass", Just "<password>"),
    ("passtemp", Just "<password>"),
    ("token", Just ""),
    ("confirm.x", Just "0"),
    ("confirm.y", Just "0"),
    ("usuario", Just "<username>),
    ("clave", Just "<password>")
    ]

-- | Returns a request. This request has two response redirect paths:
--   1. Redirect to "showSessionRestriction.go". It means session is handled to another device (and in that case, will invalidate the other device's session)
--   2. Redirect to "login.go". It means a redirect up to accounts page
accountsPageRequest :: IO Inquire
accountsPageRequest = do
  putStr "Requiriendo login ... "
  m <- newManager tlsManagerSettings
  loginRequest <- parseUrl "https://www.sucursalelectronica.com/redir/redirect.go" >>= return . setQueryString accountsPageParams

  -- follow redirects
  loginResponse <- makeHistory loginRequest
  
  let
      redirections = hrRedirects loginResponse
      loginSucceded h = length (pages h) > 2      
      inquire = historyInquire loginSucceded loginResponse
  
  --case loginSucceded of
  case inquire of
       Just i -> do
            putStrLn "OK"
            printHistory loginResponse
            return i
       Nothing -> do                              
                putStr "Invalidando ..."                    
                invalidateSessionRequest <- parseUrl "https://www1.sucursalelectronica.com/ebac/common/sessionRestriction.go"                
                invalidateSessionResponse <- responseOpenHistory invalidateSessionRequest { cookieJar = Just $ getCookieJar loginResponse } m       
                
                let
                    redirections' = hrRedirects invalidateSessionResponse
                    loginSucceded' = length redirections' > 2
                    paths = map (path . fst) redirections'
                mapM_ B.putStrLn paths
                
                case loginSucceded' of
                     True -> do
                          putStrLn "OK"
                          saveResponseBody (snd . last . hrRedirects  $ invalidateSessionResponse) "invalidacion.html"
                          return (fst $ last redirections', getCookieJar invalidateSessionResponse)
                     False -> do
                           putStrLn $ "Fallamos .. "
                           
                           let paths = map (path . fst) redirections'
                           mapM_ B.putStrLn paths
                           
                           putStrLn "querying.. " 
                           B.putStrLn (path . fst . last $ redirections')
                           let lastReq  = fst $ last redirections'
                               
                           lastResponse <- withManager tlsManagerSettings $ httpLbs lastReq { cookieJar = Just . getCookieJar $ invalidateSessionResponse } 
                           let lastResp = snd $ last redirections'
                               
                           saveResponseBody lastResponse "failed-login-again.html"
                           saveResponseBody lastResp "last-response.html"
                           let paths' = map (path . fst) redirections'


                           C.putStrLn $ (responseBody $ lastResponse) 
                           mapM_ B.putStrLn paths'
                             
                           return . fst $ last redirections'
                           
                           error "ERROR"
  
-- | returns the html of accounts page
accountsPage :: IO (SessionWith [Account])
accountsPage = do
      (req, cookieJar) <- accountsPageRequest
      accountsPageResponse <- withManager tlsManagerSettings $ httpLbs req { cookieJar = Just cookieJar} 

      accountsPageResp' <- makeHistory req
      let
        redirections' = hrRedirects accountsPageResp'
      
      putStr "Viendo cuentas ... "
      putStrLn . unpack . statusMessage . responseStatus $ accountsPageResponse
      
      let html   = C.unpack $ responseBody accountsPageResponse
          status = responseStatus accountsPageResponse
          cookieJar' = getCookieJar accountsPageResp' --responseCookieJar $ accountsPageResponse

      saveResponseBody accountsPageResponse ("data/accountsPage.html")
      
      let
          accounts = parseAccounts html
          
      case status of
           Status 200 _ -> return $ OK200 html cookieJar' accounts
           _ -> return $ Error accountsPageResponse { responseBody = html }             
 -- | Visits an account page and downloads state as CSV 
accountState :: CookieJar -> AccountId -> IO CsvFile
accountState cookieJar accountId = do
        time <- getCurrentTime
        accountRequest <- parseUrl "https://www1.sucursalelectronica.com/ebac/module/accountstate/accountState.go" >>= return . urlEncodedBody [("productId",pack accountId)]

        let addHeaders h r = r { requestHeaders = h ++ requestHeaders r }
            headers = [("Cookie",pack ("JSESSIONID=" ++ jSession cookieJar))
                      ,("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36")
                      ,("Content-Type", "application/x-www-form-urlencoded")
                      ,("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
                      
                      ,("Cache-Control", "max-age=0")
                     ]

        putStr $ "Trayendo cuenta " ++ accountId ++ " ... "        
        accountResponse <- withManager tlsManagerSettings . httpLbs . addHeaders headers $ accountRequest        
        saveResponseBody accountResponse ("data/" ++ accountId ++ ".html")
         
        -- download csv
        putStr $ "Descargando CSV ... "
        downloadReq <- parseUrl "https://www1.sucursalelectronica.com/ebac/module/bankaccountstate/download.go" >>= return . urlEncodedBody [("fileType",pack "0")]                    
        downloadResponse <- withManager tlsManagerSettings . httpLbs . addHeaders headers $ downloadReq

        let filename = ("data/" ++ accountId ++ ".csv")
        saveResponseBody downloadResponse filename        
        putStrLn . unpack . statusMessage . responseStatus $ accountResponse
        return $ filename
        
        --return . C.unpack . responseBody $ downloadResponse
            
makeRequest :: CookieJar -> HttpUrl -> HttpParams -> IO Request
makeRequest cookie url params = do
        r <- parseUrl url
        return $ setQueryString params (r { cookieJar = Just cookie })
            
saveResponseBody :: Response C.ByteString -> FilePath -> IO ()
saveResponseBody c f = writeFile f (C.unpack . responseBody $ c) 

downloadParams = liftAsParam "consolidationCurrency" "USD"

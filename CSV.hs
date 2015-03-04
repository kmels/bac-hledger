module CSV where

import Text.CSV
import Text.Regex(mkRegex, matchRegex)
import System.IO (openFile, hFlush, hGetEncoding, hSetEncoding, hGetContents, IOMode(ReadMode), Handle, utf8 )

-- | Prints a ledger file given a csv file
processFile :: FilePath -> IO ()
processFile f = do
    putStrLn $ "Procesando archivo" ++ f ++ " ..."
    h <- openFile f ReadMode
    maybeEncoding <- hGetEncoding h    
    records' <- parseCSVFromFile f
    case records' of
        Right records -> process records
        Left parseError -> print parseError            

-- 1. CSV
-- | Formats a string. If it       
format :: String -> String
format input =
  let
    date = mkRegex "([0-9]{2})/([0-9]{2})/([0-9]{4})"
  in case (matchRegex date input) of
     Just [day,month,year] -> year ++ "/" ++ month ++ "/" ++ day
     _ -> input

process :: [Record] -> IO ()
process [] = return ()
process (r:rs) = handle r >> process rs

handle :: Record -> IO ()
handle fs@[date,ref,tipo,desc,debito," 0.00",balance] = do
  putStrLn $ format date ++ " " ++ desc 
  handleDebit desc debito >> putStrLn ""
  putStrLn $ ";now " ++ balance
  putStrLn ""
  
handle fs@[date,ref,tipo,desc," 0.00", credito, balance] = do
  putStrLn $ format date ++ " " ++ desc 
  handleDebit desc credito >> putStrLn ""
  putStrLn $ ";now Q" ++ balance
  putStrLn ""
  
--handle fields = unhandable fields
handle _ = return ()

unhandable :: Record -> IO ()
unhandable fields = do
  putStrLn "; Not processable: "
  putStrLn . show $ fields

handleDebit :: String -> String -> IO ()
handleDebit desc amount = do
  putStrLn $ "\tGastos:Admin       " ++ amount
  --putStrLn $ "\t" ++ rewrite desc ++ "           Q " ++ amount
  putStrLn $ "\tActivos:Banco:Mes-12-Diciembre"

handleCredit :: String -> String -> IO ()
handleCredit desc amount = do
  putStrLn $ "\tIngresos:Admin       " ++ amount
  putStrLn $ "\tActivos:Banco:Mes-12-Diciembre      Q " ++ amount

{-
type Accountable = (String, String)

accountables :: [Accountable]
accountables = [
  ("RETIRO ATM", "Gastos:Efectivo")  
  ]
-}               
rewrite desc = desc

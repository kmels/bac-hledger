module HtmlTransform where

-- 3. html
import Text.HTML.TagSoup
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

-- from accounts page
type ProductId = String
type Html = String
data Account = Account { accountName :: String, accountBalance :: Double }

liftAsParam :: String -> String -> [(ByteString, Maybe ByteString)]
liftAsParam paramName val = [(pack paramName, Just . pack $ val)]

accountIds :: String -> [ProductId]
accountIds html = map extractProductId actionForms
      where
        tags = parseTags html
        forms = sections (isTagOpenName "form") tags
        actionForms = filter (\f -> fromAttrib "action" (head f) == "/ebac/module/accountstate/accountState.go") forms
        extractProductId f = fromAttrib "value" $ head $ filter (isTagOpenName "input") f

parseAccounts :: Html -> [Account]
parseAccounts h = []

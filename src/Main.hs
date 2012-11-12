{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Types
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.Templating.Heist.TemplateDirectory
import qualified Data.ByteString.Char8 as B

import           Data.Map
import           Data.Maybe
import           Data.DateTime

import           Glue
import           Server
import           Splices
import           Common
import           DataStore
import           Data.Digest.OpenSSL.MD5

import           Control.Monad.Trans

main :: IO ()
main = do
    let myHeistState = foldr (\(x,y) -> bindSplice x y) emptyTemplateState spliceList
    td <- newTemplateDirectory' "templates" myHeistState
    quickServer $ templateHandler td defaultReloadHandler $ \ts ->
        templateServe ts <|>
        route [ ("foo", writeBS "bar")
              , ("view/:parm", postHandler ts)
              , ("list/:tag/:sort/:count", listHandler ts)
              , ("list/:tag/:sort", listHandler ts)
              , ("list/:tag", listHandler ts)
             -- , ("show_item.html", writeBS "old")
              , ("addComment", commentHandler)
              ] <|>
        fileServe "static"

commentHandler :: Snap ()
commentHandler = do 
    req <- getRequest
    let postData = rqParams req
    date <- liftIO getCurrentTime
    let host  = mapName $ B.unpack (rqServerName req)
    let ip = B.unpack $ rqRemoteAddr req
    let content = getParam "comment" postData
    let content' = replaceAll content "\n" "<BR>"
    let name = getParam "name" postData
    let mail = getParam "email" postData
    let tsg = getParam "timestampG" postData
    let ts = B.unpack $ head (fromJust ((Data.Map.lookup (B.pack "timestamp") postData)))
    let post = B.unpack $ head (fromJust ((Data.Map.lookup (B.pack "post") postData)))
    let captcha = B.unpack $ head (fromJust ((Data.Map.lookup (B.pack "captcha") postData)))
    let id = md5sum $ B.pack  ( ip ++ name ++ mail ++ show date )
    let comm  = Comment id name ip post (take dateLen (show date)) mail content
    let isValid = validComment date tsg ts captcha post
    if isValid 
       then liftIO $ addComment host post comm
       else liftIO $ putStrLn ("Possibly bad: " ++ show comm)
    writeBS $ B.pack content' -- (show comm ++ "--" ++ tsg ++ "--" ++ ts ++ "--" ++ captcha ++ "--" ++ post ++ "--" ++ show isValid)
  where getParam x table = B.unpack $ head (fromJust ((Data.Map.lookup (B.pack x) table)))

validComment :: DateTime -> String -> String -> String -> String -> Bool
validComment curTime tsGiven tsHash "" post | md5sum (B.pack (salt ++ tsGiven)) /= tsHash = False
                                            | otherwise = checkTime 
  where givenTime = read tsGiven :: DateTime
        checkTime = isInRange $ diffSeconds curTime givenTime
        isInRange x | x < 5     = False
                    | x > 600   = False
                    | otherwise = True 
validComment _ tsGiven tsHash _  post = False 

listHandler :: TemplateState Snap -> Snap ()
listHandler ts = do
    tag <- getParam "tag"
    count <- getParam "count"
    sort <- getParam "sort"
    let count' = maybe (0) (\x -> read (B.unpack x) :: Int) (count)
    let sort' = maybe (Date) (\x -> read (B.unpack x) :: SortType) (sort)
    maybe (writeBS "must specify echo/param in URL")
          (renderList ts count' sort') tag

postHandler :: TemplateState Snap -> Snap ()
postHandler ts = do
    param <- getParam "parm"
    maybe (writeBS "must specify a post")
          (renderPost ts) param

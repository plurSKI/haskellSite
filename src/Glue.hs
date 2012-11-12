{-# LANGUAGE OverloadedStrings #-}
module Glue
    ( templateHandler
    , defaultReloadHandler
    , templateServe
    , render
    , renderPost
    , renderList
    ) where

import           Maybe
import           Control.Applicative
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Prelude hiding (catch)
import           Snap.Types hiding (dir)
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.Templating.Heist.TemplateDirectory

import           Splices
import           DataStore
import           Control.Monad.Trans
import           Common

templateHandler :: TemplateDirectory Snap
                -> (TemplateDirectory Snap -> Snap ())
                -> (TemplateState Snap -> Snap ())
                -> Snap ()
templateHandler td reload f = reload td <|> (f =<< getDirectoryTS td)


defaultReloadHandler :: TemplateDirectory Snap -> Snap ()
defaultReloadHandler td = path "admin/reload" $ do
    e <- reloadTemplateDirectory td
    modifyResponse $ setContentType "text/plain; charset=utf-8"
    writeBS . B.pack $ either id (const "Templates loaded successfully.") e


render :: TemplateState Snap -> ByteString -> Snap ()
render ts template = do
    req   <- getRequest 
    bytes <- renderTemplate ts $ B.concat [mapName' (rqServerName req), "/", template]
    flip (maybe pass) bytes $ \x -> do
        modifyResponse $ setContentType "text/html; charset=utf-8"
        writeBS $ B.pack (replaceAll (B.unpack x) "$$TITLE$$" "devrand.org")

renderList :: TemplateState Snap -> Int -> SortType -> ByteString -> Snap ()
renderList ts count sort tag = do
    req   <- getRequest 
    let host = mapName' $ rqServerName req
    let tag' = B.unpack tag
    bytes <- renderTemplate ts $ B.concat [mapName' host, "/list"]
    indiv <- renderTemplate ts $ B.concat [mapName' host, "/list-table"]
    list  <- liftIO $ makeListItems (B.unpack (fromJust indiv)) (B.unpack host) (tag', sort, count)
    selForward  <- liftIO $ makeSelForward 0 (B.unpack host) (tag', sort, count)
    selForwardB <- liftIO $ makeSelForward 1 (B.unpack host) (tag', sort, count)
    flip (maybe pass) (bytes) $ \x -> do
        let finalOutput = foldl (\x (y,z) -> replaceAll x y z) (B.unpack x) [ ("$$LIST$$", list)
                                                                            , ("$$TAGNAME$$", tag')
                                                                            , ("$$BACKLINK$$", makeBackLink tag' sort count )
                                                                            , ("$$SELECTFORWARD$$", selForward )
                                                                            , ("$$SELECTFORWARDB$$", selForwardB )
                                                                            , ("$$TITLE$$", B.unpack host ++ ": " ++ tag' ++ " Posts" )
                                                                            ]
        modifyResponse $ setContentType "text/html; charset=utf-8"
        writeBS $ B.pack finalOutput

renderPost :: TemplateState Snap -> ByteString -> Snap ()
renderPost ts id = do
    req   <- getRequest 
    let host = mapName' $ rqServerName req
    bytes <- renderTemplate ts $ B.concat [host, "/post"]
    post <- liftIO $ getSinglePost (B.unpack host) (B.unpack id)
    let pageTitle = B.unpack host ++ ": " ++ postTitle post
    let postString = concat ["<H1>", postTitle post, "</H1>", 
                             "<div id=\"postId\" style=\"visibility: hidden\">", postId post, "</div>\n", 
                             postContent post]
    flip (maybe pass) (bytes) $ \x -> do
        modifyResponse $ setContentType "text/html; charset=utf-8"
        writeBS $ B.pack (replace (replace (B.unpack x) "$$POST$$" postString) "$$TITLE$$" pageTitle)

templateServe :: TemplateState Snap -> Snap ()
templateServe ts = ifTop (render ts "index") <|> do
    path' <- getSafePath
    when (head path' == '_') pass
    render ts $ B.pack path'

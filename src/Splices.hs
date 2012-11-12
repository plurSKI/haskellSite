module Splices
where

import           DataStore
import           Common
import           Data.List
import           Data.Maybe
import           Data.DateTime
import           Snap.Types
import qualified Data.ByteString.Char8 as B
import           Text.XML.Expat.Tree as X
import           Control.Monad.Trans
import           Data.Map
import           Text.Templating.Heist
import           Control.Concurrent.MVar
import           System.Directory
import           Data.Digest.OpenSSL.MD5



makeBackLink :: String -> SortType -> Int -> String
makeBackLink _ _ 0 = "&lt;Prev"
makeBackLink tag sort count = concat ["<a href=\"/list/", tag, "/", show sort, "/", show (count - 1), "\">&lt;Prev</a>"]

makeSelForward :: Int -> String -> (String, SortType, Int) -> IO String
makeSelForward n host (tag, sort, count) = do
  let len = maxList * (count + 1)
  postList <- getPosts host sort tag 1000 -- kludge to get all matching posts
  let numPages = ((length postList) - 1) `div` maxList
  let selectOptions = concat $ makeSelect numPages
  return $ concat ["<select id=\"pageSel", show n, "\" onChange=\"pageNav(", show n ,");\">\n", selectOptions, "</select> &nbsp; | \n", makeForward numPages]
  where makeSelect numPages = Data.List.map makeOption [0..numPages]
        makeForward numPages | count < numPages = "<a href=\"" ++ makeUrl (count + 1) ++ "\">Next &gt;</a>"
                                   | otherwise        = "Next &gt;"
        makeUrl x = concat ["/list/", tag, "/", show sort, "/", show x]
        makeOption x = concat ["<option value=\"", makeUrl x, "\"", isSelected (x == count), ">" , show x, "</option>\n"]
        isSelected True = "selected=\"selected\""
        isSelected _    = ""

-- Note: Hexpat finds it funny to delete $$FOO$$ if they are in a tag's atrribute
--       Thus the TTFOOTT 'convention'
makeListItems :: String -> String -> (String, SortType, Int) -> IO String
makeListItems templ host (tag, sort, count) = do
  let length = maxList * (count + 1)
  postList <- getPosts host sort tag length
  let postList' = drop (length - maxList) postList
  return $ concat (Data.List.map (makeItem templ) postList')
  where makeItem t p = foldl (\prev (x,y) -> replaceAll prev x y) t [ ("$$TITLE$$", postTitle p)
                                                                    , ("TTLINKTT",  postId p)
                                                                    , ("TTLINKTT",  postId p)
                                                                    , ("TTLINKTT",  postId p)   -- TODO: Why does replaceAll fail!
                                                                    , ("TTTHUMBTT", postImage p) 
                                                                    , ("$$DATE$$",  postDate p)
                                                                    , ("$$SUMMARY$$",  postSummary p)
                                                                    ]

reqFullParms :: TemplateMonad Snap (String, Params)
reqFullParms = do
    req   <- lift getRequest
    let parms = rqParams req 
    let host  = mapName $ B.unpack (rqServerName req)
    return (host, parms)

reqData :: TemplateMonad Snap (String, String, String)
reqData = do
    req   <- lift getRequest
    input <- getParamNode 
    let parms = getFstParm . Data.Map.toList $ rqParams req 
    let host  = mapName $ B.unpack (rqServerName req)
    let inp   = B.unpack $ textContent input
    return (host, parms, inp)
    where getFstParm [] = ""
          getFstParm xs = (B.unpack . fst . head) xs     

maxList :: (Num t) => t
maxList = 5

mappings :: [(String, String)]
mappings = [ ("store.crafty-crafts", "ccStore") 
           , ("blog.crafty-crafts",  "ccBlog" )
           , ("devrand", "devrand") 
           , ("devrand-dev", "devrand") 
           , ("173.255.209.60", "devrand") 
           ]

mapName' :: B.ByteString -> B.ByteString
mapName' rqstServer = (B.pack . mapName) (B.unpack rqstServer)

mapName :: String -> String
mapName rqstServer = snd . head $ Data.List.filter nameTest mappings
  where nameTest x = rqstServer `isInfixOf` (fst x)

debugSplice :: Splice Snap
debugSplice = do
    (host, parms, inp) <- reqData
    liftIO $ writeFile "/siteData/faz.dat" "test"
    return [Text $ B.pack $ show (host,parms,inp)]


getImgs :: String -> IO ([String])
getImgs host = do
   dirCont <- getDirectoryContents $ "/siteData/" ++ host ++ "/thumbs/"
   return $ Data.List.map (\x -> host ++ "/thumbs/" ++ x) (trimList dirCont)
   where trimList = Data.List.filter (\x -> not ("." `isPrefixOf` x)) 


--elem :: String -> [(String, String)] -> [NodeG [] B.ByteString Text] -> NodeG [] B.ByteString Text
makeElem name params = X.Element (B.pack name) (Data.List.map packPair params)
   where packPair (x,y) = (B.pack x, B.pack y)

-- createImgElement :: (String, Int) -> NodeG [] B.ByteString text
createImgElement (x,id) = makeElem "form" [("action", "faz")] [tr]
    where img = makeElem "img" [("src",x), ("width", "190px"), ("height", "190px")] [] 
          txt = X.Element (B.pack "input") [(B.pack "type", B.pack "text")] [] 
          upd = makeElem "input" [("type", "submit"), ("value", "Update")] []
          br  = makeElem "br" [] []
          td1 = makeElem "td" [] [img]  
          td2 = makeElem "td" [] [txt, br, upd]  
          tr  = makeElem "tr" [] [td1, td2] 


extractParam :: String -> Params -> String
extractParam index params = B.unpack $ head pList
  where pList = fromJust $ Data.Map.lookup (B.pack index) params

adminPostSplice :: Splice Snap
adminPostSplice = do
    (host, parms ) <- reqFullParms
    let id = extractParam "postId" parms
    let title = extractParam "postTitle" parms
    let summary  = extractParam "postSummary" parms
    let content  = extractParam "postContent" parms
    let image  = extractParam "postImage" parms
    let rating  = read (extractParam "rating" parms) :: Int
    let visible = read (extractParam "visible" parms) :: Bool
    date <- liftIO getCurrentTime
    let date' = take dateLen $ show date
    let post = SitePost id title summary content image date' rating visible
    let tagList = "[" ++ (extractParam "tags" parms) ++ "]"
    let zipTags = zipWith (,) (read tagList :: [String]) (repeat id)
    liftIO $ addPost host post zipTags
    return $ [makeElem "a"  [("href", "/view/" ++ id)] [Text (B.pack ("View: " ++ title))]]

adminScrollSplice :: Splice Snap
adminScrollSplice = do
    (host, parms ) <- reqFullParms
    imgList <- liftIO $ getImgs host
    let action = Data.Map.lookup (B.pack "action") parms
    return $ Data.List.map createImgElement (zip imgList [0..]) -- zipWith (++) brList imgList'
 -- where makeAction imgList Nothing = B.pack imgList
 --       makeAction imgList x       = head $ fromJust x

imageShowSplice :: Splice Snap
imageShowSplice = do
    (host, parms, inp) <- reqData
    scrollList <- liftIO $ getImageScrolls host 9 
    let imageList = Data.List.map makeImageScrolls scrollList
    let tds = Data.List.map (\x -> makeElem "td" [] [x]) imageList
    return [makeElem "tr" [] (take 3 tds), makeElem "tr" [] (take 3 (drop 3 tds)), makeElem "tr" [] (drop 6 tds)]
  where makeImageScrolls p = X.Element (B.pack "a") [(B.pack "href", B.pack (imagePost p))] [imageElem p]
        imageElem p = makeElem "img" [("src", imageLocation p), ("height", "190px"), ("width", "190px")] []
    

imageScrollSplice :: Splice Snap
imageScrollSplice = do
    (host, parms, inp) <- reqData
    scrollList <- liftIO $ getImageScrolls host maxList
    return $ Data.List.map makeImageScrolls scrollList
  where makeImageScrolls p = X.Element (B.pack "a") [(B.pack "href", B.pack (imagePost p))] [imageElem p]
        imageElem p = makeElem "img" [("src", imageLocation p), ("height", "190px"), ("width", "190px")] []

tagListHeadSplice :: Splice Snap
tagListHeadSplice = do
    (host, _, inp) <- reqData
    return $ [makeElem "div" [ ("style", "height:20em;margin-bottom:1em;")] [makeElem "ul" [("id", "navigation")] []]]

tagListSplice :: Splice Snap
tagListSplice = do
    (host, _, inp) <- reqData
    tags <- liftIO $ getTags host
    unfilteredPosts <- liftIO $ getAllPosts host
    let posts = Data.List.filter (visible) unfilteredPosts
    let tags' = sortTags tags
    let tagEntries = Data.List.map (\x -> tagPosts posts (postList x tags)) tags' 
    let finalList = zip tags' tagEntries
    return $ Data.List.map createEntry finalList
  where topLink name = makeElem "a"  [("class", "head"), ("href", "/list/" ++ name)] [X.Text (B.pack name)]
        moreLink name  = makeElem "a"  [("href", "/list/" ++ name)] [X.Text (B.pack "view all...")]
        midLink p = makeElem "a"  [("href", "/view/" ++ postId p)] [X.Text (B.pack (postTitle p))]
        ulPart ps n = makeElem "ul" [] [ makeElem "li" [] (Data.List.map midLink ps), makeElem "li" [] [moreLink n] ]
        tagPosts p = Data.List.map (extractPost p)
        postList t ts = Data.List.map snd $ take maxList (Data.List.filter (\x -> fst x == t) ts)
        createEntry (x, xs) = makeElem "li" [] [topLink x, ulPart xs x]

rssSplice :: Splice Snap
rssSplice = do 
    (host, _, inp) <- reqData
    items <- liftIO $ getTuples host inp
    req   <- lift getRequest
    let hostUrl = B.unpack (rqServerName req)
    let hostPort = show (rqServerPort req)
    return $ Data.List.map (makeRssItem (hostUrl,hostPort)) items
    where makeRssItem serv (title, id, summary) = makeElem "item" [] [ makeElem "title" [] [X.Text (B.pack title)]
                                                                     , makeElem "link"  [] [X.Text (B.pack (makeFullUrl serv id))]
                                                                     , makeElem "description" [] [X.Text (B.pack summary)]
                                                                     ]
          makeFullUrl (host,port) id | (read port :: Int) < 1 = "http://" ++ host ++ "/view/" ++ id 
                                     | otherwise              = "http://" ++ host ++ ":" ++ port ++ "/view/" ++ id

getTuples :: String -> String -> IO [(String, String, String)]
getTuples host "comments" = do comments <- getAllComments host ( maxList ^2 )
                               return $ Data.List.map (\x -> (commName x ++ " (" ++ commPost x ++ ")", commPost x, take 100 (commContent x))) comments
getTuples host _          = do posts <- getPosts host Date "" (maxList ^2)
                               return $ Data.List.map (\x -> (postTitle x, postId x, postSummary x)) posts

captchaSplice :: Splice Snap
captchaSplice = do
      date <- liftIO getCurrentTime
      return [ tsElem date, tsgElem date, capElem ]
  where timestamp d = md5sum (B.pack (salt ++ show d)) 
        tsElem d    = makeElem "span" [("id", "timestamp"), ("style", "visibility: hidden")] [X.Text (B.pack (timestamp d))]
        tsgElem d   = makeElem "span" [("id", "timestampGiven"), ("style", "visibility: hidden")] [X.Text (B.pack (show d))]
        capElem     = makeElem "input" [("id", "captcha"), ("style", "visibility: hidden")] []

menuListSplice :: Splice Snap
menuListSplice = do 
    (host, _, inp) <- reqData
    menuPosts <- liftIO $ getPosts host (findSort inp) "" maxList
    return $ Data.List.map makeMenu menuPosts
  where findSort "popular" = Rating
        findSort _         = Date
        makeMenu p = makeElem "a" [("href", "/view/" ++ postId p)] [X.Text $ B.pack (postTitle p)]

commentSplice :: Splice Snap
commentSplice = do
    (host, _, inp) <- reqData
    req   <- lift getRequest
    let post = drop 6 $ (B.unpack (rqURI req)) -- sigh
    comments <- liftIO $ getComments host post
    return $ concat (Data.List.map makeComment comments)
  where makeComment c = [brElem, headElem c, contentElem c]
        brElem        = makeElem "BR" [] []
        headElem c    = makeElem "B"  [] [X.Text (B.pack (commName c ++ " said (" ++ commDate c ++ "):"))]
        contentElem c = makeElem "div"  [("class", "userComment")] [X.Text (B.pack (commContent c))]

-- Look down, now back up, your in a monad
oldSplice :: Splice Snap
oldSplice = do
  (host, parms ) <- reqFullParms
  newPost <- liftIO $ getNewPost host (extractParam "item" parms)
  return $ [makeElem "meta" [("http-equiv", "refresh"), ("content", "0;url=/view/" ++ newPost)] [X.Text (B.pack "Redirecting to correct link...")]]


cssSplice :: Splice Snap
cssSplice = do
  (host, _, _) <- reqData
  return [makeElem "link" [("href", "/css/" ++ host ++ ".css"), ("rel", "stylesheet"), ("type", "text/css")] []]

spliceList :: [(B.ByteString, Splice Snap)]
spliceList = [ (B.pack "debug"   , debugSplice) 
             , (B.pack "menuList", menuListSplice) 
             , (B.pack "captcha", captchaSplice) 
             , (B.pack "comments", commentSplice) 
             , (B.pack "tagList", tagListSplice) 
             , (B.pack "tagListHead", tagListHeadSplice) 
             , (B.pack "imageScroll", imageScrollSplice) 
             , (B.pack "imageShow", imageShowSplice) 
             , (B.pack "adminScroll", adminScrollSplice) 
             , (B.pack "adminPost", adminPostSplice) 
             , (B.pack "makeRss", rssSplice) 
             , (B.pack "handleOld", oldSplice) 
             , (B.pack "cssLink", cssSplice) 
             ]

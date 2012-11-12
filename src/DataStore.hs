module DataStore
where

import System.Random
import Data.List
import System.IO as S
import System.IO.Strict as SS
import           Control.Monad.Trans

replaceAll' :: Eq a => [a] -> [a] -> [a] -> [a]
replaceAll' [] _ _ = []
replaceAll' s find repl =
    if take (length find) s == find
        then repl ++ (replaceAll' (drop (length find) s) find repl)
        else [head s] ++ (replaceAll' (tail s) find repl)

data SitePost = SitePost { postId      :: String
                         , postTitle   :: String
                         , postSummary :: String
                         , postContent :: String
                         , postImage   :: String
                         , postDate    :: String
                         , rating      :: Int
                         , visible     :: Bool
                         } deriving (Eq, Show, Read)

type Tag = (String, String)
           
data View = View { viewPost  :: String
                 , viewDate  :: String
                 , viewIP    :: String
                 , viewAgent :: String
                 } deriving (Eq, Show, Read)

data Comment = Comment { commId      :: String
                       , commName    :: String
                       , commIp      :: String
                       , commPost    :: String
                       , commDate    :: String
                       , commMail    :: String
                       , commContent :: String
                       } deriving (Eq, Show, Read)

data BannedIp = BannedIp { bannedId :: Int
                         , banIp    :: String
                         , comment  :: String
                         } deriving (Eq, Show, Read)

data ImageScroll = ImageScroll { imageLocation :: String
                               , imagePost     :: String
                               } deriving (Eq, Show, Read)
                 

data SortType = Date | Popularity | RevDate | Rating deriving (Eq, Show, Read)

type Site = String

getTags :: Site -> IO [Tag]
getTags s = do
  curTags  <- safeRead ("/siteData/admin/" ++ s ++ "/Tags.dat")
  return $ Prelude.read curTags :: IO [Tag]

sortTags :: [Tag] -> [String]
sortTags tags = map fst $ sortBy (\x y -> compare (snd x) (snd y)) counts
  where tags'  = map fst tags
        elems  = nub tags'
        counts = zip elems $ map (\x -> length (filter (==x) tags')) elems

sortPosts :: SortType -> [SitePost] -> [SitePost]
sortPosts Date    posts = reverse posts
sortPosts RevDate posts = posts
sortPosts Rating  posts = sortBy (\x y -> compare (rating x) (rating y)) posts 
sortPosts _ posts       = posts

extractPost :: [SitePost] -> String -> SitePost
extractPost posts id = head $ filter (\x -> postId x == id) posts

getSinglePost :: Site -> String -> IO SitePost
getSinglePost s id = do
  allPosts <- getAllPosts s
  return $ head $ filter (\x -> postId x == id) allPosts

getPosts :: Site -> SortType -> String -> Int -> IO [SitePost]
getPosts s sort tag num = do
  allPosts <- getAllPosts s
  tagList  <- getTags s
  let filteredPosts | tag == "All" = allPosts
                    | otherwise    = filterTags tag tagList allPosts
  let sortedPosts = filter (visible) $ sortPosts sort filteredPosts
  return $ take num sortedPosts

filterTags :: String -> [Tag] -> [SitePost] -> [SitePost]
filterTags "" _ xs = xs
filterTags t tagList siteList = filter (\x -> (postId x) `elem` tagList') siteList
  where tagList' = map snd $ filter (\x -> t == (fst x)) tagList

safeRead :: String -> IO String
safeRead s = do
  let file = SS.readFile s
  SS.run file

getAllPosts :: Site -> IO [SitePost]
getAllPosts s = do 
  file <- safeRead ("/siteData/admin/" ++ s ++ "/Posts.dat")
  return $ Prelude.read file :: IO [SitePost]

getAllComments :: Site -> Int -> IO [Comment]
getAllComments s count = do
  comments <- safeRead ("/siteData/comments/" ++ s ++ "/AllComments.dat")
  let comments' = Prelude.read ("[" ++ comments ++ "]") :: [Comment]
  return $ take count (reverse ( tail comments' ))
  

getComments :: Site -> String -> IO [Comment]
getComments s post = do
  comments <- safeRead ("/siteData/comments/" ++ s ++ "/" ++ post ++ ".dat")
  let comments' = Prelude.read ("[" ++ comments ++ "]") :: [Comment]
  return $ reverse ( tail comments' )

getBannedIps :: [BannedIp]
getBannedIps = [BannedIp 0 "faz" "baz"]

getNewPost :: Site -> String -> IO String
getNewPost s old = do 
  file <- safeRead ("/siteData/admin/" ++ s ++ "/OldPosts.dat")
  let list = Prelude.read file :: [(String, String)]
  return $ snd $ head (filter (\x -> fst x == old) list)

getImageScrolls :: Site -> Int -> IO [ImageScroll]
getImageScrolls s num = do 
  file <- safeRead ("/siteData/admin/" ++ s ++ "/ImageScrolls.dat")
  let list = Prelude.read file :: [ImageScroll]
  gen <- newStdGen
  let rVals = nub $ randomRs (0, (length list) - 1) gen
  let pics = take num rVals  
  return $ map (\x -> list !! x) pics

addPost :: Site -> SitePost -> [Tag] -> IO ()
addPost s post tags = do
  allPosts <- getAllPosts s
  curTags <- getTags s
  let newTags = tags ++ curTags 
  let newPosts = allPosts  ++ [post]
  addImage $ ImageScroll (postImage post) ("/view/" ++ postId post) 
  let emptyComment = "Comment {commId =\"\", commName = \"\", commIp = \"\", commPost = \"\", commDate = \"\", commMail = \"\", commContent = \"\"}"
  S.writeFile ("/siteData/admin/" ++ s ++ "/Posts.dat") (show newPosts)
  S.writeFile ("/siteData/admin/" ++ s ++ "/Tags.dat") (show newTags)
  S.writeFile ("/siteData/comments/" ++ s ++ "/" ++ postId post ++ ".dat") emptyComment
  where addImage x | visible post = addImageScroll s x
                   | otherwise = S.putStrLn $ "Invisible Post"

addComment :: Site -> String -> Comment -> IO()
addComment s post comment  = do
  S.appendFile ("/siteData/comments/" ++ s ++ "/" ++ post ++ ".dat") (",\n" ++ show comment)
  S.appendFile ("/siteData/comments/" ++ s ++ "/AllComments.dat") (",\n" ++ show comment)

delComment :: Int -> IO()
delComment s = S.putStrLn $ "del comment"

addBannedIP :: BannedIp -> IO()
addBannedIP s = S.putStrLn $ "Added s"

delBannedIP :: String -> IO()
delBannedIP s = S.putStrLn $ "del s"

addView :: View -> IO()
addView s = S.putStrLn "add view"

addImageScroll :: Site -> ImageScroll -> IO()
addImageScroll s x = do
  file <- safeRead ("/siteData/admin/" ++ s ++ "/ImageScrolls.dat")
  let cur = Prelude.read file :: [ImageScroll]
  let newImages = cur ++ [x]
  S.writeFile ("/siteData/admin/" ++ s ++ "/ImageScrolls.dat") (show newImages)

delImageScroll :: Site -> Int -> IO()
delImageScroll s x = S.putStrLn "del image"


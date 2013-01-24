{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe
import System.Environment (getEnv)
import Control.Monad (msum)
import Happstack.Server
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

type PathConf = (String, String) -- ^ url, filepath

main :: IO ()
main = do
  home <- getEnv "HOME"
  content <- readFile $ home ++ "/.hfsrc"
  let conf = getConf $ lines content
  servFileWith conf

appTemplate :: String -> H.Html -> H.Html
appTemplate title body = H.html $ do
  H.head $ do
    H.title (H.toHtml title)
    H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=uft-8"
  H.body body

servFileWith :: [PathConf] -> IO ()
servFileWith pcs = do simpleHTTP conf $ msum $ withConf ++ [indexPage]
  where
    conf = Conf 8000 Nothing Nothing 60
    entry = ["frames.html","index.html","index.htm"]
    withConf = map (\(u, p) -> dirs u $ serveDirectory EnableBrowsing entry p) pcs
    indexPage = ok $ toResponse $ appTemplate "INDEX" $ sequence_ $ map toA pcs
    toA (u,_) = H.h1 $ H.a ! A.href (toValue u) $ (H.toHtml u) >> H.br

getConf :: [String] -> [PathConf]
getConf ss = catMaybes $ map getConf1 ss
  where
    getConf1 s = case break (== '=') s of
      (u,(_:p)) -> Just (u,p)
      _ -> Nothing

{- ####  put this in ~/.hfsrc

/usr/share/doc/ghc/html/=/usr/share/doc/ghc/html
any=/usr/share/doc
Python=/usr/share/doc/python/html
gcc=/usr/share/doc/gcc

#### -}

-- /home/wuxb/.cabal/share/doc=/home/wuxb/.cabal/share/doc

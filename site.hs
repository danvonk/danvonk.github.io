--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Hakyll

config :: Configuration
config = defaultConfiguration {
  deployCommand = "rsync -avz _site/ ubuntu@danvonk.com:/home/user-data/www/default/"
}

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
  -- et book, reynold
  forM_
    [ "css/et-book/*",
      "css/et-book/et-book-bold-line-figures/*",
      "css/et-book/et-book-display-italic-old-style-figures/*",
      "css/et-book/et-book-roman-line-figures/*",
      "css/et-book/et-book-roman-old-style-figures/*",
      "css/et-book/et-book-semi-bold-old-style-figures/*",
      "css/reynold/*"
    ]
    $ \f -> match f $ do
      route idRoute
      compile copyFileCompiler

      forM_
        [ "images/*",
          "fonts/*",
          "static/*"
        ]
        $ \f -> match f $ do
          route idRoute
          compile copyFileCompiler

      match "css/*" $ do
        route idRoute
        compile compressCssCompiler

      match ("drafts/*") $ do
        route $ setExtension "html"
        compile $
          pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

      match (fromList ["about.html", "readinglist.md"]) $ do
        route $ setExtension "html"
        compile $
          pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

      match "posts/*" $ do
        tags <- buildTags "posts/*" (fromCapture "tags/*.html")
        route $ setExtension "html"
        compile $
          pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

      create ["archive.html"] $ do
        route idRoute
        compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let archiveCtx =
                listField "posts" postCtx (return posts)
                  <> constField "title" "Archives"
                  <> defaultContext

          makeItem ""
            >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
            >>= relativizeUrls

      match "index.html" $ do
        route idRoute
        compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let indexCtx =
                listField "posts" postCtx (return posts)
                  <> defaultContext

          getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

      match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%e %B, %Y"
  <> dateField "year" "%Y"
  <> defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

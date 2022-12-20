--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Data.Monoid (mappend)
import Hakyll

config = defaultConfiguration {
  deployCommand = "rsync -avz _site/ ubuntu@danvonk.com:/home/user-data/www/danvonk.com/"
}

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  -- et book
  forM_
    [ "css/et-book/*",
      "css/et-book/et-book-bold-line-figures/*",
      "css/et-book/et-book-display-italic-old-style-figures/*",
      "css/et-book/et-book-roman-line-figures/*",
      "css/et-book/et-book-roman-old-style-figures/*",
      "css/et-book/et-book-semi-bold-old-style-figures/*"
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
        route $ setExtension "html"
        compile $
          pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

      create ["archive.html"] $ do
        route idRoute
        compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let archiveCtx =
                listField "posts" postCtx (return posts)
                  `mappend` constField "title" "Archives"
                  `mappend` defaultContext

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
                  `mappend` defaultContext

          getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

      match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

mainCtx :: Context String
mainCtx =
  dateField "year" "%Y"
  `mappend` defaultContext

postCtx :: Context String
postCtx =
  dateField "date" "%e %B, %Y"
    `mappend` defaultContext


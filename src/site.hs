--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Hakyll
import Text.Pandoc.SideNote

import BlogPost (usingVenoBox)

config :: Configuration
config = defaultConfiguration {
  deployCommand = "rsync -avz _site/ ubuntu@danvonk.com:/home/user-data/www/default/"
}

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
  forM_
    [ "css/et-book/*",
      "css/et-book/et-book-bold-line-figures/*",
      "css/et-book/et-book-display-italic-old-style-figures/*",
      "css/et-book/et-book-roman-line-figures/*",
      "css/et-book/et-book-roman-old-style-figures/*",
      "css/et-book/et-book-semi-bold-old-style-figures/*",
      "css/reynold/*",
      "images/*",
      "fonts/*",
      "static/*"
    ]
    $ \f -> match f $ do
      route idRoute
      compile copyFileCompiler

      match "css/*" $ do
        route idRoute
        compile compressCssCompiler

      match "drafts/*" $ do
        tags <- buildTags "drafts/*" (fromCapture "tags/*.html")
        route $ setExtension "html"
        compile $ do
          let tagsCtx = tagsField "tags" tags <> postCtx
          pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions (usingSideNotes . usingVenoBox)
            >>= loadAndApplyTemplate "templates/post.html" tagsCtx
            >>= loadAndApplyTemplate "templates/default.html" pageCtx
            >>= relativizeUrls

      create ["pages/about.html", "pages/readinglist.md"] $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $
          pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" pageCtx
            >>= relativizeUrls

      match "posts/*" $ do
        tags <- buildTags "posts/*" (fromCapture "tags/*.html")
        let tagsCtx = tagsField "tags" tags <> postCtx
        -- Create a /tags/* page for each tag
        tagsRules tags $ \tag pattern -> do
          let title = "Posts tagged \'" ++ tag ++ "\'"
          route idRoute
          compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" tagsCtx (return posts)
                      `mappend` defaultContext
            makeItem ""
              >>= loadAndApplyTemplate "templates/tag.html" ctx
              >>= loadAndApplyTemplate "templates/default.html" ctx
              >>= relativizeUrls
        -- Create a page for each blog post
        route $ setExtension "html"
        compile $ do
          pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions usingVenoBox
            >>= loadAndApplyTemplate "templates/post.html" tagsCtx
            >>= loadAndApplyTemplate "templates/default.html" tagsCtx
            >>= relativizeUrls

      create ["pages/archive.html"] $ do
        route $ gsubRoute "pages/" (const "")
        compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let archiveCtx =
                listField "posts" postCtx (return posts)
                  <> constField "title" "Archives"
                  <> pageCtx
          makeItem ""
            >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
            >>= relativizeUrls

      match "pages/index.html" $ do
        route $ gsubRoute "pages/" (const "")
        compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let indexCtx =
                listField "posts" postCtx (return posts)
                  <> pageCtx
          getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

      match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

pageCtx :: Context String
pageCtx = dateField "year" "%Y"
  <> defaultContext

postCtx :: Context String
postCtx =
  dateField "date" "%e %B, %Y"
  `mappend` pageCtx

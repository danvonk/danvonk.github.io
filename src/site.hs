--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import BlogPost (blogPostCompiler, usingVenoBox)
import Control.Monad (filterM, forM_)
import Data.Functor ((<&>))
import Data.Text as T hiding (elem)
import Data.Time.Calendar
import Data.Time.Clock
import Gallery (galleryCompiler)
import Hakyll
import Hakyll.Images
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Highlighting
import Text.Pandoc.Options
import Text.Pandoc.Readers
import Text.Pandoc.Writers.HTML

config :: Configuration
config =
    defaultConfiguration
        { deployCommand = "rsync -rltvz _site/ ubuntu@danvonk.com:/home/user-data/www/default/"
        }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    -- Static content
    forM_
        [ "css/et-book/*"
        , "css/et-book/et-book-bold-line-figures/*"
        , "css/et-book/et-book-display-italic-old-style-figures/*"
        , "css/et-book/et-book-roman-line-figures/*"
        , "css/et-book/et-book-roman-old-style-figures/*"
        , "css/et-book/et-book-semi-bold-old-style-figures/*"
        , "js/*"
        , "images/**.png"
        , "fonts/*"
        , "static/*"
        ]
        $ \f -> match f $ do
            route idRoute
            compile copyFileCompiler

            -- Compress images
            forM_ ["images/**.JPG", "images/**.jpg"] $ \f -> match f $ do
                route idRoute
                compile $ loadImage >>= compressJpgCompiler 65

            create ["css/code-style-light.css"] $ do
                route idRoute
                compile $ do
                    makeItem $ styleToCss haddock
            create ["css/code-style-dark.css"] $ do
                route idRoute
                compile $ do
                    makeItem $ styleToCss breezeDark

            match "gallery/*" $ do
                route idRoute
                compile $ do
                    loadImage

            -- create ["pages/gallery.md"] $ do
            --   route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
            --   compile $ do
            --     images <- loadAll "gallery/*"
            --     let galleryCtx = listField "images" (return images) <> pageCtx
            --     galleryCompiler
            --     >>= loadAndApplyTemplate "templates/default.html" galleryCtx
            --     >>= relativizeUrls

            match "css/*" $ do
                route idRoute
                compile compressCssCompiler

            match "drafts/*" $ do
                tags <- buildTags "drafts/*" (fromCapture "tags/*.html")
                route $ setExtension "html"
                compile $ do
                    let tagsCtx = tagsField "tags" tags <> postCtx
                    blogPostCompiler
                        >>= loadAndApplyTemplate "templates/post.html" tagsCtx
                        >>= loadAndApplyTemplate "templates/default.html" pageCtx
                        >>= relativizeUrls

            create ["pages/about.md", "pages/readinglist.md"] $ do
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
                        let ctx =
                                constField "title" title
                                    <> listField "posts" tagsCtx (return posts)
                                    <> pageCtx
                        makeItem ""
                            >>= loadAndApplyTemplate "templates/tag.html" ctx
                            >>= loadAndApplyTemplate "templates/default.html" ctx
                            >>= relativizeUrls
                -- Create a page for each blog post
                route $ setExtension "html"
                compile $ do
                    blogPostCompiler
                        >>= saveSnapshot "content"
                        >>= loadAndApplyTemplate "templates/post.html" tagsCtx
                        >>= loadAndApplyTemplate "templates/default.html" tagsCtx
                        >>= relativizeUrls

            create ["pages/archive.md"] $ do
                tags <- buildTags "posts/*" (fromCapture "tags/*.html")
                route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
                compile $ do
                    posts <- recentFirst =<< loadAll "posts/*"
                    let archiveCtx =
                            listField "posts" (tagsField "tags" tags <> postCtx) (return posts)
                                <> constField "title" "Archives"
                                <> pageCtx
                    makeItem ""
                        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                        >>= relativizeUrls

            match "pages/index.md" $ do
                tags <- buildTags "posts/*" (fromCapture "tags/*.html")
                route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
                compile $ do
                    -- show "serious" posts before travel posts
                    posts <- recentFirst =<< loadAll "posts/*"
                    travelPosts <- filterM (hasTag "travel") posts
                    otherPosts <- filterM (fmap not . hasTag "travel") posts

                    let indexCtx =
                            listField "posts" (tagsField "tags" tags <> postCtx) (return $ otherPosts ++ travelPosts)
                                <> pageCtx
                    getResourceBody
                        >>= applyAsTemplate indexCtx
                        >>= loadAndApplyTemplate "templates/default.html" indexCtx
                        >>= relativizeUrls

            match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

-- date :: IO (Integer, Int, Int) -- :: (year, month, day)
-- date = getCurrentTime <&> toGregorian . utctDay

-- year :: IO String
-- year = fromInt . (\(y, _, _) -> y) <$> date

pageCtx :: Context String
pageCtx =
    constField "copyright" "© 2025 Dan Vonk"
        <> defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%e %B, %Y"
        <> teaserField "teaser" "content"
        <> pageCtx

hasTag :: String -> Item a -> Compiler Bool
hasTag tag post = do
    postTags <- getTags (itemIdentifier post)
    return (tag `elem` postTags)

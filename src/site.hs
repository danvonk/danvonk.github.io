--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import BlogPost (blogPostCompiler, usingVenoBox)
import Control.Monad (filterM, forM, forM_, liftM)
import Data.Functor ((<&>))
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import qualified Data.Text as T hiding (elem, length)
import Gallery (galleryCompiler)
import Hakyll
import Hakyll.Images
import Hakyll.Web.Paginate

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
        , "js/**.js"
        , "images/**.png"
        , "images/**.svg"
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

            -- Logic for paginated index pages
            let mkIndexIdentifier pageNr =
                    if pageNr == 1
                        then fromFilePath "index.html"
                        else fromFilePath ("page/" ++ show pageNr ++ "/index.html")

            tags <- buildTags "posts/*" (fromCapture "tags/*.html")
            pag <- buildPaginateWith grouper "posts/*" mkIndexIdentifier
            -- Create paginated index pages
            -- First, build the Paginate structure
            do
                -- Then, create rules for each page using the Paginate structure
                paginateRules pag $ \pageNumber pattern -> do
                    route idRoute
                    compile $ do
                        -- get all posts and filter for the pinned header
                        allPosts <- recentFirst =<< loadAll "posts/*"
                        pinned <- filterM (isPinnedPost . itemIdentifier) allPosts
                        postsForPage <- recentFirst =<< loadAll pattern
                        tagCloud <- renderTagList (sortTagsBy (comparing $ Down . length . snd) tags)
                        let paginateCtx = paginateContext pag pageNumber
                        let indexCtx =
                                listField "posts" (tagsField "tags" tags <> postCtx) (return postsForPage)
                                    <> listField "pinned" (tagsField "tags" tags <> postCtx) (return pinned)
                                    <> constField "tagCloud" tagCloud
                                    <> constField "title" "Home"
                                    <> paginateCtx
                                    <> pageCtx
                        makeItem ""
                            >>= loadAndApplyTemplate "templates/index.html" indexCtx
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
    constField "copyright" "© 2026 Dan Vonk"
        <> defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%e %B, %Y"
        <> teaserField "teaser" "content"
        <> pageCtx

-- Run sortRecentFirst on ids, and then liftM (paginateEvery 10) into it
grouper :: [Identifier] -> Rules [[Identifier]]
grouper = fmap (paginateEvery 8) . sortRecentFirst

isPinnedPost :: Identifier -> Compiler Bool
isPinnedPost i = do
    md <- getMetadata i
    pure $ fromMaybe "" (lookupString "pinned" md) == "true"

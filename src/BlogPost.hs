-- | Filters for blog post compiler
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module BlogPost (
  usingVenoBox,
  defaultVenoBoxOptions,
  blogPostCompiler )
where

import qualified Data.Text as T
import Control.Monad.State (State, get, put, evalState)

import Hakyll
import Text.Pandoc.SideNote (usingSideNotes)
import Text.Pandoc.JSON
import Text.Pandoc.Walk (walkM)
import Text.Pandoc.Builder (emptyCaption)
import Text.Pandoc.Definition (Block(..))
import Text.Pandoc.Highlighting
import Text.Pandoc.Options

type MarginNote = State Int

data VenoBoxOptions = VenoBoxOptions
  {
    -- The CSS class to be used for the HTML <a> tag wrapping the image for VenoBox. It needs to be equal to the
    -- option set in the JS instantiation of the VenoBox object.
    galleryClass :: T.Text,
    -- The VenoBox gallery to be used. This sets data-gallery attribute in the <a> tag. If images have the same
    -- gallery, they can be scrolled through the lightbox gallery modal.
    defaultGallery :: T.Text
  }

defaultVenoBoxOptions :: VenoBoxOptions
defaultVenoBoxOptions = VenoBoxOptions
  {
    galleryClass = "image-gallery",
    defaultGallery = "gallery01"
  }

usingVenoBox :: VenoBoxOptions -> Pandoc -> Pandoc
usingVenoBox opts (Pandoc meta blocks) = Pandoc meta $ evalState (walkM (mkFigure opts) blocks) 0

mkFigure :: VenoBoxOptions -> Block -> MarginNote Block
mkFigure opts (Plain [Image attrs@(ids, cls, kvs) inls target]) = do
  num <- get
  put (num + 1)
  let checkBoxLabel = RawBlock "html" (T.pack $ "<label for=\"mn" ++ show num ++ "\" class=\"margin-toggle\">&#8853;</label>")
  let checkBox = RawBlock "html" (T.pack $ "<input type=\"checkbox\" id=\"mn" ++ show num ++ "\" class=\"margin-toggle\"/>")
  pure $ Figure nullAttr
                emptyCaption
                -- blocks contained in the figure:
                -- label and checkbox are for margin note toggle
                [checkBoxLabel,
                 checkBox,
                 captionDiv,
                 -- wrap the image in a link for the lightbox. Use the image alt text as a title for the lightbox
                 Plain [Link (ids, galleryClass opts : cls, [("data-gall", defaultGallery opts), ("title", snd target)] ++ kvs)
                          [Image attrs inls target] target]]
  where
    -- make the div tag for the margin note
    captionAttr = (T.empty, ["marginnote"], [])
    captionDiv = Div captionAttr [Plain [Str (snd target)]]

-- otherwise the figure will be double-wrapped?
mkFigure _ (Figure _ _ [Figure a c bs]) = pure $ Figure a c bs
mkFigure _ x = pure x

blogPostCompiler :: Compiler (Item String)
blogPostCompiler = pandocCompilerWithTransform
  defaultHakyllReaderOptions
  defaultHakyllWriterOptions {
    writerHighlightStyle = Just haddock
  }
  (usingSideNotes . usingVenoBox defaultVenoBoxOptions)

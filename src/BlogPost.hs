-- |
{-# LANGUAGE FlexibleContexts, OverloadedStrings, PatternSynonyms #-}

module BlogPost (
  usingVenoBox,
  defaultVenoBoxOptions,
  blogPostCompiler )
where

import qualified Data.Text as T

import Hakyll
import Text.Pandoc.SideNote
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Text.Pandoc.Definition (Block(..))

-- ![A view of the sahara](/images/DSCF7664.JPG)
-- ............ results in ...............................
-- <a class="image-gallery" data-gall="gallery01" title="a view of the sahara" href="/images/DSCF7664.JPG">
--  <img src="/images/DSCF7664.JPG"></a>

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

usingVenoBox :: Pandoc -> Pandoc
usingVenoBox (Pandoc meta blocks) = Pandoc meta (walk mkFigure blocks)

-- <label for="outside-ankara" class="margin-toggle">&#8853;</label><input type="checkbox" id="outside-ankara" class="margin-toggle"/>

mkFigure :: Block -> Block
mkFigure (Plain [Image attrs@(ids, cls, kvs) inls target]) = Figure
  nullAttr
  (Caption Nothing [])
  [checkBoxLabel, checkBox, Plain [Link (ids, "image-gallery" : cls, [("data-gall", "gallery01"), ("title", snd target)] ++ kvs)
          [Image attrs inls target] target], captionDiv]
  where
    -- make the margin note: <div class="marginnote">content</div>
    captionDiv = Div captionAttr [Plain [Str (snd target)]]
    captionAttr = (T.empty, ["marginnote"], [])
    checkBoxLabel = RawBlock "html" "<label for=\"xyz\" class=\"margin-toggle\">&#8853;</label>"
    checkBox = RawBlock "html" "<input type=\"checkbox\" id=\"xyz\" class=\"margin-toggle\"/>"

mkFigure (Figure _ _ [Figure a c bs]) = Figure a c bs
mkFigure x = x


blogPostCompiler :: Compiler (Item String)
blogPostCompiler = pandocCompilerWithTransform
  defaultHakyllReaderOptions
  defaultHakyllWriterOptions
  (usingSideNotes . usingVenoBox)

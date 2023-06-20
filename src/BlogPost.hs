-- |
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module BlogPost (
  usingVenoBox,
  defaultVenoBoxOptions )
where

import qualified Data.Text as T

import Text.Pandoc.JSON
import Text.Pandoc.Walk

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

veno :: Inline -> Inline
veno (Image attrs@(ids, cls, kvs) inls target) = Link
  (ids, "image-gallery" : cls, [("data-gall", "gallery01"), ("title", snd target)] ++ kvs)
  [Image attrs [] target]
  target
veno x = x

usingVenoBox :: Pandoc -> Pandoc
usingVenoBox (Pandoc meta blocks) = Pandoc meta (walk veno blocks)

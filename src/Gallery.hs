module Gallery (galleryCompiler) where

import Hakyll

galleryCompiler :: Compiler (Item String)
galleryCompiler = pandocCompiler

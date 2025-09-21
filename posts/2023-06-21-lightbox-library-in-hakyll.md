---
title: Adding a Lightbox Library to Hakyll
author: Dan Vonk
tags: Haskell, tech
---

Often when I am blogging, I am describing past events such as holidays and one of
my hobbies during those times is to take some snaps. Hakyll, my static site
generator, lets you write these articles in markdown format, insert image tags and have them rendered by `pandoc`
into HTML. <!--more--> Writing in markdown also lets you mix HTML tags and by extension even LaTeX freely into the
document, which all works perfectly well. However, for my blog, I like to have
side-notes to accompany the main text of my articles in order to describe
nuances but not to break the flow of the text (as brackets would!). This
functionality is provided by _tufte.css_ in my case, which is nice in part because
it doesn't require any additional JavaScript. In order to create these
side-notes, one has to use custom HTML
embedded into the markdown document, like this:
```html
<figure>
  <label for="mn-exports-imports" class="margin-toggle">&#8853;</label>
  <input type="checkbox" id="mn-exports-imports" class="margin-toggle"/>
  <span class="marginnote">From Edward Tufte, <em>Visual Display of Quantitative Information</em>, page 92.</span>
  <img src="img/exports-imports.png" alt="Exports and Imports to and from Denmark & Norway from 1700 to 1780" />
</figure>
```
It is inconvenient to keep this snippet around to paste each time an image is
required and it is also error-prone because one has to remember to give each
image a new unique id. Furthermore, this just provides me the ability to have a caption next to the
image. If the image is particularly nice, I want the ability to click on it
and _enlarge_ it. This is probably not too difficult to program yourself in
JavaScript. But  through a little bit of research on the internet, I found that there were
hundreds of libraries that already provide this. The one I settled one is called
**VenoBox**; not for any particular reason, though it seemed capable enough. Now
I am able to have an image with a margin note and click on it to open up a modal
window for higher-res viewing:

![Bears smoking hookah in a cafe in Brick Lane, London](/images/84530007.JPG "An
example of an image with a caption that opens up a modal window for higher-res
viewing. Taken through the open door of a cafe on Brick Lane, London, on Fomapan
100 film through a Zorki-4K.")

This is done by wrapping the image in `<a>` tags, which are given a special class
name known to the VenoBox JavaScript object, initialised at the bottom
of the page.

### Automatisation

But all this manual labour is disappointing and something a static site
generator should take care of! The simplest way that I could see to automate my
insertion of margin notes and the lightbox library hooks was to write a pandoc filter
to transform a regular image tag in markdown to HTML with my extra tags during
the page compilation. Fortunately this wasn't a difficult task and I'll share my
approach below.

Pandoc filters let you transform the document AST, which consists of `Block`s
and `Inline`s. Blocks correspond to HTML container tags like `<div>` and
`<figure>` whereas Inlines can be things like `<image>`. Therefore, what I
needed was to transform an image type to a figure block which contains the
additional `<input>` and `<label>` required for `tufte.css` as well as an the
image, now wrapped in an `<a>` tag and marked with the additional attributes to
make VenoBox work.

This is done in the `mkFigure` function, which pattern matches on a `Image`
inside a `Plain`^[Plain corresponds to `<p>` in HTML.] type. We simply construct
a new `Figure` with all of the new elements inside it. I needed to construct the
label and check box manually using `RawBlock` types as pandoc does not support
these at the AST level. Another issue I encountered was that this would lead to
my images being doubly wrapped in `<figure>` tags. I solved this by simply
unwrapping them in a second pattern-match but there may be better ways to do this.

```haskell
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

```

There are a couple of other things worth mentioning. Firstly, I created a
`VenoBoxOptions` record to allow the customisation of the VenoBox options in the
future. Secondly, each image still needs a unique id. For this, I simply created
a `State Int` type which keeps a counter running for every invokation of the function.

```haskell
data VenoBoxOptions = VenoBoxOptions
  {
    -- The CSS class to be used for the HTML <a> tag wrapping the image for VenoBox. It needs to be equal to the
    -- option set in the JS instantiation of the VenoBox object.
    galleryClass :: T.Text,
    -- The VenoBox gallery to be used. This sets data-gallery attribute in the <a> tag. If images have the same
    -- gallery, they can be scrolled through the lightbox gallery modal.
    defaultGallery :: T.Text
  }

type MarginNote = State Int
usingVenoBox :: VenoBoxOptions -> Pandoc -> Pandoc
usingVenoBox opts (Pandoc meta blocks) = Pandoc meta $ evalState (walkM (mkFigure opts) blocks) 0

```

Now this filter can be used in your Hakyll blog compiler function:

```haskell
blogPostCompiler :: Compiler (Item String)
blogPostCompiler = pandocCompilerWithTransform
  defaultHakyllReaderOptions
  defaultHakyllWriterOptions
  usingVenoBox defaultVenoBoxOptions
```

Which can then be used in the route section of your `site.hs`...

```haskell
route $ setExtension "html"
        compile $ do
          blogPostCompiler
            >>= loadAndApplyTemplate "templates/post.html" tagsCtx
            >>= loadAndApplyTemplate "templates/default.html" tagsCtx
            >>= relativizeUrls
```

... and you should now have margin notes with a lightbox library!


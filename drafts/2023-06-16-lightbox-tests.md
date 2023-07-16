---
title: Shots from Morocco
author: Dan Vonk
tags: personal, travel
---

<figure>
    <label for="double-exp" class="margin-toggle">&#8853;</label><input type="checkbox" id="double-exp" class="margin-toggle"/><span class="marginnote">A fun double (triple?) exposure.</span>
    <a class="image-gallery" data-gall="gallery01" style="background-repeat: no-repeat" href="/images/20310025.JPG"><img src="/images/20310025.JPG"></a>
</figure>

This was one of my more touristy holidays but sometimes the tourist track isn't so bad. Here's a footnote with a number.[^1]

This is a next paragraph. But this is a margin note without a number?[^-] 

Next try^[content].


[^1]: This is a footnote about how much I hate tourists.

[^-]: this is a margin note.

The HTML equivalent from the Tufte CSS github page is after this paragraph.

<label for="mn-demo" class="margin-toggle">&#8853;</label>
<input type="checkbox" id="mn-demo" class="margin-toggle"/>
<span class="marginnote">
  This is a margin note. Notice there isn’t a number preceding the note.
</span>

Now some other unrelated paragpraph goes after.

```cpp

#include <iostream>

int main() {
    std::cout << "hi";
}

```

or hs
```haskell

main :: IO ()
main = putStrLn "hi"

```

<!-- <figure> -->
<!--     <label for="sahara-view" class="margin-toggle">&#8853;</label>
<!--     <input type="checkbox" id="sahara-view" class="margin-toggle"/> -->
<!--     <span class="marginnote">A fun double exposure.</span> -\-> -->
<!--     <img src="/images/DSCF7664.JPG" alt="Train stopped beside platform." /> -\-> -->
<!--     <a class="image-gallery" data-gall="gallery01" data-title="a view of the sahara" href="/images/DSCF7664.JPG"><img src="/images/DSCF7664.JPG"></a> -->
<!-- </figure> -->

Image goes here:

![A view of the sahara](/images/DSCF7664.JPG "The Sahara")

End
<!-- The following image should be the result of the pandoc transformation... -->
<!-- <a class="image-gallery" data-gall="gallery01" title="a view of the sahara" style="background-size:0rem" href="/images/DSCF7664.JPG"><img src="/images/DSCF7664.JPG"></a> -->


This is an inline equation: $$V_{sphere} = \frac{4}{3}\pi r^3$$,
followed by a display style equation:

$$V_{sphere} = \frac{4}{3}\pi r^3$$

Let $G$ be a finite group with exponent $2$.  Then every element is
an involution, hence for any $x$, $y$ in $G$ we have:

$$\begin{align*}
  e &= (xy)^2  \\
  &=xyxy \implies \\
  y^{-1} &= xyx \implies \\
  y^{-1}x^{-1} &= xy,
\end{align*}$$

establishing that $G$ is abelian.

<figure>
<label for="mn-exports-imports" class="margin-toggle">⊕</label><input type="checkbox" id="mn-exports-imports" class="margin-toggle">
<span class="marginnote">From Edward Tufte, <em>Visual Display of Quantitative Information</em>, page 92.</span>
<img src="/images/DSCF7664.JPG" alt="Exports and Imports to and from Denmark &amp; Norway from 1700 to 1780">
</figure>

Content

---
title: How to embed Conway's Game of Life in a scrollable web page
---

My short illustrated narrative "[Life Story](/projects/lifestory)" embeds interactive simulations of [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) into a text about the same. The simulations are powered by a [TypeScript library written specifically
for the story](https://github.com/justinmanley/lifescroll). To readers, it (hopefully) seems as though text and patterns from the Game of Life are interleaved on the page and moving the page causes the patterns to evolve according to the Life update rule.

However, this is an illusion. Or, more properly, two illusions:

1. Text and patterns are interleaved on the page
2. Patterns evolve according to the Life update rule

Neither of these is strictly true. But the code goes to great lengths to sustain the impression that they are. Here's why that's necessary, and how it's done.

## Interleaving

The Game of Life is rendered using an HTML `<canvas>` element. The most natural way to layout this `<canvas>` element would be to have the element start at the top of the page and stretch all the way to the bottom, overlaying the entire text. However, large `<canvas>` elements slow down the page. In order to make the page perform well, the `<canvas>` must instead be sized to the _viewport_ (i.e. the screen) rather than the page.

This means that the patterns aren't actually interleaved with the text. Instead, they sit in a separate layer on top of it. Whenever the page scrolls, the code rerenders each pattern in a slightly updated position on the screen. If the rendering is fast enough, it seems as though patterns and text are interleaved and so that the patterns move in tandem with the text. It seems like the patterns are _part_ of the text.

That's a big _if_, though -- because in order to make scrolling feel completely smooth and seamless, the page would ideally rerender the pattern at 60 frames per second, which means that all the Life evolution and rendering code must run in 16ms.

When it doesn't run this fast (and in practice, it never runs quite this fast), the Life patterns seem to slide around on top of the text.

The library's biggest performance optimization is that it runs the Life update calculations on the GPU. This makes a _big_ difference in performance. However, it's not perfect. Transferring data from the GPU back to the CPU is expensive -- the page spends 1/3rd of its total script execution time just transferring the evolved Life state back from the GPU.

If the Life update rule was the only logic the page needed to run, it would be a relatively simple matter to keep everything on the GPU and avoid ever transferring data back to the CPU. But as we'll see below, it's not that simple.

## Evolution

In Conway's Game of Life, the update rule is applied across the entire grid simultaneously.

However, in the context of a scrolling webpage, only a small portion of the webpage is in view at any given time. Because "Life Story" includes patterns with distinctive initial shapes (but unremarkable end-states) as well as patterns which disappear and others which move offscreen, the update rule could not apply across the entire page simultaneously. Instead, the update rule only applies to the part of the page currently visible on the screen.

However, this creates a problem at the edges of the screen (especially the top and bottom, since the library assumes the page will be scrolled vertically). When a pattern is scrolling onto the screen, there is always a split second when it is halfway onscreen and halfway offscreen. Applying the Life update rule to half of a pattern almost always corrupts the pattern.

Consider the [Gosper glider gun](https://conwaylife.com/wiki/Gosper_glider_gun), a famous pattern:

<pattern-anchor src=./patterns/gosper-glider-gun.rle></pattern-anchor>

The hallmark of this pattern is that it produces a stream of gliders. The pattern above exhibits this expected behavior because the entire pattern is being updated simultaneously.

Here's the same pattern again.

<pattern-anchor src=./patterns/gosper-glider-gun-without-atomic-update.rle></pattern-anchor>

This is a copy of the Gosper glider gun. Its initial state is exactly the same as that of the glider gun above. As long as this pattern is fully on-screen, it will behave predictably. But if you scroll the page up and down so that the pattern moves on and offscreen repeatedly, you will see that the pattern quickly becomes corrupted. It doesn't behave like the first pattern, producing a steady stream of gliders. Probably, it dissolves into a random collection of small still-lives and oscillators.

My first attempt to solve this problem was to determine dynamically at each frame which cells were close enough to each other that they should be updated together (in graph-theoretic language, calculating the connected components). This maintained beautifully the integrity of patterns whose cells began and remained continguous for the pattern's entire evolution. Even in noncontiguous patterns, the contiguous subcomponents of the pattern maintained their integrity individually. However, fixing only the connected components failed to maintain spatial and temporal symmetry and consistency _between_ connected components. 

The Gosper glider gun is formed of multiple moving pieces which over the course of its evolution periodically connect and disconnect. If the Life update rule is applied to only part of the glider gun while its parts are disconnected, then the parts will be out of sync and the mechanism will not work as expected. The problem can also be seen with the gliders produced by the glider gun. The gun produces a glider every 30 steps. The gliders are widely separated; each is its own connected component. However, if they are evolved separately, then the first glider to leave the screen will be frozen in place just offscreen. When the next glider reaches the edge of the screen, it will interact with the first glider. This caused a kickback reaction which often propagated backwards through the stream of gliders and ultimately corrupted the glider gun itself.


<pattern-anchor src=./patterns/gosper-glider-gun-with-partial-atomic-update.rle></pattern-anchor>

If these problems weren't enough, there was also the problem of performance: computing the connected components is an expensive operation which noticeably slowed down scrolling and caused the patterns to slide around on top of the text.

I knew as soon as I finished implementing it that the connected component approach would not work. But this initial implementation helped me understand more clearly the aspects of Life any solution would need to preserve.

The Game of Life is hermetic and deterministic. The state of the grid (meaning: which cells are on and which cells are off) at one moment _completely_ determines the entire future evolution of the grid.

But when I ran the Life update on scroll, it was all too easy to induce patterns to evolve differently based on how quickly I scrolled them into view. This above, all, was what I wanted to avoid. Same inputs would need to always produce same outputs.

The solution I came up with is much less fancy than the dynamic connected component calculation. It requires up-front manual work for each pattern, and doesn't help with cells added by the reader (readers can <span class="tappable">tap</span><span class="clickable">click</span> to add or remove cells). However, despite these limitations, it works well to preserve the illusion that the whole grid is evolving according to the Life rule.

Along with the cells, the library loads a file which specifies how the pattern should be rendered. That file can specify, among other things, if there are cells which should always be updated together: an "atomic" update. The atomic update for each pattern is represented by a list of rectangles, each of which can have optional movement or expansion, and can even spawn new rectangles.

Below is the Gosper glider gun again, this time with its atomic update regions visualized. When you scroll down the page, whenever any part of the atomic update region intersects the viewport, the _entire_ pattern is updated. This prevents the pattern from being corrupted (though of course you can corrupt it yourself if you want to, by <span class="tappable">tapping</span><span class="clickable">clicking</span> it!).

<pattern-anchor src=./patterns/gosper-glider-gun-with-atomic-update-visualized.rle></pattern-anchor>

In order to maintain the integrity of the Gosper glider gun, including the glider stream it produces, the pattern above specifices a static rectangle containing the main part of the glider gun, and then it generates a new rectangle every 30 steps to contain the outgoing glider.

While this approach requires the bounding rectangles to be manually specified for each pattern to ensure the full pattern is contained within the atomic update regions, it has the advantage of being fast at runtime -- it is easy to quickly perform bounding rectangle intersection and containment checks.

The `?debug=atomic-updates` query param visualizes all the atomic updates on a page. Try adding it to this page or to "Life Story," and you'll see which parts of the page update atomically.

And that's it!

If all goes well, when you read "Life Story," the patterns will scroll perfectly in sync with the text and you'll never spot any discrepancy between the evolution of patterns on the page, and their true evolution in the real Game of Life. But life isn't always perfect. If you see something that doesn't look right -- if the mask slips -- you now have some idea of what's going on underneath.

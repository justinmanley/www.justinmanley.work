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

The solution I came up with is much less fancy than the dynamic connected component calculation. It requires up-front manual work for each pattern, and doesn't help with cells added by the reader (readers can click/tap to add or remove cells). However, despite these limitations, it works well to preserve the illusion that the whole grid is evolving according to the Life rule.

Along with the cells, the library loads a file which specifies how the pattern should be rendered. That file can specify, among other things, if there are cells which should always be updated together: an "atomic" update. The atomic update for each pattern is represented by a list of rectangles, each of which can have optional movement or expansion, and can even spawn new rectangles.

Below is the Gosper glider gun again, this time with its atomic update regions visualized. When you scroll the page up, whenever any part of the atomic update region intersects the viewport, the _entire_ pattern is updated. This prevents the pattern from being corrupted (though of course you can corrupt it yourself if you want to, by <span class="tappable">tapping</span><span class="clickable">clicking</span> it!).

<pattern-anchor src=./patterns/gosper-glider-gun-with-atomic-update-visualized.rle></pattern-anchor>
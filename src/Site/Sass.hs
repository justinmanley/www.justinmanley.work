module Site.Sass (compileSass) where

import Hakyll (Compiler, Item, unixFilter, withItemBody)

-- If this compiler fails, then you may need to install the `sass` binary (see
-- README for more details).
compileSass :: Item String -> Compiler (Item String)
compileSass = withItemBody (unixFilter "sass" ["--stdin"])
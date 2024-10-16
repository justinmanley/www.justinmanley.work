### Out of the Yards

[Hakyll]: https://jaspervdj.be/hakyll/
[SASS]: http://sass-lang.com/
[Bower]: http://bower.io/

This site is built using [Hakyll][], a static site generator written in Haskell.
It also uses [SASS][] and [Bower][].

### Writing a new post
Images added to new posts should be 600px wide JPEGs or links to external images.
Ideally, images should have a 16:9 aspect ratio.

Images account for the largest portion of data required for each blog post, so
they should be compressed, if possible. To compress all of the images for a blog
post, run `tools/compress-images.sh posts/my-new-post/images`.

#### Metadata specification
Metadata should be added to new posts using the following specification (although
note that metadata is loosely typed, so the data structure given here is not
actually compiled).

```haskell
data PostMetadata = PostMetadata
    { title :: String
    , date :: String               -- formatted as "Month, Day Year"
    , synposis :: String
    , featured :: Maybe Bool       -- optional
    , extras :: ExtraPostMetadata
    }

data ExtraPostMetadata
    = BlogPost {}
    | ExternallyPublishedPost
        { source :: String
        , sourceUrl :: String  -- URL
        }
```

### Setup
This static site generator requires [Sass](https://sass-lang.com), and currently
uses the Dart implementation of Sass/Scss (version 1.18.0).

In order to enable local git hooks (for running tests before committing, etc),
run `git config core.hooksPath .githooks`.

### Development
```bash
stack build && stack run site watch
```

### Deployment
```
tools/deploy.sh
```

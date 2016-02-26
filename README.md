### Out of the Yards

[Hakyll]: https://jaspervdj.be/hakyll/
[SASS]: http://sass-lang.com/
[Bower]: http://bower.io/

This site is built using [Hakyll][], a static site generator written in Haskell.
It also uses [SASS][] and [Bower][].

### Development
```
source ~/.bash_profile
cabal run site watch
```

### Deployment
```
tools/deploy.sh
```

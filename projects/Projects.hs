module Projects (Projects.compile) where

import Hakyll (Context, Rules)

import Lifestory.Lifestory as Lifestory

compile :: Context String -> Rules ()
compile = do
    Lifestory.compile

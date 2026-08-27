module Projects (Projects.compile) where

import Hakyll (Context, Rules)
import Lifestory.Lifestory as Lifestory
import LightningField.LightningField as LightningField
import Notegraph.Notegraph as Notegraph
import PhotoPairs as PhotoPairs

compile :: Context String -> Rules ()
compile context =
  sequence_
    [ Lifestory.compile context,
      Notegraph.compile context,
      LightningField.compile context,
      PhotoPairs.compile context
    ]

module Projects (Projects.compile) where

import Hakyll (Context, Rules)
import Lifestory.Lifestory as Lifestory
import LightningField.LightningField as LightningField
import Notegraph.Notegraph as Notegraph

compile :: Context String -> Rules ()
compile context =
  sequence_
    [ Lifestory.compile context,
      Notegraph.compile context,
      LightningField.compile context
    ]

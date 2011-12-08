module Context.Runtime.Features where

import Context.Knowledge.Features
import Context.Runtime

individual ▶ feat = updateCT (individual :> feat)
infixr 1 ▶

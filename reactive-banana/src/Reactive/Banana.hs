{-----------------------------------------------------------------------------
    Reactive Banana

    A small library for functional reactive programming.
------------------------------------------------------------------------------}

module Reactive.Banana (
    module Reactive.Banana.Model,
    module Reactive.Banana.Implementation,

    Event, Behavior
    ) where

import Reactive.Banana.Model hiding (interpret, Event, Behavior)
import qualified Reactive.Banana.Model as Model
import Reactive.Banana.Implementation
import qualified Reactive.Banana.Implementation as Implementation

type Event = Model.Event PushIO
type Behavior = Model.Behavior PushIO

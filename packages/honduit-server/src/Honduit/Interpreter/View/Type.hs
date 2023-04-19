module Honduit.Interpreter.View.Type where

import RIO 
import qualified Data.Has as Has
import qualified Honduit.Core.Type as Type

data ViewContext = ViewContext
  { session :: Type.Session
  }
  deriving (Generic)

instance Has.Has Type.Session ViewContext where
  getter context = context.session
  modifier fn context = context { session = fn context.session }


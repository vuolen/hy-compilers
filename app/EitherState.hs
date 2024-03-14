module EitherState
  ( EitherState (..),
    runEitherState,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (MonadState, StateT (runStateT), evalStateT)

-- "Parser" in "ParserT" relates to parsing in general, not just the parsing
-- step of the compiler. So ParserT can parse type data from the AST

newtype EitherState e s a
  = EitherState
      ( ExceptT
          e
          (StateT s Identity)
          a
      )
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError e,
      MonadState s
    )

runEitherState :: forall e s a. EitherState e s a -> s -> (Either e a, s)
runEitherState (EitherState eitherState) state = runIdentity $ runStateT (runExceptT eitherState) state
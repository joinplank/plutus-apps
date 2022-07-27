{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-

Interface to beam ecosystem used by the PAB to store contracts.

-}
module Plutus.PAB.Db.Beam
  where

import Cardano.BM.Trace (Trace)
import Control.Monad.Freer (Eff, interpret, reinterpret, runM, subsume)
import Control.Monad.Freer.Delay (DelayEffect, handleDelayEffect)
import Control.Monad.Freer.Error (handleError, runError, throwError)
import Control.Monad.Freer.Extras (LogMsg, mapLog)
import Control.Monad.Freer.Extras.Beam.Effects (handleBeam)
import Control.Monad.Freer.Extras.Beam.Postgres qualified as Postgres (runBeam)
import Control.Monad.Freer.Extras.Beam.Sqlite qualified as Sqlite (runBeam)
import Control.Monad.Freer.Extras.Modify qualified as Modify
import Control.Monad.Freer.Reader (runReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Pool (Pool)
import Data.Typeable (Typeable)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Sqlite (Sqlite)
import Database.PostgreSQL.Simple qualified as Postgres (Connection)
import Database.SQLite.Simple qualified as Sqlite (Connection)
import Plutus.PAB.Db.Beam.ContractStore (handleContractStore)
import Plutus.PAB.Effects.Contract (ContractStore)
import Plutus.PAB.Effects.Contract.Builtin (Builtin, HasDefinitions)
import Plutus.PAB.Monitoring.Monitoring (convertLog, handleLogMsgTrace)
import Plutus.PAB.Monitoring.PABLogMsg (PABLogMsg (..), PABMultiAgentMsg (..))
import Plutus.PAB.Types (PABError (..))


-- | Run the ContractStore and ContractDefinitionStore effects on the
--   SQLite database.
runBeamStoreAction ::
    forall a b.
    ( ToJSON a
    , FromJSON a
    , HasDefinitions a
    , Typeable a
    )
    => Pool Postgres.Connection
    -> Trace IO (PABLogMsg (Builtin a))
    -> Eff '[ContractStore (Builtin a), LogMsg (PABMultiAgentMsg (Builtin a)), DelayEffect, IO] b
    -> IO (Either PABError b)
runBeamStoreAction pool trace =
    runM
    . runError
    . runReader pool
    . flip handleError (throwError . BeamEffectError)
    . interpret (handleBeam Postgres.runBeam (convertLog (SMultiAgent . BeamLogItem) trace))
    . subsume @IO
    . handleDelayEffect
    . interpret (handleLogMsgTrace trace)
    . reinterpret (mapLog SMultiAgent)
    . interpret (handleContractStore @Postgres)
    . Modify.raiseEnd

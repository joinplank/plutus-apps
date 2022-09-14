-- | Blockfrost environments

module Blockfrost.Env
  ( Env (..)
  , parseEnv
  ) where

import Data.Text (Text)
import Data.Text qualified
import GHC.Generics (Generic)
import Text.Read qualified

-- | Blockfrost environments
--
-- Corresponds to Network when creating a Blockfrost project.
-- Each environment has separate token.
data Env =
    Ipfs
  | Mainnet
  | Testnet
  | Preprod
  | Preview
  | Localhost
  deriving (Eq, Read, Show, Ord, Generic)

-- | Try parsing @Env@ from @Text@
parseEnv :: Text -> Either Text Env
parseEnv tEnv = case Text.Read.readMaybe (Data.Text.unpack $ Data.Text.toTitle tEnv) of
  Just env -> pure env
  Nothing ->
    Left
      $ "Ble Unknown environment: `" <> tEnv <> "`"
      <> " expecting one of `ipfs`, `mainnet`, `testnet`, `localhost`"

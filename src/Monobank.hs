{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Monobank where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH                  ( deriveJSON )
import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.Lazy          as ByteString.Lazy
import           GHC.Generics                   ( Generic )
import           Network.Wreq
import           Utils

data CurrencyPair = CurrencyPair
  { _cpCurrencyCodeA :: Integer
  , _cpCurrencyCodeB :: Integer
  , _cpDate          :: Double
  , _cpRateSell      :: Maybe Double
  , _cpRateBuy       :: Maybe Double
  , _cpRateCross     :: Maybe Double
  }
  deriving (Show, Generic, Eq)
deriveJSON (defaultOptions { fieldLabelModifier = removePrefix "_cp"}) 'CurrencyPair
makeLenses ''CurrencyPair

data Jar = Jar
  { _jarId           :: String
  , _jarSendId       :: String
  , _jarTitle        :: String
  , _jarDescription  :: String
  , _jarCurrencyCode :: Integer
  , _jarBalance      :: Integer
  , _jarGoal         :: Integer
  }
  deriving (Show, Generic, Eq)
deriveJSON (defaultOptions { fieldLabelModifier = removePrefix "_jar"}) 'Jar
makeLenses ''Jar

data Transaction = Transaction
  { _txId              :: String
  , _txTime            :: Integer
  , _txDescription     :: String
  , _txMcc             :: Integer
  , _txOriginalMcc     :: Integer
  , _txHold            :: Bool
  , _txAmount          :: Integer
  , _txOperationAmount :: Integer
  , _txCurrencyCode    :: Integer
  , _txComissionRate   :: Maybe Integer
  , _txCashbackAmount  :: Integer
  , _txBalance         :: Integer
  , _txComment         :: Maybe String
  , _txReceiptId       :: Maybe String
  , _txInvoiceId       :: Maybe String
  , _txCounterEdrpou   :: Maybe String
  , _txCounterIban     :: Maybe String
  }
  deriving (Show, Generic, Eq)
deriveJSON (defaultOptions { fieldLabelModifier = removePrefix "_tx"}) 'Transaction
makeLenses ''Transaction

data Account = Account
  { _accountId           :: String
  , _accountSendId       :: String
  , _accountBalance      :: Integer
  , _accountCreditLimit  :: Integer
  , _accountType         :: String
  , _accountCurrencyCode :: Integer
  , _accountCashbackType :: String
  , _accountMaskedPan    :: [String]
  , _accountIban         :: String
  }
  deriving (Show, Generic, Eq)
deriveJSON (defaultOptions { fieldLabelModifier = removePrefix "_account"}) 'Account
makeLenses ''Account

data ClientInfo = ClientInfo
  { _ciClientId    :: String
  , _ciName        :: String
  , _ciWebHookUrl  :: String
  , _ciPermissions :: String
  , _ciAccounts    :: [Account]
  , _ciJars        :: Maybe [Jar]
  }
  deriving (Show, Generic, Eq)
deriveJSON (defaultOptions { fieldLabelModifier = removePrefix "_ci"}) 'ClientInfo
makeLenses ''ClientInfo

baseMonobankUrl :: String
baseMonobankUrl = "https://api.monobank.ua"

decodeRequest
  :: (FromJSON b, Applicative f)
  => Response ByteString.Lazy.ByteString
  -> f (Either (Response ByteString.Lazy.ByteString) b)
decodeRequest response = maybe (pure . Left $ response)
                               (pure . Right)
                               (decode (response ^. responseBody))

currencies
  :: String -> IO (Either (Response ByteString.Lazy.ByteString) [CurrencyPair])
currencies baseUrl = get url >>= decodeRequest
  where url = baseUrl <> "/bank/currency"

clientInfo
  :: String
  -> ByteString.ByteString
  -> IO (Either (Response ByteString.Lazy.ByteString) ClientInfo)
clientInfo baseUrl token = getWith opts url >>= decodeRequest
 where
  opts = defaults & header "X-Token" .~ [token]
  url  = baseUrl <> "/personal/client-info"

accountStatement
  :: String
  -> ByteString.ByteString
  -> String
  -> Integer
  -> Maybe Integer
  -> IO
       ( Either
           (Response ByteString.Lazy.ByteString)
           [Transaction]
       )
accountStatement baseUrl token accId fromTimestamp toTimestamp =
  getWith opts url >>= decodeRequest
 where
  opts = defaults & header "X-Token" .~ [token]
  url =
    baseUrl
      <> "/personal/statement/"
      <> accId
      <> "/"
      <> show fromTimestamp
      <> maybe mempty (\to -> "/" <> show to) toTimestamp

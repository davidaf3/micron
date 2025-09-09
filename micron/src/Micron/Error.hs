module Micron.Error (Error (..), BaseErrorType (..), ErrorType (..), errorRes) where

import Data.String (fromString)
import Micron.Request (Request)
import Micron.Response
  ( ToResponseContent (..),
    badRequest,
    forbidden,
    notFound,
    unauthorized,
  )
import Network.Wai qualified as Wai

errorRes :: Error -> Request b -> Wai.Response
errorRes err@(Error t _) = responseMaker t err

class ErrorType a where
  responseMaker :: a -> Error -> Request b -> Wai.Response

data Error where
  Error :: (ErrorType a) => a -> String -> Error

instance ToResponseContent Error where
  toAppJson (Error _ msg) = Just $ fromString $ "{\"error\":\"" ++ msg ++ "\"}"
  toTextPlain (Error _ msg) = Just $ fromString msg

data BaseErrorType = NotFound | InvalidArgument | Forbidden | Unauthorized

instance ErrorType BaseErrorType where
  responseMaker NotFound = notFound
  responseMaker InvalidArgument = badRequest
  responseMaker Forbidden = forbidden
  responseMaker Unauthorized = unauthorized

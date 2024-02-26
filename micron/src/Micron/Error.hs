module Micron.Error
  ( Error (..),
    BaseErrorType (..),
    ErrorType (..),
    BaseError,
    errorRes,
  )
where

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

errorRes :: Error a -> Request -> Wai.Response
errorRes err@(Error t _) = responseMaker t err

class ErrorType a where
  responseMaker :: a -> Error a -> Request -> Wai.Response

data Error a where
  Error :: (ErrorType a) => a -> String -> Error a

instance ToResponseContent (Error a) where
  toAppJson (Error _ msg) = Just $ fromString $ "{\"error\":\"" ++ msg ++ "\"}"
  toTextPlain (Error _ msg) = Just $ fromString msg

data BaseErrorType = NotFound | InvalidArgument | Forbidden | Unauthorized

instance ErrorType BaseErrorType where
  responseMaker NotFound = notFound
  responseMaker InvalidArgument = badRequest
  responseMaker Forbidden = forbidden
  responseMaker Unauthorized = unauthorized

type BaseError = Error BaseErrorType

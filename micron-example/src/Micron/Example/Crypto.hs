module Micron.Example.Crypto (hashPassword, checkPassword) where

import Data.Functor ((<&>))
import Data.Password.Argon2 qualified as Argon2
  ( PasswordCheck (..),
    PasswordHash (..),
    checkPassword,
    hashPassword,
    mkPassword,
  )
import Data.Text qualified as T

hashPassword :: T.Text -> IO T.Text
hashPassword pass = Argon2.hashPassword (Argon2.mkPassword pass) <&> Argon2.unPasswordHash

checkPassword :: T.Text -> T.Text -> Bool
checkPassword plain hash =
  let check = Argon2.checkPassword (Argon2.mkPassword plain) (Argon2.PasswordHash hash)
   in case check of
        Argon2.PasswordCheckSuccess -> True
        _ -> False

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Micron.Sing.Path
  ( (/),
    (./),
    (/:),
    type (/),
    type (./),
    type (/:),
    ToPath (intoPath),
    IntoPath,
    R,
    Path (..),
    DPath (..),
    SDPath (..),
    SpecialPath (..),
    DSpecialPath (..),
    PathPart (..),
    DPathPart (..),
    NotFoundPathSym0,
    HasParam,
    hasParam,
  )
where

import Data.Bool.Singletons (FalseSym0, SBool (SFalse, STrue))
import Data.List.Singletons
  ( ElemSym0,
    NilSym0,
    SList (SCons, SNil),
    sElem,
    (%++),
    type (++@#@$),
    type (:@#@$),
  )
import Data.Singletons.TH (genSingletons, singletons)
import Data.Singletons.TH.Options
  ( Options (defunctionalizedName, promotedDataTypeOrConName),
    defaultOptions,
    withOptions,
  )
import Data.Text qualified as T
import GHC.TypeLits.Singletons (ErrorSym0, Symbol, sError)
import Language.Haskell.TH (Name)
import Prelude hiding ((/))

data DPathPart = DExact T.Text | DParam T.Text

data PathPart = Exact Symbol | Param Symbol

data DSpecialPath = DNotFoundPath deriving (Eq, Ord)

data SpecialPath = NotFoundPath deriving (Eq, Ord)

data DPath = DSpecial DSpecialPath | DParts [DPathPart]

data Path = Special SpecialPath | Parts [PathPart]

$( let customPromote :: Name -> Name
       customPromote n
         | n == ''T.Text = ''Symbol
         | n == ''DPathPart = ''PathPart
         | n == 'DExact = 'Exact
         | n == 'DParam = 'Param
         | n == ''DSpecialPath = ''SpecialPath
         | n == 'DNotFoundPath = 'NotFoundPath
         | n == ''DPath = ''Path
         | n == 'DSpecial = 'Special
         | n == 'DParts = 'Parts
         | otherwise = promotedDataTypeOrConName defaultOptions n

       customDefun :: Name -> Int -> Name
       customDefun n = defunctionalizedName defaultOptions (customPromote n)
    in withOptions
         defaultOptions
           { promotedDataTypeOrConName = customPromote,
             defunctionalizedName = customDefun
           }
         $ do
           decs1 <- genSingletons [''DPathPart, ''DSpecialPath, ''DPath]
           decs2 <-
             singletons
               [d|
                 pathPartsParams :: [DPathPart] -> [T.Text]
                 pathPartsParams [] = []
                 pathPartsParams (DExact _ : ps) = pathPartsParams ps
                 pathPartsParams (DParam p : ps) = p : pathPartsParams ps

                 hasParam :: DPath -> T.Text -> Bool
                 hasParam (DSpecial _) _ = False
                 hasParam (DParts parts) param = elem param (pathPartsParams parts)

                 infixl 9 /

                 (/) :: [DPathPart] -> T.Text -> [DPathPart]
                 xs / x = xs ++ [DExact x]

                 infixl 9 ./

                 (./) :: [DPathPart] -> T.Text -> [DPathPart]
                 (./) = (/)

                 infixl 9 /:

                 (/:) :: [DPathPart] -> T.Text -> [DPathPart]
                 xs /: x =
                   if x `elem` pathPartsParams xs
                     then error "Duplicate path parameter"
                     else xs ++ [DParam x]

                 class ToPath a where
                   intoPath :: a -> DPath

                 instance ToPath DSpecialPath where
                   intoPath = DSpecial

                 instance ToPath [DPathPart] where
                   intoPath = DParts
                 |]
           return $ decs1 ++ decs2
 )

type R = '[]

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy(Proxy))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Servant.API ((:>), Get, Post, JSON, Capture, Header, ReqBody, QueryParam, QueryParams, QueryFlag)
import Servant.Ruby (ruby, NameSpace(NameSpace))
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (fromStrict)
import Servant.Foreign (GenerateList, NoContent, Foreign, HasForeign, NoTypes)

main :: IO ()
main = defaultMain $ testGroup "golden tests"
  [ goldenVsString "static" "test/golden/expected/static.rb" (test (Proxy :: Proxy StaticApi))
  , goldenVsString "parameters" "test/golden/expected/parameters.rb" (test (Proxy :: Proxy ParametersApi))
  , goldenVsString "body" "test/golden/expected/body.rb" (test (Proxy :: Proxy BodyApi))
  , goldenVsString "header" "test/golden/expected/header.rb" (test (Proxy :: Proxy HeaderApi))
  , goldenVsString "query param" "test/golden/expected/query_param.rb" (test (Proxy :: Proxy QueryParamApi))
  , goldenVsString "query params" "test/golden/expected/query_params.rb" (test (Proxy :: Proxy QueryParamsApi))
  , goldenVsString "query flag" "test/golden/expected/query_flag.rb" (test (Proxy :: Proxy QueryFlagApi))
  ]


type StaticApi = "hello" :> "world" :> Get '[JSON] ()

type ParametersApi = Capture "butterfly" () :> Get '[JSON] ()

type BodyApi = ReqBody '[JSON] () :> Post '[JSON] ()

type HeaderApi = Header "moth" () :> Post '[JSON] ()

type QueryParamApi = QueryParam "spider" () :> Get '[JSON] ()

type QueryParamsApi = QueryParams "spiders" () :> Get '[JSON] ()

type QueryFlagApi = QueryFlag "vw-beetle" :> Get '[JSON] ()

test
  :: (GenerateList NoContent (Foreign NoContent api), HasForeign NoTypes NoContent api)
  => Proxy api
  -> IO ByteString
test = pure . encodeUtf8 . fromStrict . ruby (NameSpace ["Generated", "V1"] "Things")

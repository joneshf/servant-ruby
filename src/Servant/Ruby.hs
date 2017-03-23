{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module: Servant.Ruby
Description: Generate a Ruby client from a Servant API using Net::HTTP.
Copyright: (c) Hardy Jones, 2017
License: BSD3
Maintainer: jones3.hardy@gmail.com
Stability: Experimental

-}

module Servant.Ruby (NameSpace(..), ruby) where

import Control.Lens (filtered, folded, to, view, (^.), (^..), (&))

import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')

import Servant.Foreign
  ( ArgType(Flag, List, Normal)
  , Foreign
  , GenerateList
  , HasForeign
  , HeaderArg
  , NoContent
  , NoTypes
  , QueryArg
  , Req
  , Segment(Segment)
  , SegmentType(Cap, Static)
  , argName
  , argPath
  , captureArg
  , headerArg
  , isCapture
  , listFromAPI
  , path
  , queryArgName
  , queryArgType
  , queryStr
  , reqBody
  , reqFuncName
  , reqHeaders
  , reqMethod
  , reqUrl
  , snakeCaseL
  , _PathSegment
  )

import Text.Casing (quietSnake)

import qualified Data.Text as T

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeOperators
-- >>> :m + Servant.API
-- >>> :m + Data.Text.IO

{-|
The namespace for the generated class.
-}
data NameSpace
  = NameSpace
    { moduleNames :: [Text]
      -- ^ The list of namespaces you'd like the class to appear in.
    , className   :: Text
      -- ^ The name of the class you'd like the API methods to appear in.
    }

{-|
Generate a Ruby class with methods for the Servant API.

Currently assumes the API accepts and returns JSON.

For example:

>>> Data.Text.IO.putStr $ ruby (NameSpace [] "Baz") (Proxy :: Proxy (Get '[JSON] ()))
require "json"
require "net/http"
require "uri"
<BLANKLINE>
class Baz
  def initialize(origin)
    @origin = URI(origin)
    @http = Net::HTTP.new(@origin.host, @origin.port)
  end
<BLANKLINE>
  def get()
    uri = URI("#{@origin}")
<BLANKLINE>
    req = Net::HTTP::Get.new(uri)
<BLANKLINE>
    @http.request(req)
  end
end

The class can be nested in a module namespace if you choose so.

>>> Data.Text.IO.putStr $ ruby (NameSpace ["Foo", "Bar"] "Baz") (Proxy :: Proxy (Get '[JSON] ()))
require "json"
require "net/http"
require "uri"
<BLANKLINE>
module Foo
  module Bar
    class Baz
      def initialize(origin)
        @origin = URI(origin)
        @http = Net::HTTP.new(@origin.host, @origin.port)
      end
<BLANKLINE>
      def get()
        uri = URI("#{@origin}")
<BLANKLINE>
        req = Net::HTTP::Get.new(uri)
<BLANKLINE>
        @http.request(req)
      end
    end
  end
end

Captures and query parameters are translated into required arguments, in that order.

The request body and headers are translated into keyword arguments, in that order.

>>> let api = Proxy :: Proxy ("foo" :> Capture "fooId" Int :> ReqBody '[JSON] () :> QueryParam "bar" Bool :> Header "Max-Forwards" Int :> Post '[JSON] ())
>>> Data.Text.IO.putStr $ ruby (NameSpace [] "Foo") api
require "json"
require "net/http"
require "uri"
<BLANKLINE>
class Foo
  def initialize(origin)
    @origin = URI(origin)
    @http = Net::HTTP.new(@origin.host, @origin.port)
  end
<BLANKLINE>
  def post_foo_by_foo_id(foo_id, bar, body:, max_forwards:)
    uri = URI("#{@origin}/foo/#{fooId}?bar=#{bar}")
<BLANKLINE>
    req = Net::HTTP::Post.new(uri)
    req["Content-Type"] = "application/json"
    req["Max-Forwards"] = max_forwards
<BLANKLINE>
    @http.request(req, body)
  end
end
-}
ruby
  :: (GenerateList NoContent (Foreign NoContent api), HasForeign NoTypes NoContent api)
  => NameSpace
  -> Proxy api
  -> Text
ruby (NameSpace {..}) p =
  T.unlines $
    imports
    ++ prologue
    ++ body
    ++ epilogue
  where
  body =
    initialize indent
    ++ foldMap (public indent) api
  api =
    listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) p
  prologue =
    ((\(i, n) -> T.replicate i "  " <> "module " <> n) <$> zip [0..] moduleNames)
    ++ [T.replicate indent "  " <> "class " <> className]
  epilogue =
    reverse $ (\i -> T.replicate i "  " <> "end") <$> [0..indent]
  indent =
    length moduleNames

imports :: [Text]
imports =
  [ "require \"json\""
  , "require \"net/http\""
  , "require \"uri\""
  , ""
  ]

properIndent :: Functor f => Int -> f (Maybe Text) -> f Text
properIndent indent =
  fmap (maybe "" (T.replicate (indent + 1) "  " <>))

initialize :: Int -> [Text]
initialize indent =
  properIndent indent
    [ Just "def initialize(origin)"
    , Just "  @origin = URI(origin)"
    , Just "  @http = Net::HTTP.new(@origin.host, @origin.port)"
    , Just "end"
    ]

public :: Int -> Req NoContent -> [Text]
public indent req =
  properIndent indent $
    [ Nothing
    , Just $ "def " <> functionName <> "(" <> argsStr <> ")"
    , Just $ "  uri = URI(" <> url <> ")"
    , Nothing
    , Just $ "  req = Net::HTTP::" <> method <> ".new(uri)"
    ]
    ++ requestHeaders
    ++
    [ Nothing
    , Just $ "  @http.request" <> request
    , Just "end"
    ]
  where
  functionName :: Text
  functionName = req ^. reqFuncName.snakeCaseL.to snake

  argsStr  :: Text
  argsStr = T.intercalate ", " $ snake <$> args

  args :: [Text]
  args =
    captures
    ++ ((^. queryArgName.argPath) <$> queryparams)
    ++ body
    ++ headerArgs

  segments :: [Segment NoContent]
  segments = req ^. reqUrl.path^..folded.filtered isCapture

  queryparams :: [QueryArg NoContent]
  queryparams = req ^.. reqUrl.queryStr.traverse

  captures :: [Text]
  captures = view argPath . captureArg <$> segments

  body :: [Text]
  body =
    case req ^. reqBody of
      Just _  -> ["body:"]
      Nothing -> []

  request :: Text
  request =
    case req ^. reqBody of
      Just _  -> "(req, body)"
      Nothing -> "(req)"

  contentType :: [Maybe Text]
  contentType =
    case req ^. reqBody of
      Just _  -> [Just "  req[\"Content-Type\"] = \"application/json\""]
      Nothing -> []

  hs :: [HeaderArg NoContent]
  hs = req ^. reqHeaders

  requestHeaders :: [Maybe Text]
  requestHeaders =
    contentType
    ++ (requestHeader <$> rawHeaders)

  headerArgs :: [Text]
  headerArgs = (<> ":") <$> rawHeaders

  rawHeaders :: [Text]
  rawHeaders = (^. headerArg.argName._PathSegment) <$> hs

  method :: Text
  method =
    case req ^. reqMethod.to decodeUtf8' of
      Right m -> T.toTitle m
      Left _  -> "Get"

  url :: Text
  url = "\"#{@origin}" <> urlArgs <> queryArgs <> "\""

  urlArgs :: Text
  urlArgs = req ^.. reqUrl.path.traverse & rbSegments

  queryArgs :: Text
  queryArgs =
    if null queryparams then
      ""
    else
      "?" <> rbParams "&" queryparams

requestHeader :: Text -> Maybe Text
requestHeader header = Just $ "  req[\"" <> header <> "\"] = " <> snake header

rbSegments :: [Segment f] -> Text
rbSegments []     = ""
rbSegments [x]    = "/" <> segmentToStr x
rbSegments (x:xs) = "/" <> segmentToStr x <> rbSegments xs

segmentToStr :: Segment f -> Text
segmentToStr (Segment st) = segmentTypeToStr st

segmentTypeToStr :: SegmentType f -> Text
segmentTypeToStr (Static s) = s ^. _PathSegment
segmentTypeToStr (Cap s)    = "#{" <> s ^. argName._PathSegment <> "}"

rbParams :: Text -> [QueryArg f] -> Text
rbParams _ []     = ""
rbParams _ [x]    = paramToStr x
rbParams s (x:xs) = paramToStr x <> s <> rbParams s xs

paramToStr :: QueryArg f -> Text
paramToStr qarg =
  case qarg ^. queryArgType of
    Normal -> key <> "=#{" <> val <> "}"
    Flag   -> key
    List   -> key <> "[]=#{" <> val <> "}"
  where
  key = qarg ^. queryArgName.argName._PathSegment
  val = snake key

snake :: Text -> Text
snake = T.pack . quietSnake . T.unpack

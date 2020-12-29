{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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

import Control.Applicative (Const(Const, getConst))
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Monoid (Endo(Endo, appEndo), (<>))
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
  def initialize(origin, timeout = nil)
    @origin = URI(origin)
    @http = Net::HTTP.new(@origin.host, @origin.port)
<BLANKLINE>
    unless timeout.nil?
      @http.open_timeout = timeout
      @http.read_timeout = timeout
    end
    @http.use_ssl = @origin.scheme == 'https'
  end
<BLANKLINE>
  def get_uri()
    URI("#{@origin}")
  end
<BLANKLINE>
  def get()
    req = Net::HTTP::Get.new(get_uri())
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
      def initialize(origin, timeout = nil)
        @origin = URI(origin)
        @http = Net::HTTP.new(@origin.host, @origin.port)
<BLANKLINE>
        unless timeout.nil?
          @http.open_timeout = timeout
          @http.read_timeout = timeout
        end
        @http.use_ssl = @origin.scheme == 'https'
      end
<BLANKLINE>
      def get_uri()
        URI("#{@origin}")
      end
<BLANKLINE>
      def get()
        req = Net::HTTP::Get.new(get_uri())
<BLANKLINE>
        @http.request(req)
      end
    end
  end
end

Captures and query parameters are translated into required arguments, in that order.

The request body and headers are translated into keyword arguments, in that order.

>>> let api = Proxy :: Proxy ("foo" :> Capture "fooId" Int :> ReqBody '[JSON] () :> QueryParam "barId" Bool :> QueryParams "ids" Int :> Header "Max-Forwards" Int :> Post '[JSON] ())
>>> Data.Text.IO.putStr $ ruby (NameSpace [] "Foo") api
require "json"
require "net/http"
require "uri"
<BLANKLINE>
class Foo
  def initialize(origin, timeout = nil)
    @origin = URI(origin)
    @http = Net::HTTP.new(@origin.host, @origin.port)
<BLANKLINE>
    unless timeout.nil?
      @http.open_timeout = timeout
      @http.read_timeout = timeout
    end
    @http.use_ssl = @origin.scheme == 'https'
  end
<BLANKLINE>
  def post_foo_by_foo_id_uri(foo_id, bar_id, ids)
    foo_id = if foo_id.kind_of?(Array) then foo_id.join(',') else foo_id end
<BLANKLINE>
    URI("#{@origin}/foo/#{foo_id}?barId=#{bar_id}&#{ ids.collect { |x| 'ids[]=' + x.to_s }.join('&') }")
  end
<BLANKLINE>
  def post_foo_by_foo_id(foo_id, bar_id, ids, body:, max_forwards:)
    foo_id = if foo_id.kind_of?(Array) then foo_id.join(',') else foo_id end
<BLANKLINE>
    req = Net::HTTP::Post.new(post_foo_by_foo_id_uri(foo_id, bar_id, ids))
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
    [ Just "def initialize(origin, timeout = nil)"
    , Just "  @origin = URI(origin)"
    , Just "  @http = Net::HTTP.new(@origin.host, @origin.port)"
    , Nothing
    , Just "  unless timeout.nil?"
    , Just "    @http.open_timeout = timeout"
    , Just "    @http.read_timeout = timeout"
    , Just "  end"
    , Just "  @http.use_ssl = @origin.scheme == 'https'"
    , Just "end"
    ]

public :: Int -> Req NoContent -> [Text]
public indent req =
  properIndent indent $
    [ Nothing
    , Just $ "def " <> functionName <> "_uri(" <> argsStr <> ")"
    ]
    ++ cleanCaptures
    ++
    [ Just $ "  URI(" <> url <> ")"
    , Just "end"
    , Nothing
    , Just $ "def " <> functionName <> "(" <> allArgsStr <> ")"
    ]
    ++ cleanCaptures
    ++
    [ Just $ "  req = Net::HTTP::" <> method <> ".new(" <> functionName <> "_uri(" <> callArgsStr <> "))"
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

  cleanCaptures  :: [Maybe Text]
  cleanCaptures =
    case captures of
      [] -> []
      xs ->
        (Just . (<>) "  " . cleanCapture . snake <$> xs)
        ++ [ Nothing ]

  cleanCapture  :: Text -> Text
  cleanCapture c =
    T.concat
      [ c
      , " = if "
      , c
      , ".kind_of?(Array) then "
      , c
      , ".join(',') else "
      , c
      , " end"
      ]

  argsStr  :: Text
  argsStr = T.intercalate ", " args

  callArgsStr  :: Text
  callArgsStr = T.intercalate ", " callArgs

  callArgs :: [Text]
  callArgs = captures ++ (paramToCallArg <$> queryparams)

  allArgsStr  :: Text
  allArgsStr = T.intercalate ", " (args ++ bodyAndHeader)

  args :: [Text]
  args = captures ++ (paramToArg <$> queryparams)

  bodyAndHeader :: [Text]
  bodyAndHeader = snake <$> (body ++ headerArgs)

  segments :: [Segment NoContent]
  segments = filter isCapture paths
    where
    paths = toList (req ^. reqUrl.path)

  queryparams :: [QueryArg NoContent]
  queryparams = req ^.. reqUrl.queryStr.traverse

  captures :: [Text]
  captures = fmap snake $ view argPath . captureArg <$> segments

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
segmentTypeToStr (Cap s)    = "#{" <> s ^. argName._PathSegment.to snake <> "}"

rbParams :: Text -> [QueryArg f] -> Text
rbParams _ []     = ""
rbParams _ [x]    = paramToStr x
rbParams s (x:xs) = paramToStr x <> s <> rbParams s xs

paramToStr :: QueryArg f -> Text
paramToStr qarg =
  case qarg ^. queryArgType of
    Normal -> key <> "=#{" <> val <> "}"
    Flag   -> "#{" <> snake key <> " ? '" <> key <> "' : ''}"
    List   -> "#{ " <> val <> ".collect { |x| '" <> key <> "[]=' + x.to_s }.join('&') }"
  where
  key = qarg ^. queryArgName.argName._PathSegment
  val = snake key

paramToArg :: QueryArg f -> Text
paramToArg qarg =
  case qarg ^. queryArgType of
    Normal -> snake name
    Flag   -> snake name <> ": false"
    List   -> snake name
  where
  name = qarg ^. queryArgName.argPath

paramToCallArg :: QueryArg f -> Text
paramToCallArg qarg =
  case qarg ^. queryArgType of
    Normal -> snake name
    Flag   -> snake name <> ": " <> snake name
    List   -> snake name
  where
  name = qarg ^. queryArgName.argPath

snake :: Text -> Text
snake = T.pack . quietSnake . T.unpack

-- optics

to :: (s -> a) -> Getting a s a
to f a2fa s = s <$ (a2fa $ f s)
{-# INLINE to #-}

view :: Getting a s a -> s -> a
view s2a s = getConst (s2a Const s)
{-# INLINE view #-}

infixl 8 ^.

(^.) :: s -> Getting a s a -> a
x ^. l = view l x
{-# INLINE (^.) #-}

infixl 8 ^..

(^..) :: s -> Getting (Endo [a]) s a -> [a]
x ^.. l = appEndo (getConst $ l (Const . Endo . (:)) x) []
{-# INLINE (^..) #-}

type Getting r s a = (a -> Const r a) -> s -> Const r s

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Servant.Ruby (ruby) where

import Control.Lens (filtered, folded, to, view, (^.), (^..))

import Data.Function ((&))
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

data NameSpace
  = NameSpace
    { moduleNames :: [Text]
    , className   :: Text
    }

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
    initialize
    ++ foldMap (public indent) api
    ++ private
  initialize =
    fmap (maybe "" (T.replicate (indent + 1) "  " <>))
      [ Just "attr_reader :origin"
      , Nothing
      , Just "def initialize(origin:)"
      , Just "  @origin = URI(origin)"
      , Just "  @http = Net::HTTP.new(@origin.host, @origin.port)"
      , Just "end"
      ]
  private =
    fmap (maybe "" (T.replicate (indent + 1) "  " <>))
      [ Nothing
      , Just "private"
      , Nothing
      , Just "def request(req, body = nil)"
      , Just "  res = @http.request(req, body)"
      , Just "  res.body = JSON.parse(res.body, symbolize_names: true)"
      , Just "  res"
      , Just "end"
      ]
  api =
    listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) p
  imports =
    [ "require \"json\""
    , "require \"net/http\""
    , "require \"uri\""
    , ""
    ]
  prologue =
    ((\(i, n) -> T.replicate i "  " <> "module " <> n)<$> zip [0..] moduleNames)
    ++ [T.replicate indent "  " <> "class " <> className]
  epilogue =
    reverse $ (\i -> T.replicate i "  " <> "end") <$> [0..indent]
  indent =
    length moduleNames

public :: Int -> Req NoContent -> [Text]
public indent req =
  fmap (maybe "" (T.replicate (indent + 1) "  " <>)) $
    [ Nothing
    , Just $ "def " <> functionName <> "(" <> argsStr <> ")"
    , Just $ "  uri = URI(" <> url <> ")"
    , Nothing
    , Just $ "  req = Net::HTTP::" <> method <> ".new(uri)"
    ]
    ++ requestHeaders
    ++
    [ Nothing
    , Just $ "  request" <> request
    , Just "end"
    ]
  where
  functionName :: Text
  functionName =
    req ^. reqFuncName.snakeCaseL.to snake

  argsStr  :: Text
  argsStr =
      T.intercalate ", " $ snake <$> args

  args :: [Text]
  args =
    captures
    ++ (view (queryArgName.argPath) <$> queryparams)
    ++ body
    ++ headerArgs

  segments :: [Segment NoContent]
  segments =
    req ^. reqUrl.path^..folded.filtered isCapture

  queryparams :: [QueryArg NoContent]
  queryparams =
    req ^.. reqUrl.queryStr.traverse

  captures :: [Text]
  captures =
    view argPath . captureArg <$> segments

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

  accept :: [Maybe Text]
  accept =
    [Just "  req[\"Accept\"] = \"application/json\""]

  contentType :: [Maybe Text]
  contentType =
    case req ^. reqBody of
      Just _  -> [Just "  req[\"Content-Type\"] = \"application/json\""]
      Nothing -> []

  hs :: [HeaderArg NoContent]
  hs =
    req ^. reqHeaders

  requestHeaders :: [Maybe Text]
  requestHeaders =
    accept
    ++ contentType
    ++ (requestHeader <$> rawHeaders)

  requestHeader :: Text -> Maybe Text
  requestHeader header =
    Just $ "  req[\"" <> header <> "\"] = " <> snake header

  headerArgs :: [Text]
  headerArgs =
    (<> ":") <$> rawHeaders

  rawHeaders :: [Text]
  rawHeaders =
    (^. headerArg.argName._PathSegment) <$> hs

  snake :: Text -> Text
  snake =
    T.pack . quietSnake . T.unpack

  method :: Text
  method =
    case req ^. reqMethod.to decodeUtf8' of
      Right m -> T.toTitle m
      Left _  -> "Get"

  url :: Text
  url =
    "\"#{origin}" <> urlArgs <> queryArgs <> "\""

  urlArgs :: Text
  urlArgs =
    req ^.. reqUrl.path.traverse & rbSegments

  queryArgs :: Text
  queryArgs =
    if null queryparams then
      ""
    else
      "?" <> rbParams "&" queryparams

  rbSegments :: [Segment f] -> Text
  rbSegments []     = ""
  rbSegments [x]    = "/" <> segmentToStr x
  rbSegments (x:xs) = "/" <> segmentToStr x <> rbSegments xs

  segmentToStr :: Segment f -> Text
  segmentToStr (Segment st) =
    segmentTypeToStr st

  segmentTypeToStr :: SegmentType f -> Text
  segmentTypeToStr (Static s) =
    s ^. _PathSegment
  segmentTypeToStr (Cap s)    =
    "#{" <> s ^. argName._PathSegment <> "}"

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

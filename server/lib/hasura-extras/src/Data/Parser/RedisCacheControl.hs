module Data.Parser.RedisCacheControl
  ( RedisCacheControl,
    RedisCacheControlDirective (..),
    parseCacheControl,
    parseMaxAge,
    findMaxAge,
    noCacheExists,
    noStoreExists,
    mustRevalidateExists,
  )
where

import Data.Attoparsec.Text qualified as AT
import Data.Bifunctor (first)
import Data.Text qualified as T
import Hasura.Prelude hiding (first)

type RedisCacheControl = [RedisCacheControlDirective]

data CacheControlDirective
  = CCDOnlyToken !Text
  | CCDTokenWithVal !Text !Text
  deriving (Show, Eq)

-- | Tries to parse the @max-age@ or @s-maxage@ present in the value of @Cache-Control@ header
parseMaxAge :: (Integral a) => Text -> Either String a
parseMaxAge t =
  parseRedisCacheControl t >>= findMaxAge >>= maybe (Left notFoundErr) Right
  where
    notFoundErr = "could not find max-age/s-maxage"

findMaxAge :: (Integral a) => RedisCacheControl -> Either String (Maybe a)
findMaxAge cacheControl = do
  case findCCDTokenWithVal checkMaxAgeToken cacheControl of
    Just (_, val) -> Just <$> first parseErr (AT.parseOnly AT.decimal val)
    Nothing -> Right Nothing
  where
    parseErr _ = "could not parse max-age/s-maxage value"
    checkMaxAgeToken token = token == "max-age" || token == "s-maxage"

-- | Checks if the @no-cache@ directive is present
noCacheExists :: RedisCacheControl -> Bool
noCacheExists cacheControl =
  isJust $ findCCDOnlyToken (== "no-cache") cacheControl

-- | Checks if the @no-store@ directive is present
noStoreExists :: RedisCacheControl -> Bool
noStoreExists cacheControl =
  isJust $ findCCDOnlyToken (== "no-store") cacheControl

-- | Checks if the @must-revalidate@ directive is present
mustRevalidateExists :: RedisCacheControl -> Bool
mustRevalidateExists cacheControl =
  isJust $ findCCDOnlyToken (== "must-revalidate") cacheControl

findCCDOnlyToken :: (Text -> Bool) -> RedisCacheControl -> Maybe Text
findCCDOnlyToken tokenPredicate cacheControl =
  listToMaybe $ mapMaybe check cacheControl
  where
    check = \case
      CCDOnlyToken token -> if tokenPredicate token then Just token else Nothing
      _ -> Nothing

findCCDTokenWithVal :: (Text -> Bool) -> RedisCacheControl -> Maybe (Text, Text)
findCCDTokenWithVal tokenPredicate cacheControl =
  listToMaybe $ mapMaybe check cacheControl
  where
    check = \case
      CCDTokenWithVal token val -> if tokenPredicate token then Just (token, val) else Nothing
      _ -> Nothing

parseRedisCacheControl :: Text -> Either String RedisCacheControl
parseRedisCacheControl =
  T.splitOn ","
    >>> map T.strip
    >>> map parseCCD
    >>> sequence

parseCCD :: Text -> Either String RedisCacheControlDirective
parseCCD t =
  case T.splitOn "=" t of
    [token] -> Right $ CCDOnlyToken token
    [token, val] -> Right $ CCDTokenWithVal token val
    _ -> Left $ "could not parse cache control directive: " <> T.unpack t

-- | Parses a @Cache-Control@ header and returns a list of directives
parseCacheControl :: Text -> Either String RedisCacheControl
parseCacheControl = AT.parseOnly cacheControlParser

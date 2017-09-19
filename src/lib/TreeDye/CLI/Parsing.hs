{-# LANGUAGE TypeFamilies, ScopedTypeVariables, TypeApplications, BangPatterns,
             RecordWildCards, TupleSections, LambdaCase #-}

module TreeDye.CLI.Parsing (
  -- * Numbers
  -- ** Integral/rational all-in-one number type
  Number(..), foldNumber, numToRational, numToPercentage,
  -- ** Parsing
  number, percentage, numberOrPercentage,

  -- * Dimensions
  dimension,
  -- ** Kinds of dimensions
  dimensionOrRange, squareDimension,

  -- * Colors
  color, anyColor,
  -- ** Hex colors
  hexColor, hexColor',
  -- ** Named colors
  namedColor,
  -- ** RGB triples
  -- *** Specific RGB triple parsers
  rgbGeneric,
  rgb8, rgb16,
  rgbPercentage, rgbRational,
  -- *** RGB triple parser specifications
  RGBTriple(..), RGBTripleValidator(..),
  rgbTriple,
  -- *** Generic RGB triple parsers
  boundedRGBTriple, rationalRGBTriple,
  boundedRGBTripleValidator, rationalRGBTripleValidator,
  -- ** Randomization
  random,

  -- * Utility parsers
  keywords,

  -- * @optparse-applicative@ interoperability
  parsecReader
) where

import TreeDye.CLI.Types

import Data.Proxy
import Data.Void

import Data.Bifunctor
import Data.Foldable
import TreeDye.Util.Function
import Data.Either

import Numeric.Natural
import Data.Word

import Control.Monad

import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Data.Colour.Names

import Data.Char
import TreeDye.Util.String

import Text.Megaparsec
import Text.Megaparsec.Stream (chunkToTokens)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal, signed)

import qualified Options.Applicative as Opt

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

-- If any keyword is a prefix of another keyword, it must come later in the list
keywords :: (MonadParsec e s m, Tokens s ~ String) => [String] -> m ()
keywords = void . asum . map string'

--------------------------------------------------------------------------------
-- Numbers
--------------------------------------------------------------------------------

---------- Type ----------

data Number = NumNatural  !Natural
            | NumRational !Rational
            deriving (Eq, Ord, Show, Read)

foldNumber :: (Natural -> a) -> (Rational -> a) -> Number -> a
foldNumber nat _   (NumNatural  n) = nat n
foldNumber _   rat (NumRational r) = rat r

numToRational :: Number -> Rational
numToRational = foldNumber toRational id

numToPercentage :: Number -> Rational
numToPercentage = (/ 100) . numToRational

---------- Parsers ----------

number :: forall e s m. (MonadParsec e s m, Token s ~ Char) => m Number
number =
  let digits :: (a -> Char -> a) -> a -> m a
      digits f x    = foldl' f x . chunkToTokens (Proxy @s)
                        <$> takeWhile1P (Just "digit") isDigit
      
      (!n) `withDigit` (!d) = 10*n + fromIntegral (digitToInt d)
      
      integer       = digits withDigit 0
      fractional !i = digits (\(!n,!e) d -> (n `withDigit` d, e-1))
                             (i,0 :: Integer)
      exp           = option 0 $ char' 'e' *> signed (pure ()) decimal
      
      asRational !base !mag = toRational base * 10^^mag
      
      justFractional = do
        (base, mag) <- char '.' *> fractional 0
        mag'        <- exp
        pure . NumRational . asRational base $ mag+mag'
      
      optionalFractional = do
        whole <- integer
        mfrac <- optional $ char '.' *> option (whole, 0) (fractional whole)
        mag'  <- exp
        pure $ case mfrac of
          Nothing | mag' >= 0 -> NumNatural  $ whole * 10^mag'
                  | otherwise -> NumRational $ asRational whole mag'
          Just (base, mag)    -> NumRational . asRational base $ mag+mag'
  
  in justFractional <|> optionalFractional <?> "number"

percentage :: (MonadParsec e s m, Token s ~ Char) => m Rational
percentage = numToPercentage <$> number <* space <* char '%' <?> "percentage"

numberOrPercentage :: (MonadParsec e s m, Token s ~ Char) => m Number
numberOrPercentage = do
  n <- number
  optional (space <* char '%') <&> \case
    Nothing -> n
    Just () -> NumRational $ numToPercentage n

--------------------------------------------------------------------------------
-- Dimensions
--------------------------------------------------------------------------------

dimension :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String)
          => m Dimension
dimension =   either FixedDimension (uncurry Range) <$> dimensionOrRange
          <|> Square <$ squareDimension

dimensionOrRange :: (MonadParsec e s m, Token s ~ Char)
                 => m (Either Natural (Natural, Natural))
dimensionOrRange = do
  let natural = fromInteger <$> decimal <* space
  low  <- natural
  high <- optional $ char '-' *> space *> natural
  pure $ maybe (Left low) (Right . (low,)) high

squareDimension :: (MonadParsec e s m, Tokens s ~ String) => m ()
squareDimension = keywords $ words "square sq same x"

--------------------------------------------------------------------------------
-- Colors
--------------------------------------------------------------------------------

---------- Aggregate color parser ----------

color :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Color
color =   RandomColor <$  random -- Has to come first because of named colors
      <|> FixedColor  <$> anyColor

anyColor :: ( Ord c, Floating c
            , MonadParsec e s m, Token s ~ Char, Tokens s ~ String )
         => m (Colour c)
anyColor =   hexColor
         <|> rgb8 <|> rgb16 <|> rgbPercentage <|> rgbRational
         <|> rgbGeneric
         <|> namedColor

---------- Random color ----------

random :: (MonadParsec e s m, Tokens s ~ String) => m ()
random = keywords $ words "random rand"

---------- Hex colors ----------

hexColor' :: (MonadParsec e s m, Token s ~ Char) => m (RGB Word8)
hexColor' = do
  let hex3     = (,,) <$> hexDigitChar <*> hexDigitChar <*> hexDigitChar
      chan h l = fromIntegral $ 16*digitToInt h + digitToInt l
  void $ char '#'
  (h1,h2,h3) <- hex3
  optional hex3 <&> \case
    Just (h4,h5,h6) -> RGB (chan h1 h2) (chan h3 h4) (chan h5 h6)
    Nothing         -> RGB (chan h1 h1) (chan h2 h2) (chan h3 h3)

hexColor :: (Ord c, Floating c, MonadParsec e s m, Token s ~ Char)
         => m (Colour c)
hexColor = uncurryRGB sRGB24 <$> hexColor'

---------- Named colors ----------

namedColor :: (Ord c, Floating c, MonadParsec e s m, Token s ~ Char)
           => m (Colour c)
namedColor =   label "color name"
           $   maybe (fail "unknown color name") pure
           =<< readColourName . map toLower
           <$> some letterChar <* lookAhead (void spaceChar <|> eof)
               -- The lookahead is for error messages

---------- RGB triples: generic structure ----------

data RGBTripleValidator raw chan color =
  RGBTripleValidator { rgbtLow       :: !raw
                     , rgbtHigh      :: !raw
                     , rgbtToChannel :: !(raw -> chan)
                     , rgbtColor     :: !(chan -> chan -> chan -> color) }

data RGBTriple m raw chan color =
  RGBTriple { rgbtSuffix    :: !String
            , rgbtChannel   :: !(m raw)
            , rgbtValidator :: !(RGBTripleValidator raw chan color) }

rawRGBTriple :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String)
             => String -> m a -> m (a,a,a)
rawRGBTriple suffix channel = do
  string' ("rgb" ++ suffix) *> space
  char '(' *> space
  (,,) <$> channel <* space <* char ',' <* space
       <*> channel <* space <* char ',' <* space
       <*> channel <* space <* char ')'

validateRGBTriple :: Ord raw
                  => RGBTripleValidator raw chan color
                  -> (raw,raw,raw)
                  -> Either String color
validateRGBTriple RGBTripleValidator{..} (r',g',b') =
  let validate what c
        | rgbtLow <= c && c <= rgbtHigh = Right $ rgbtToChannel c
        | otherwise                     = Left  $ what
  in case partitionEithers [ validate "red"   r'
                           , validate "green" g'
                           , validate "blue"  b' ] of
       ([], [r,g,b]) -> Right $ rgbtColor r g b
       (errs, _)     -> Left $  describeWithList "and" errs "value" "values"
                             ++ " out of range"

rgbTriple :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String, Ord raw)
          => RGBTriple m raw chan color -> m color
rgbTriple RGBTriple{..} =
  either fail pure . validateRGBTriple rgbtValidator
    =<< rawRGBTriple rgbtSuffix rgbtChannel

---------- RGB triples: specific triples ----------

boundedRGBTripleValidator :: forall raw chan px
                          .  ( Integral raw
                             , Integral chan, Bounded chan
                             , Ord px, Floating px )
                          => RGBTripleValidator raw chan (Colour px)
boundedRGBTripleValidator =
  RGBTripleValidator { rgbtLow       = fromIntegral $ minBound @chan
                     , rgbtHigh      = fromIntegral $ maxBound @chan
                     , rgbtToChannel = fromIntegral
                     , rgbtColor     = sRGBBounded }

boundedRGBTriple :: ( Integral raw
                    , Integral chan, Bounded chan
                    , Ord px, Floating px
                    , MonadParsec e s m, Token s ~ Char )
                 => String -> RGBTriple m raw chan (Colour px)
boundedRGBTriple suff =
  RGBTriple { rgbtSuffix    = suff
            , rgbtChannel   = decimal
            , rgbtValidator = boundedRGBTripleValidator }

rationalRGBTripleValidator :: (Ord c, Floating c)
                           => RGBTripleValidator Rational c (Colour c)
rationalRGBTripleValidator =
  RGBTripleValidator { rgbtLow       = 0
                     , rgbtHigh      = 1
                     , rgbtToChannel = fromRational
                     , rgbtColor     = sRGB }

rationalRGBTriple :: (Ord c, Floating c, MonadParsec e s m, Token s ~ Char)
                  => String -> m Rational -> RGBTriple m Rational c (Colour c)
rationalRGBTriple suff chan =
  RGBTriple { rgbtSuffix    = suff
            , rgbtChannel   = chan
            , rgbtValidator = rationalRGBTripleValidator }

rgb8 :: ( Ord c, Floating c
        , MonadParsec e s m, Token s ~ Char, Tokens s ~ String )
     => m (Colour c)
rgb8 = rgbTriple $ boundedRGBTriple @Natural @Word8 "8"

rgb16 :: ( Ord c, Floating c
         , MonadParsec e s m, Token s ~ Char, Tokens s ~ String )
      => m (Colour c)
rgb16 = rgbTriple $ boundedRGBTriple @Natural @Word16 "16"

rgbPercentage :: ( Ord c, Floating c
                 , MonadParsec e s m, Token s ~ Char, Tokens s ~ String )
              => m (Colour c)
rgbPercentage = rgbTriple $ rationalRGBTriple "%" percentage

rgbRational :: ( Ord c, Floating c
               , MonadParsec e s m, Token s ~ Char, Tokens s ~ String )
            => m (Colour c)
rgbRational = rgbTriple $ rationalRGBTriple "f" (numToRational <$> number)

rgbGeneric :: ( Ord c, Floating c
              , MonadParsec e s m, Token s ~ Char, Tokens s ~ String )
           => m (Colour c)
rgbGeneric = rawRGBTriple "" numberOrPercentage >>= either fail pure . \case
  (NumNatural r, NumNatural g, NumNatural b) ->
    validateRGBTriple (boundedRGBTripleValidator @Natural @Word8)
                      (r,g,b)
  (r, g, b) ->
    validateRGBTriple rationalRGBTripleValidator
                      (numToRational r, numToRational g, numToRational b)

--------------------------------------------------------------------------------
-- optparse-applicative interoperability
--------------------------------------------------------------------------------

parsecReader :: Parsec Void String a -> Opt.ReadM a
parsecReader p = Opt.eitherReader $ first (init . parseErrorTextPretty)
               . runParser (space *> p <* space <* eof) ""

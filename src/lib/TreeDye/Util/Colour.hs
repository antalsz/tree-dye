{-|
Module      : TreeDye.Util.Colour
Description : Utilities for working with 'Colour's
Copyright   : © Antal Spector-Zabusky 2017–2018
License     : BSD3
Maintainer  : Antal Spector-Zabusky <antal.b.sz@gmail.com>

Utilities for working with 'Colour's.
-}

module TreeDye.Util.Colour (namedColours, colourNameList) where

import Prelude hiding (tan)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Colour
import Data.Colour.Names

-- |A 'Map' from color names to the corresponding 'Colour' values.  The reified
-- form of 'Data.Colour.Names.readColourName'.
--
-- > Data.Map.lookup name namedColours == readColourName name
--
-- We can't have a 'Map' that goes the other way, because 'Colour's are not
-- 'Ord'erable – instead, see 'colourNameList'.
--
-- This is a polymorphic constant, but has a specialization to @Colour Double@.
namedColours :: (Ord a, Floating a) => Map String (Colour a)
namedColours = M.fromDistinctAscList colourNameList
{-# SPECIALIZE namedColours :: Map String (Colour Double) #-}

-- |An association list mapping color names to the corresponding 'Colour'
-- values.  The reified form of 'Data.Colour.Names.readColourName'.
--
-- > lookup name colourNameList == readColourName name
--
-- If all you need to do is go from a color name to a 'Colour', use
-- 'namedColours' instead.
--
-- This is a polymorphic constant, but has a specialization to @Colour Double@.
colourNameList :: (Ord a, Floating a) => [(String, Colour a)]
colourNameList =
  [ ("aliceblue",            aliceblue)
  , ("antiquewhite",         antiquewhite)
  , ("aqua",                 aqua)
  , ("aquamarine",           aquamarine)
  , ("azure",                azure)
  , ("beige",                beige)
  , ("bisque",               bisque)
  , ("black",                black)
  , ("blanchedalmond",       blanchedalmond)
  , ("blue",                 blue)
  , ("blueviolet",           blueviolet)
  , ("brown",                brown)
  , ("burlywood",            burlywood)
  , ("cadetblue",            cadetblue)
  , ("chartreuse",           chartreuse)
  , ("chocolate",            chocolate)
  , ("coral",                coral)
  , ("cornflowerblue",       cornflowerblue)
  , ("cornsilk",             cornsilk)
  , ("crimson",              crimson)
  , ("cyan",                 cyan)
  , ("darkblue",             darkblue)
  , ("darkcyan",             darkcyan)
  , ("darkgoldenrod",        darkgoldenrod)
  , ("darkgray",             darkgray)
  , ("darkgreen",            darkgreen)
  , ("darkgrey",             darkgrey)
  , ("darkkhaki",            darkkhaki)
  , ("darkmagenta",          darkmagenta)
  , ("darkolivegreen",       darkolivegreen)
  , ("darkorange",           darkorange)
  , ("darkorchid",           darkorchid)
  , ("darkred",              darkred)
  , ("darksalmon",           darksalmon)
  , ("darkseagreen",         darkseagreen)
  , ("darkslateblue",        darkslateblue)
  , ("darkslategray",        darkslategray)
  , ("darkslategrey",        darkslategrey)
  , ("darkturquoise",        darkturquoise)
  , ("darkviolet",           darkviolet)
  , ("deeppink",             deeppink)
  , ("deepskyblue",          deepskyblue)
  , ("dimgray",              dimgray)
  , ("dimgrey",              dimgrey)
  , ("dodgerblue",           dodgerblue)
  , ("firebrick",            firebrick)
  , ("floralwhite",          floralwhite)
  , ("forestgreen",          forestgreen)
  , ("fuchsia",              fuchsia)
  , ("gainsboro",            gainsboro)
  , ("ghostwhite",           ghostwhite)
  , ("gold",                 gold)
  , ("goldenrod",            goldenrod)
  , ("gray",                 gray)
  , ("grey",                 grey)
  , ("green",                green)
  , ("greenyellow",          greenyellow)
  , ("honeydew",             honeydew)
  , ("hotpink",              hotpink)
  , ("indianred",            indianred)
  , ("indigo",               indigo)
  , ("ivory",                ivory)
  , ("khaki",                khaki)
  , ("lavender",             lavender)
  , ("lavenderblush",        lavenderblush)
  , ("lawngreen",            lawngreen)
  , ("lemonchiffon",         lemonchiffon)
  , ("lightblue",            lightblue)
  , ("lightcoral",           lightcoral)
  , ("lightcyan",            lightcyan)
  , ("lightgoldenrodyellow", lightgoldenrodyellow)
  , ("lightgray",            lightgray)
  , ("lightgreen",           lightgreen)
  , ("lightgrey",            lightgrey)
  , ("lightpink",            lightpink)
  , ("lightsalmon",          lightsalmon)
  , ("lightseagreen",        lightseagreen)
  , ("lightskyblue",         lightskyblue)
  , ("lightslategray",       lightslategray)
  , ("lightslategrey",       lightslategrey)
  , ("lightsteelblue",       lightsteelblue)
  , ("lightyellow",          lightyellow)
  , ("lime",                 lime)
  , ("limegreen",            limegreen)
  , ("linen",                linen)
  , ("magenta",              magenta)
  , ("maroon",               maroon)
  , ("mediumaquamarine",     mediumaquamarine)
  , ("mediumblue",           mediumblue)
  , ("mediumorchid",         mediumorchid)
  , ("mediumpurple",         mediumpurple)
  , ("mediumseagreen",       mediumseagreen)
  , ("mediumslateblue",      mediumslateblue)
  , ("mediumspringgreen",    mediumspringgreen)
  , ("mediumturquoise",      mediumturquoise)
  , ("mediumvioletred",      mediumvioletred)
  , ("midnightblue",         midnightblue)
  , ("mintcream",            mintcream)
  , ("mistyrose",            mistyrose)
  , ("moccasin",             moccasin)
  , ("navajowhite",          navajowhite)
  , ("navy",                 navy)
  , ("oldlace",              oldlace)
  , ("olive",                olive)
  , ("olivedrab",            olivedrab)
  , ("orange",               orange)
  , ("orangered",            orangered)
  , ("orchid",               orchid)
  , ("palegoldenrod",        palegoldenrod)
  , ("palegreen",            palegreen)
  , ("paleturquoise",        paleturquoise)
  , ("palevioletred",        palevioletred)
  , ("papayawhip",           papayawhip)
  , ("peachpuff",            peachpuff)
  , ("peru",                 peru)
  , ("pink",                 pink)
  , ("plum",                 plum)
  , ("powderblue",           powderblue)
  , ("purple",               purple)
  , ("red",                  red)
  , ("rosybrown",            rosybrown)
  , ("royalblue",            royalblue)
  , ("saddlebrown",          saddlebrown)
  , ("salmon",               salmon)
  , ("sandybrown",           sandybrown)
  , ("seagreen",             seagreen)
  , ("seashell",             seashell)
  , ("sienna",               sienna)
  , ("silver",               silver)
  , ("skyblue",              skyblue)
  , ("slateblue",            slateblue)
  , ("slategray",            slategray)
  , ("slategrey",            slategrey)
  , ("snow",                 snow)
  , ("springgreen",          springgreen)
  , ("steelblue",            steelblue)
  , ("tan",                  tan)
  , ("teal",                 teal)
  , ("thistle",              thistle)
  , ("tomato",               tomato)
  , ("turquoise",            turquoise)
  , ("violet",               violet)
  , ("wheat",                wheat)
  , ("white",                white)
  , ("whitesmoke",           whitesmoke)
  , ("yellow",               yellow)
  , ("yellowgreen",          yellowgreen) ]
{-# SPECIALIZE colourNameList :: [(String, Colour Double)] #-}

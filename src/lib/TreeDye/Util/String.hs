{-|
Module      : TreeDye.Util.String
Description : Utilities for formatting strings
Copyright   : © Antal Spector-Zabusky 2017–2018
License     : BSD3
Maintainer  : Antal Spector-Zabusky <antal.b.sz@gmail.com>

Utilities for formatting strings.
-}

module TreeDye.Util.String (describeWithList) where

-- |Format a list for English-language human-facing output.  Takes as its arguments:
--
-- 1. The conjunction for the list (e.g., @"and"@, @"or"@);
-- 2. The list of items;
-- 3. The singular noun for an item in the list (e.g., @"item"@); and
-- 4. The plural noun for an item in the list (e.g., @"items"@).
--
-- It is easiest to understand 'describeWithList' by example.  To talk about the
-- color channels in an image, we might have any of the following:
--
-- >>> describeWithList "and" ["cyan", "magenta", "yellow", "black", "alpha"] "channel" "channels"
-- "cyan, magenta, yellow, black, and alpha channels"
-- >>> describeWithList "and" ["red", "green", "blue"] "channel" "channels"
-- "red, green, and blue channels"
-- >>> describeWithList "and" ["black", "white"] "channel" "channels"
-- "black and white channels"
-- >>> describeWithList "and" ["color"] "channel" "channels"
-- "color channel"
-- >>> describeWithList "and" [] "channel" "channels"
-- "no channels"
--
-- More completely: Given @describeWithList conj things one many@, concatenate
-- the elements of @things@ separated with commas and spaces, placing the
-- conjunction @conj@ before the last item.  If there are only two items, there
-- are no commas; if there is only one item, there are neither commas nor a
-- conjunction; and if there are no items, say "no".  Then append @one@ or
-- @many@, preceded by a space, depending on if there was one item or not in the
-- list (zero and more than one are handled identically).  If any of @conj@,
-- @one@, or @many@ is empty, it is ignored.
describeWithList :: String -> [String] -> String -> String -> String
describeWithList conj things one many = unwords $ case things of
  []      -> "no" : present many
  [s]     -> s : present one
  [s1,s2] -> s1 : present conj ++ s2 : present many
  ss      -> map (++ ",") (init ss) ++ present conj ++ last ss : present many
  where present "" = []
        present s  = [s]

module TreeDye.Util.String (describeWithList) where

describeWithList :: String -> [String] -> String -> String -> String
describeWithList conj things one many = unwords $ case things of
  []      -> ["no", many]
  [s]     -> [s, one]
  [s1,s2] -> [s1, conj, s2, many]
  ss      -> map (++ ",") (init ss) ++ [conj, last ss, many]

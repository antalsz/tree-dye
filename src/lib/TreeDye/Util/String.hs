module TreeDye.Util.String (describeWithList) where

describeWithList :: String -> [String] -> String -> String -> String
describeWithList conj things one many = unwords $ case things of
  []      -> "no" : present many
  [s]     -> s : present one
  [s1,s2] -> [s1, conj, s2] ++ present many
  ss      -> map (++ ",") (init ss) ++ [conj, last ss] ++ present many
  where present "" = []
        present s  = [s]

module Main where

countChars :: Text -> [(Char, Int)]
countChars txt =
  txt
    & Text.foldl
        (\m chr -> Map.alter (Just . maybe 1 (+1)) chr m)
        Map.empty
    & Map.toList
    & sortOn (Down . snd)

main :: IO ()
main =
  countChars (Text.pack "a peck of pickled peppers")
    & mapM_ print

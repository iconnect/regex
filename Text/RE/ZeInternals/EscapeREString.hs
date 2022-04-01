module Text.RE.ZeInternals.EscapeREString where

-- | Convert a string into a regular expression that will match that
-- string
escapeREString :: String -> String
escapeREString = foldr esc []
  where
    esc c t | isMetaChar c = '\\' : c : t
            | otherwise    = c : t

-- | returns True iff the character is an RE meta character
-- ('[', '*', '{', etc.)
isMetaChar :: Char -> Bool
isMetaChar c = case c of
  '^'  -> True
  '\\' -> True
  '.'  -> True
  '|'  -> True
  '*'  -> True
  '?'  -> True
  '+'  -> True
  '('  -> True
  ')'  -> True
  '['  -> True
  ']'  -> True
  '{'  -> True
  '}'  -> True
  '$'  -> True
  _    -> False

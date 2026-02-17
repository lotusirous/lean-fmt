{-# LANGUAGE OverloadedStrings #-}

module LeanFmt (
  -- * High-level API
  formatLean,
  formatLeanSafe,
  formatWith,
  formatWithSafe,
  Rule,
  defaultRules,
  ruleColonType,
  ruleNumericOp,
) where

import Data.Char (isDigit, isLetter)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as B

{- | A rewrite rule operating on code (not comments/strings).

Rules must be semantics-safe:
- Only insert whitespace (never delete or reorder characters).
- Only match when you are confident it preserves Lean semantics.
- Return the remaining input to continue scanning.
-}
type Rule = Maybe Char -> Text -> Maybe Rewrite

data Rewrite = Rewrite
  { rewriteOut :: !B.Builder
  , rewritePrev :: !(Maybe Char)
  , rewriteRest :: !Text
  }

data State
  = Code
  | LineComment
  | BlockComment !Int
  | Str
  | RawStr !Int
  | QuotedIdent
  deriving (Eq, Show)

-- | Default rule set (composable): type-colon spacing + numeric +/* spacing.
defaultRules :: [Rule]
defaultRules =
  [ ruleColonType
  -- , ruleNumericOp '+'
  -- , ruleNumericOp '*'
  -- , ruleNumericOp '/'
  -- , ruleNumericOp '-'
  ]

-- | Format with the default rules. On tokenization failure, returns the input unchanged.
formatLean :: Text -> Text
formatLean = formatWith defaultRules

-- | Returns 'Nothing' if tokenization fails (unterminated string/raw string/block comment).
formatLeanSafe :: Text -> Maybe Text
formatLeanSafe = formatWithSafe defaultRules

-- | Format with a custom rule list. On tokenization failure, returns the input unchanged.
formatWith :: [Rule] -> Text -> Text
formatWith rs input = fromMaybe input (formatWithSafe rs input)

-- | Format with a custom rule list. Returns 'Nothing' on tokenization failure.
formatWithSafe :: [Rule] -> Text -> Maybe Text
formatWithSafe rs input = do
  b <- go rs Code Nothing input mempty
  pure (LT.toStrict (B.toLazyText b))

go :: [Rule] -> State -> Maybe Char -> Text -> B.Builder -> Maybe B.Builder
go rs st prev t acc =
  case T.uncons t of
    Nothing ->
      case st of
        Code -> Just acc
        LineComment -> Just acc
        BlockComment _ -> Nothing
        Str -> Nothing
        RawStr _ -> Nothing
        QuotedIdent -> Nothing
    Just (c, rest) ->
      case st of
        Code -> stepCode rs prev t acc
        LineComment ->
          if c == '\n'
            then go rs Code (Just '\n') rest (acc <> B.singleton '\n')
            else go rs LineComment (Just c) rest (acc <> B.singleton c)
        BlockComment depth ->
          case c of
            '/' ->
              case T.uncons rest of
                Just ('-', rest2) ->
                  go rs (BlockComment (depth + 1)) (Just '-') rest2 (acc <> B.fromText "/-")
                _ -> go rs (BlockComment depth) (Just '/') rest (acc <> B.singleton '/')
            '-' ->
              case T.uncons rest of
                Just ('/', rest2) ->
                  if depth == 1
                    then go rs Code (Just '/') rest2 (acc <> B.fromText "-/")
                    else go rs (BlockComment (depth - 1)) (Just '/') rest2 (acc <> B.fromText "-/")
                _ -> go rs (BlockComment depth) (Just '-') rest (acc <> B.singleton '-')
            _ -> go rs (BlockComment depth) (Just c) rest (acc <> B.singleton c)
        Str ->
          case c of
            '\\' ->
              case T.uncons rest of
                Nothing -> Nothing
                Just (c2, rest2) ->
                  go rs Str (Just c2) rest2 (acc <> B.singleton '\\' <> B.singleton c2)
            '"' -> go rs Code (Just '"') rest (acc <> B.singleton '"')
            _ -> go rs Str (Just c) rest (acc <> B.singleton c)
        RawStr hashCount ->
          if c == '"'
            then
              let (hashes, rest2) = T.splitAt hashCount rest
               in if T.length hashes == hashCount && T.all (== '#') hashes
                    then
                      let prev' = if hashCount == 0 then Just '"' else Just '#'
                       in go rs Code prev' rest2 (acc <> B.singleton '"' <> B.fromText hashes)
                    else go rs (RawStr hashCount) (Just '"') rest (acc <> B.singleton '"')
            else go rs (RawStr hashCount) (Just c) rest (acc <> B.singleton c)
        QuotedIdent ->
          if c == '»'
            then go rs Code (Just '»') rest (acc <> B.singleton '»')
            else go rs QuotedIdent (Just c) rest (acc <> B.singleton c)

-- Code-state scanning is split into:
-- 1) lexical constructs (comments/strings/raw strings/char literals)
-- 2) a list of rewrite rules (easy to compose/extend)
stepCode :: [Rule] -> Maybe Char -> Text -> B.Builder -> Maybe B.Builder
stepCode rs prev t acc =
  case T.uncons t of
    Nothing -> Just acc
    Just (c, rest) ->
      case c of
        ' ' ->
          let (spaces, restSpaces) = T.span (== ' ') rest
           in case T.uncons restSpaces of
                -- Collapse any run of spaces immediately before a type colon.
                Just (':', afterColon) ->
                  case T.uncons afterColon of
                    Just (c2, rest2) | isIdentStart c2 ->
                      let before =
                            case prev of
                              Just ' ' -> mempty
                              Just _ -> B.singleton ' '
                              Nothing -> mempty
                          out =
                            before
                              <> B.fromText ": "
                              <> B.singleton c2
                       in go rs Code (Just c2) rest2 (acc <> out)
                    _ ->
                      let spaceCount = 1 + T.length spaces
                          outSpaces = B.fromText (T.replicate spaceCount " ")
                       in go rs Code (Just ' ') restSpaces (acc <> outSpaces)
                _ ->
                  let spaceCount = 1 + T.length spaces
                      outSpaces = B.fromText (T.replicate spaceCount " ")
                   in go rs Code (Just ' ') restSpaces (acc <> outSpaces)
        '-' ->
          case T.uncons rest of
            Just ('-', rest2) -> go rs LineComment (Just '-') rest2 (acc <> B.fromText "--")
            _ -> applyOrEmit rs prev t acc
        '/' ->
          case T.uncons rest of
            Just ('-', rest2) -> go rs (BlockComment 1) (Just '-') rest2 (acc <> B.fromText "/-")
            _ -> applyOrEmit rs prev t acc
        'r' ->
          case parseRawOpen t of
            Just (hashCount, consumed, rest2) ->
              go rs (RawStr hashCount) (Just '"') rest2 (acc <> B.fromText consumed)
            Nothing ->
              applyOrEmit rs prev t acc
        '"' -> go rs Str (Just '"') rest (acc <> B.singleton '"')
        '«' -> go rs QuotedIdent (Just '«') rest (acc <> B.singleton '«')
        '\'' ->
          case parseCharLiteral t of
            Just (consumed, rest2) -> go rs Code (Just '\'') rest2 (acc <> B.fromText consumed)
            Nothing -> go rs Code (Just '\'') rest (acc <> B.singleton '\'')
        _ -> applyOrEmit rs prev t acc

applyOrEmit :: [Rule] -> Maybe Char -> Text -> B.Builder -> Maybe B.Builder
applyOrEmit rs prev t acc =
  case applyRules rs prev t of
    Just Rewrite{rewriteOut, rewritePrev, rewriteRest} ->
      go rs Code rewritePrev rewriteRest (acc <> rewriteOut)
    Nothing ->
      case T.uncons t of
        Nothing -> Just acc
        Just (c, rest) -> go rs Code (Just c) rest (acc <> B.singleton c)

applyRules :: [Rule] -> Maybe Char -> Text -> Maybe Rewrite
applyRules rs prev t = asum (map (\r -> r prev t) rs)

isIdentStart :: Char -> Bool
isIdentStart c = c == '_' || isLetter c

-- | Rule: `n:Nat` -> `n : Nat` and `:Nat` -> `: Nat` (only in code).
ruleColonType :: Rule
ruleColonType prev t = do
  (':', afterColon0) <- T.uncons t
  let (_spaces, rest) = T.span (== ' ') afterColon0
  (c2, rest2) <- T.uncons rest
  if prev /= Just ':' && isIdentStart c2
    then
      let before =
            case prev of
              Just ' ' -> mempty
              Just _ -> B.singleton ' '
              Nothing -> mempty
       in Just
            Rewrite
              { rewriteOut = before <> B.fromText ": " <> B.singleton c2
              , rewritePrev = Just c2
              , rewriteRest = rest2
              }
    else Nothing

{- | Rule: `5+10` -> `5 + 10` (only inserts spaces; never deletes).
Applied only when the operator is adjacent to digits.
-}
ruleNumericOp :: Char -> Rule
ruleNumericOp op prev t = do
  (c, rest) <- T.uncons t
  if c /= op
    then Nothing
    else do
      let before = case prev of
            Just p | isDigit p -> B.singleton ' '
            _ -> mempty
          after =
            case T.uncons rest of
              Just (n, _) | isDigit n -> B.singleton ' '
              _ -> mempty
          out = before <> B.singleton op <> after
          prev' = Just (if after == mempty then op else ' ')
      pure
        Rewrite
          { rewriteOut = out
          , rewritePrev = prev'
          , rewriteRest = rest
          }

-- Lean raw strings: r#"..."#, r##"..."##, etc.
parseRawOpen :: Text -> Maybe (Int, Text, Text)
parseRawOpen txt = do
  ('r', afterR) <- T.uncons txt
  let (hashes, afterHashes) = T.span (== '#') afterR
  ('"', afterQuote) <- T.uncons afterHashes
  let n = T.length hashes
      consumedLen = 2 + n
      consumed = T.take consumedLen txt
  pure (n, consumed, afterQuote)

-- Lean char literals are short: 'a' or '\\n' or '\\''.
-- If this doesn't match, treat '\'' as a normal character (e.g. prime in identifiers).
parseCharLiteral :: Text -> Maybe (Text, Text)
parseCharLiteral txt = do
  ('\'', rest1) <- T.uncons txt
  case T.uncons rest1 of
    Nothing -> Nothing
    Just ('\\', rest2) -> do
      (_escaped, rest3) <- T.uncons rest2
      ('\'', rest4) <- T.uncons rest3
      let consumed = T.take 4 txt
      pure (consumed, rest4)
    Just (_c1, rest2) -> do
      ('\'', rest3) <- T.uncons rest2
      let consumed = T.take 3 txt
      pure (consumed, rest3)

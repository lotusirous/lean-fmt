{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char (isSpace)
import Data.Text qualified as T
import LeanFmt (formatLean)
import Test.QuickCheck (Property, (==>))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main =
  defaultMain $
    testGroup
      "lean-fmt"
      [ unitTests
      , propertyTests
      ]

unitTests =
  testGroup
    "unit"
    [ testCase "constructor types: add space after ':'" $
        formatLean "| false :Boolean\n| true :Boolean\n"
          @?= "| false : Boolean\n| true : Boolean\n"
    , testCase "function types: space around ':' in binder and return" $
        formatLean "def add1(n:Nat) :Nat := n + 1\n"
          @?= "def add1(n : Nat) : Nat := n + 1\n"
    , testCase "collapse multiple spaces before ':' in binder" $
        formatLean "def add1(n  :Nat)  :Nat := n + 1\n"
          @?= "def add1(n : Nat) : Nat := n + 1\n"
    , testCase "multiple binders: normalize both parameter types" $
        formatLean
          "def maximum (n: Nat) (k : Nat) : Nat :=\n  if n < k then k\n  else n\n"
          @?= "def maximum (n : Nat) (k : Nat) : Nat :=\n  if n < k then k\n  else n\n"
    , testCase "idempotent on already formatted" $
        formatLean "| false : Boolean\n" @?= "| false : Boolean\n"
    , testCase "do not touch line comments" $
        formatLean "-- x:Nat\n" @?= "-- x:Nat\n"
    , testCase "format code but not comment tail" $
        formatLean "def x:Nat := 1 -- y:Nat\n"
          @?= "def x : Nat := 1 -- y:Nat\n"
    , testCase "do not touch normal strings" $
        formatLean "def s := \"x:Nat\"\n"
          @?= "def s := \"x:Nat\"\n"
    , testCase "do not touch ':' inside strings" $
        formatLean "def s := \":\"\n"
          @?= "def s := \":\"\n"
    , testCase "do not touch raw strings" $
        formatLean "def s := r#\"x:Nat\"#\n"
          @?= "def s := r#\"x:Nat\"#\n"
    , testCase "unterminated string: return input unchanged" $
        formatLean "def s := \"x:Nat\n"
          @?= "def s := \"x:Nat\n"
    , testCase "unterminated block comment: return input unchanged" $
        formatLean "def x:Nat := 1\n/- comment\n"
          @?= "def x:Nat := 1\n/- comment\n"
    , testCase "do not touch quoted identifiers «... »" $
        formatLean "def «x:Nat» := 1\n"
          @?= "def «x:Nat» := 1\n"
    , testCase "do not break := or ::" $
        formatLean "def x := 1\n#check List.nil :: List Nat\n"
          @?= "def x := 1\n#check List.nil :: List Nat\n"
    , testCase "colon formats only when followed by identifier (and adds space before)" $
        formatLean "def f(x:Nat) := (x:Nat)\n"
          @?= "def f(x : Nat) := (x : Nat)\n"
    , testCase "do not touch colon inside char literal" $
        formatLean "let c := ':'\n"
          @?= "let c := ':'\n"
    , testCase "do not touch colon inside block comment" $
        formatLean "def n : Nat := 1\n/- type : Nat -/\n"
          @?= "def n : Nat := 1\n/- type : Nat -/\n"
    , testCase "do not add space for :> or :< (coercion / type ascription)" $
        formatLean "def f (a :> Nat) : Nat := a\n"
          @?= "def f (a :> Nat) : Nat := a\n"
    , testCase "colon inside nested block comment unchanged" $
        formatLean "/- outer /- inner :Nat -/ -/\n"
          @?= "/- outer /- inner :Nat -/ -/\n"
    , testCase "double space in abbrev" $
        formatLean "abbrev N  : Type := Nat"
          @?= "abbrev N : Type := Nat"
    , testCase "trim trailing space" $
        formatLean "def x : Nat := 1  \n"
          @?= "def x : Nat := 1\n"
    , testCase "indent normalized to 2 spaces" $
        formatLean "def x : Nat :=\n if True then\n   1\n"
          @?= "def x : Nat :=\n  if True then\n    1\n"
    -- Edge cases: empty / minimal input
    , testCase "empty input unchanged" $
        formatLean "" @?= ""
    , testCase "only newline unchanged" $
        formatLean "\n" @?= "\n"
    , testCase "only spaces (no newline) trimmed to empty" $
        formatLean "   " @?= ""
    , testCase "blank line with spaces becomes empty line" $
        formatLean "a\n  \nb" @?= "a\n\nb"
    -- Edge cases: colon rule
    , testCase "colon at end of input unchanged" $
        formatLean "def x :" @?= "def x :"
    , testCase "colon followed by digit unchanged" $
        formatLean "x:0" @?= "x:0"
    , testCase "type at start of input" $
        formatLean ":Type" @?= ": Type"
    , testCase "do not add space for :=" $
        formatLean "def x:=1" @?= "def x:=1"
    -- Edge cases: layout (trim + indent)
    , testCase "tab indent becomes 2 spaces" $
        formatLean "\tdef x := 1" @?= "  def x := 1"
    , testCase "one space indent rounds to 2" $
        formatLean " x" @?= "  x"
    , testCase "trailing tab trimmed" $
        formatLean "x\t\n" @?= "x\n"
    , testCase "multiple blank lines preserved" $
        formatLean "a\n\n\nb" @?= "a\n\n\nb"
    , testCase "mixed tab and space indent normalized" $
        formatLean "\t  y" @?= "    y"
    -- Edge cases: unterminated constructs (return input unchanged)
    , testCase "unterminated raw string unchanged" $
        formatLean "def s := r#\"x" @?= "def s := r#\"x"
    , testCase "unterminated quoted identifier unchanged" $
        formatLean "def «x" @?= "def «x"
    -- Edge cases: colon after newline / at line start
    , testCase "colon at line start gets space after (indent normalized to 2)" $
        formatLean "\n:Type" @?= "\n  : Type"
    , testCase "string with newline unchanged" $
        formatLean "def s := \"line1\nline2\"" @?= "def s := \"line1\nline2\""
    , testCase "empty block comment unchanged" $
        formatLean "/-**/-" @?= "/-**/-"
    ]

propertyTests =
  testGroup
    "properties"
    [ testProperty "idempotent" prop_idempotent
    , testProperty "preserves non-whitespace" prop_preservesNonWhitespace
    , testProperty "unterminated string without quotes stays unchanged" prop_unterminatedStringNoQuote
    ]

prop_idempotent :: String -> Bool
prop_idempotent s =
  let t = T.pack s
   in formatLean (formatLean t) == formatLean t

prop_preservesNonWhitespace :: String -> Bool
prop_preservesNonWhitespace s =
  let t = T.pack s
      out = formatLean t
      nonWs = T.filter (\c -> not (isSpace c))
   in nonWs out == nonWs t

prop_unterminatedStringNoQuote :: String -> Property
prop_unterminatedStringNoQuote s =
  '"' `notElem` s ==>
    let t = T.pack ('"' : s)
     in formatLean t == t

package main

import (
	"strings"
	"testing"
	"unicode"

	"golang.org/x/tools/txtar"
)

var tests = []struct {
	name    string
	archive string
}{
	{
		name: "constructor types: add space after ':'",
		archive: `
-- input.lean --
| false :Boolean
| true :Boolean
-- output.lean --
| false : Boolean
| true : Boolean
`,
	},
	{
		name: "function types: space around ':' in binder and return",
		archive: `
-- input.lean --
def add1(n:Nat) :Nat := n + 1
-- output.lean --
def add1(n : Nat) : Nat := n + 1
`,
	},
	{
		name: "collapse multiple spaces before ':' in binder",
		archive: `
-- input.lean --
def add1(n  :Nat)  :Nat := n + 1
-- output.lean --
def add1(n : Nat) : Nat := n + 1
`,
	},
	{
		name: "multiple binders: normalize both parameter types",
		archive: `
-- input.lean --
def maximum (n: Nat) (k : Nat) : Nat :=
  if n < k then k
  else n
-- output.lean --
def maximum (n : Nat) (k : Nat) : Nat :=
  if n < k then k
  else n
`,
	},
	{
		name: "idempotent on already formatted",
		archive: `
-- input.lean --
| false : Boolean
-- output.lean --
| false : Boolean
`,
	},
	{
		name: "do not touch line comments",
		archive: `
-- input.lean --
-- x:Nat
-- output.lean --
-- x:Nat
`,
	},
	{
		name: "format code but not comment tail",
		archive: `
-- input.lean --
def x:Nat := 1 -- y:Nat
-- output.lean --
def x : Nat := 1 -- y:Nat
`,
	},
	{
		name: "do not touch normal strings",
		archive: `
-- input.lean --
def s := "x:Nat"
-- output.lean --
def s := "x:Nat"
`,
	},
	{
		name: "do not touch ':' inside strings",
		archive: `
-- input.lean --
def s := ":"
-- output.lean --
def s := ":"
`,
	},
	{
		name: "do not break := or ::",
		archive: `
-- input.lean --
def x := 1
#check List.nil :: List Nat
-- output.lean --
def x := 1
#check List.nil :: List Nat
`,
	},
	{
		name: "colon formats only when followed by identifier (and adds space before)",
		archive: `
-- input.lean --
def f(x:Nat) := (x:Nat)
-- output.lean --
def f(x : Nat) := (x : Nat)
`,
	},
	{
		name: "do not touch colon inside char literal",
		archive: `
-- input.lean --
let c := ':'
-- output.lean --
let c := ':'
`,
	},
	{
		name: "do not touch colon inside block comment",
		archive: `
-- input.lean --
def n : Nat := 1
/- type : Nat -/
-- output.lean --
def n : Nat := 1
/- type : Nat -/
`,
	},
	{
		name: "colon inside nested block comment unchanged",
		archive: `
-- input.lean --
/- outer /- inner :Nat -/ -/
-- output.lean --
/- outer /- inner :Nat -/ -/
`,
	},
	{
		name: "double space in abbrev",
		archive: `
-- input.lean --
abbrev N  : Type := Nat
-- output.lean --
abbrev N : Type := Nat
`,
	},
	{
		name: "trim trailing space",
		archive: `
-- input.lean --
def x : Nat := 1  
-- output.lean --
def x : Nat := 1
`,
	},
	{
		name: "indent normalized to 2 spaces",
		archive: `
-- input.lean --
def x : Nat :=
 if True then
   1
-- output.lean --
def x : Nat :=
  if True then
  1
`,
	},
	{
		name: "indent should work in the block",
		archive: `
-- input.lean --
def spaceBetween (before : String) (after : String) : String :=
  String.append before (String.append " " after)

#check (spaceBetween)
-- output.lean --
def spaceBetween (before : String) (after : String) : String :=
  String.append before (String.append " " after)

#check (spaceBetween)
`,
	},
	{
		name: "empty input unchanged",
		archive: `
-- input.lean --
-- output.lean --
`,
	},
	{
		name: "only newline preserved",
		archive: `
-- input.lean --

-- output.lean --

`,
	},
	{
		name: "only spaces trimmed to newline",
		archive: `
-- input.lean --
   
-- output.lean --

`,
	},
	{
		name: "only spaces (no newline) trimmed to empty",
		archive: `
-- input.lean --
   
-- output.lean --
`,
	},
	{
		name: "blank line with spaces becomes empty line",
		archive: `
-- input.lean --
a
  
b
-- output.lean --
a

b
`,
	},
	{
		name: "colon at end of input unchanged",
		archive: `
-- input.lean --
def x :
-- output.lean --
def x :
`,
	},
	{
		name: "multiple blank lines preserved (capped at 2)",
		archive: `
-- input.lean --
a



b
-- output.lean --
a


b
`,
	},
	{
		name: "collapse spaces",
		archive: `
-- input.lean --
def  foo   :=  bar
-- output.lean --
def foo := bar
`,
	},
	{
		name: "trailing whitespace",
		archive: `
-- input.lean --
def foo := bar   
theorem baz : Nat   
-- output.lean --
def foo := bar
theorem baz : Nat
`,
	},
	{
		name: "operator spacing :=",
		archive: `
-- input.lean --
def foo:=bar
def baz :=qux
def a:= b
def c := d
-- output.lean --
def foo := bar
def baz := qux
def a := b
def c := d
`,
	},
	{
		name: "operator spacing :",
		archive: `
-- input.lean --
theorem foo:Nat := sorry
-- output.lean --
theorem foo : Nat := sorry
`,
	},
	{
		name: "operator spacing =>",
		archive: `
-- input.lean --
fun x=>x + 1
fun y =>y
-- output.lean --
fun x => x + 1
fun y => y
`,
	},
	{
		name: "blank line preservation (2 lines stay)",
		archive: `
-- input.lean --
def foo := 1


def bar := 2
-- output.lean --
def foo := 1


def bar := 2
`,
	},
	{
		name: "indent after where",
		archive: `
-- input.lean --
def foo := bar where
baz := 1
-- output.lean --
def foo := bar where
  baz := 1
`,
	},
	{
		name: "indent after by",
		archive: `
-- input.lean --
theorem foo : True := by
exact trivial
-- output.lean --
theorem foo : True := by
  exact trivial
`,
	},
	{
		name: "indent after do",
		archive: `
-- input.lean --
def main : IO Unit := do
let x <- pure 1
pure x
-- output.lean --
def main : IO Unit := do
  let x <- pure 1
  pure x
`,
	},
	{
		name: "indent for inductive with deriving",
		archive: `
-- input.lean --
inductive WireType where
  | I32 : WireType
deriving Inhabited, Repr, DecidableEq
-- output.lean --
inductive WireType where
  | I32 : WireType
deriving Inhabited, Repr, DecidableEq
`,
	},
	{
		name: "dont auto indent after do keyword",
		archive: `
-- input.lean --
def foo(n : Nat) := do
  let a = 1
  let b = 2

  let c = 3
-- output.lean --
def foo(n : Nat) := do
  let a = 1
  let b = 2

  let c = 3
`,
	},
	{
		name: "keyword do should not enter in the newline",
		archive: `
-- input.lean --
def main (args : List String) : IO UInt32 := do
  match configFromArgs args with
  | some config =>
    dirTree config (← IO.currentDir)
    pure 0
  | none =>
    IO.eprintln s!"Didn't understand argument(s) {" ".separate args}\n"
    IO.eprintln usage
    pure 1
-- output.lean --
def main (args : List String) : IO UInt32 := do
  match configFromArgs args with
  | some config =>
    dirTree config (← IO.currentDir)
    pure 0
  | none =>
    IO.eprintln s!"Didn't understand argument(s) {" ".separate args}\n"
    IO.eprintln usage
    pure 1
`,
	},
	{
		name: "indent for inductive and where with trailing comment",
		archive: `
-- input.lean --
inductive Boolean where -- comment
| false : Boolean
| true : Boolean
-- output.lean --
inductive Boolean where -- comment
  | false : Boolean
  | true : Boolean
`,
	},
	{
		name: "indent after where with trailing comment",
		archive: `
-- input.lean --
def foo : Nat :=
  bar
where -- comment
  baz : Nat := 1
-- output.lean --
def foo : Nat :=
  bar
where -- comment
  baz : Nat := 1
`,
	},
	{
		name: "indent after by with trailing comment",
		archive: `
-- input.lean --
theorem foo : True := by -- proof
  exact trivial
-- output.lean --
theorem foo : True := by -- proof
  exact trivial
`,
	},
	{
		name: "indent after do with trailing comment",
		archive: `
-- input.lean --
def main : IO Unit := do -- IO
  let x <- pure 1
  pure x
-- output.lean --
def main : IO Unit := do -- IO
  let x <- pure 1
  pure x
`,
	},
	{
		name: "indent after := with trailing comment",
		archive: `
-- input.lean --
def foo := -- comment
  bar
-- output.lean --
def foo := -- comment
  bar
`,
	},
	{
		name: "indent after => with trailing comment",
		archive: `
-- input.lean --
def f := fun x => -- arrow
  x
-- output.lean --
def f := fun x => -- arrow
  x
`,
	},
	{
		name: "no indent after match with in a line with trailing comment",
		archive: `
-- input.lean --
def pred (n : Nat) : Nat :=
  match n with -- comment
  | Nat.zero => Nat.zero
  | Nat.succ k => k
-- output.lean --
def pred (n : Nat) : Nat :=
  match n with -- comment
  | Nat.zero => Nat.zero
  | Nat.succ k => k
`,
	},
	{
		name: "match arms indent when match on same line as := with trailing comment",
		archive: `
-- input.lean --
def foo := match x with -- comment
| 0 => "zero"
| _ => "other"
-- output.lean --
def foo := match x with -- comment
  | 0 => "zero"
  | _ => "other"
`,
	},
	{
		name: "no indent after match with in a line",
		archive: `
-- input.lean --
def pred (n : Nat) : Nat :=
  match n with
  | Nat.zero => Nat.zero
  | Nat.succ k => k
-- output.lean --
def pred (n : Nat) : Nat :=
  match n with
  | Nat.zero => Nat.zero
  | Nat.succ k => k
`,
	},
	{
		name: "match arms indent when match on same line as :=",
		archive: `
-- input.lean --
def foo := match x with
| 0 => "zero"
| _ => "other"
-- output.lean --
def foo := match x with
  | 0 => "zero"
  | _ => "other"
`,
	},
	{
		name: "dedent on closer structure",
		archive: `
-- input.lean --
structure Foo where
  x : Nat
  y : Nat
-- output.lean --
structure Foo where
  x : Nat
  y : Nat
`,
	},
	{
		name: "block comment preserved",
		archive: `
-- input.lean --
/- this is a comment -/
def foo := 1
-- output.lean --
/- this is a comment -/
def foo := 1
`,
	},
	{
		name: "nested block comment preserved",
		archive: `
-- input.lean --
/- outer /- inner -/ still outer -/
def foo := 1
-- output.lean --
/- outer /- inner -/ still outer -/
def foo := 1
`,
	},
	{
		name: "string with spaces preserved",
		archive: `
-- input.lean --
def foo := "  spaces  inside  "
-- output.lean --
def foo := "  spaces  inside  "
`,
	},
	{
		name: "line comment preserved",
		archive: `
-- input.lean --
def foo := 1 -- this is a comment
-- output.lean --
def foo := 1 -- this is a comment
`,
	},
	{
		name: "passthrough unknown syntax",
		archive: `
-- input.lean --
#check Nat
-- output.lean --
#check Nat
`,
	},
	{
		name: "pipe separator spacing",
		archive: `
-- input.lean --
inductive Foo where
|bar : Foo
|baz : Foo
-- output.lean --
inductive Foo where
  | bar : Foo
  | baz : Foo
`,
	},
	{
		name: "open bracket indent",
		archive: `
-- input.lean --
def foo := (
1
)
-- output.lean --
def foo := (
  1
)
`,
	},
	{
		name: "long line break at :=",
		archive: `
-- input.lean --
def veryLongVariableName : SomeLongTypeName := someExtremelyLongExpressionThatJustKeepsGoingAndGoingPastOneHundredColumns
-- output.lean --
def veryLongVariableName : SomeLongTypeName :=
  someExtremelyLongExpressionThatJustKeepsGoingAndGoingPastOneHundredColumns
`,
	},
	{
		name: "single line",
		archive: `
-- input.lean --
def foo := 1
-- output.lean --
def foo := 1
`,
	},
	{
		name: "char literal",
		archive: `
-- input.lean --
def c := 'a'
-- output.lean --
def c := 'a'
`,
	},
	{
		name: "string with newline unchanged",
		archive: `
-- input.lean --
def s := "line1
line2"
-- output.lean --
def s := "line1
line2"
`,
	},
	{
		name: "empty block comment unchanged",
		archive: `
-- input.lean --
/-**/-
-- output.lean --
/-**/-
`,
	},
	{
		name: "unchanged the indent",
		archive: `
-- input.lean --
def maximum (n : Nat) (k : Nat) : Nat :=
  if n < k then k
  else n

-- this is a curry by default
-- m3 :: Nat -> Nat
def m3 := maximum 3
-- output.lean --
def maximum (n : Nat) (k : Nat) : Nat :=
  if n < k then k
  else n

-- this is a curry by default
-- m3 :: Nat -> Nat
def m3 := maximum 3
`,
	},
	{
		name: "2 max input",
		archive: `
-- input.lean --
def maximum' (n k : Nat) : Nat :=
  if n < k then k
  else n

def maximum (n : Nat) (k : Nat) : Nat :=
  if n < k then k
  else n
-- output.lean --
def maximum' (n k : Nat) : Nat :=
  if n < k then k
  else n

def maximum (n : Nat) (k : Nat) : Nat :=
  if n < k then k
  else n
`,
	},
	{
		name: "french quoted identifier unchanged",
		archive: `
-- input.lean --
def «my weird name» := 1
-- output.lean --
def «my weird name» := 1
`,
	},
	{
		name: "hierarchical identifier with dots",
		archive: `
-- input.lean --
def foo := List.map
-- output.lean --
def foo := List.map
`,
	},
	{
		name: "nested match with keyword",
		archive: `
-- input.lean --
theorem foo (x : Option (Sum Nat Bool)) : Nat :=
  match x with
  | some y =>
    match y with
    | Sum.inl n =>
      match n with
      | 0 => 0
      | _ => 1
    | Sum.inr b =>
      match b with
      | true => 2
      | false => 3
  | none => 42
-- output.lean --
theorem foo (x : Option (Sum Nat Bool)) : Nat :=
  match x with
  | some y =>
    match y with
    | Sum.inl n =>
      match n with
      | 0 => 0
      | _ => 1
    | Sum.inr b =>
      match b with
      | true => 2
      | false => 3
  | none => 42
`,
	},
	{
		name: "unicode identifiers",
		archive: `
-- input.lean --
def α := 1
def β := α + 1
-- output.lean --
def α := 1
def β := α + 1
`,
	},
	{
		name: "doc comment preserved",
		archive: `
-- input.lean --
/-- This is a doc comment -/
def foo := 1
-- output.lean --
/-- This is a doc comment -/
def foo := 1
`,
	},
	{
		name: "interpolated string unchanged",
		archive: `
-- input.lean --
def msg := s!"Hello {name}"
-- output.lean --
def msg := s!"Hello {name}"
`,
	},
	{
		name: "raw string unchanged",
		archive: `
-- input.lean --
def s := r"raw\nstring"
-- output.lean --
def s := r"raw\nstring"
`,
	},
	{
		name: "namespace declaration",
		archive: `
-- input.lean --
namespace Foo.Bar
def x := 1
end Foo.Bar
-- output.lean --
namespace Foo.Bar
def x := 1
end Foo.Bar
`,
	},
	{
		name: "open command preserved",
		archive: `
-- input.lean --
open List in
def foo := map
-- output.lean --
open List in
def foo := map
`,
	},
	{
		name: "arrow operator",
		archive: `
-- input.lean --
def f : Nat -> Nat := fun x => x
-- output.lean --
def f : Nat -> Nat := fun x => x
`,
	},
	{
		name: "lambda with unicode arrow",
		archive: `
-- input.lean --
def f : Nat → Nat := fun x => x
-- output.lean --
def f : Nat → Nat := fun x => x
`,
	},
	{
		name: "or operator unchanged",
		archive: `
-- input.lean --
def f := a || b
-- output.lean --
def f := a || b
`,
	},
	{
		name: "pipe bind operator unchanged",
		archive: `
-- input.lean --
def f := x |> g
-- output.lean --
def f := x |> g
`,
	},
	{
		name: "backwards pipe operator unchanged",
		archive: `
-- input.lean --
def f := g <| x
-- output.lean --
def f := g <| x
`,
	},
	{
		name: "range syntax no spacing",
		archive: `
-- input.lean --
def r := [1:10]
-- output.lean --
def r := [1:10]
`,
	},
}

func readContent(a *txtar.Archive, name string) string {
	for _, f := range a.Files {
		if f.Name == name {
			return string(f.Data)
		}
	}
	return ""
}

func TestFormat(t *testing.T) {
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			a := txtar.Parse([]byte(tc.archive))
			input := readContent(a, "input.lean")
			want := readContent(a, "output.lean")

			// txtar adds trailing newlines to files. We strip them from both
			// input and ant so the test reflects pure content comparison.
			input = strings.TrimSuffix(input, "\n")
			want = strings.TrimSuffix(want, "\n")

			got := formatString(input)

			if got != want {
				t.Errorf("mismatch\n--- got ---\n%q\n--- want ---\n%q", got, want)
			}

			got2 := formatString(got)
			if got2 != got {
				t.Errorf("not idempotent\n--- second pass ---\n%q\n--- first pass ---\n%q", got2, got)
			}
		})
	}
}

func TestFormatWithCustomIndent(t *testing.T) {
	input := "def foo := bar where\nbaz := 1\n"
	opts := FormatOptions{IndentSize: 4}
	got := formatStringWithOptions(input, opts)
	want := "def foo := bar where\n    baz := 1\n"
	if got != want {
		t.Errorf("custom indent mismatch\n--- got ---\n%q\n--- want ---\n%q", got, want)
	}
}

func TestIdempotentProperty(t *testing.T) {
	inputs := []string{
		"def x := 1",
		"theorem foo : Nat := sorry",
		"| a : T\n| b : T",
		"def f(x:Nat) :Nat := x",
		"",
		"\n",
		"   ",
	}
	for _, input := range inputs {
		got := formatString(input)
		got2 := formatString(got)
		if got2 != got {
			t.Errorf("not idempotent for input %q\nfirst: %q\nsecond: %q", input, got, got2)
		}
	}
}

func TestPreservesNonWhitespace(t *testing.T) {
	inputs := []string{
		"def x := 1",
		"theorem foo : Nat := sorry",
		"| a : T\n| b : T",
		"def f(x:Nat) :Nat := x",
	}
	for _, input := range inputs {
		got := formatString(input)
		inputNonWS := filterNonWS(input)
		gotNonWS := filterNonWS(got)
		if inputNonWS != gotNonWS {
			t.Errorf("non-whitespace changed for input %q\ninput: %q\ngot: %q", input, inputNonWS, gotNonWS)
		}
	}
}

func filterNonWS(s string) string {
	var sb strings.Builder
	for _, r := range s {
		if !unicode.IsSpace(r) {
			sb.WriteRune(r)
		}
	}
	return sb.String()
}

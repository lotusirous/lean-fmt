package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"
	"unicode"
	"unicode/utf8"
)

const DefaultIndentSize = 2

// ---------------------------------------------------------------------------
// Token types
// ---------------------------------------------------------------------------

type TokenKind int

const (
	tokEOF TokenKind = iota
	tokWhitespace
	tokNewline
	tokLineComment
	tokBlockComment
	tokString
	tokChar
	tokKeyword
	tokOperator
	tokOpen
	tokClose
	tokIdent
)

type Token struct {
	Kind TokenKind
	Text string
}

var keywords = map[string]bool{
	"theorem": true, "def": true, "lemma": true, "instance": true,
	"structure": true, "inductive": true, "by": true, "do": true,
	"match": true, "where": true, "let": true, "have": true,
	"with": true,
}

var indentKeywords = map[string]bool{
	"by": true, "do": true, "match": true, "where": true, "with": true,
}

var toplevelKeywords = map[string]bool{
	"def": true, "theorem": true, "lemma": true, "instance": true,
	"structure": true, "inductive": true, "abbrev": true, "class": true,
	"example": true, "axiom": true, "constant": true, "opaque": true,
}

// ---------------------------------------------------------------------------
// Scanner — state-function based lexer (Rob Pike pattern)
// ---------------------------------------------------------------------------

type Scanner struct {
	input  string
	start  int
	pos    int
	tokens []Token
}

// stateFn represents the state of the scanner as a function that returns the next state.
type stateFn func(*Scanner) stateFn

func NewScanner(input string) *Scanner {
	return &Scanner{input: input}
}

func (s *Scanner) Scan() []Token {
	for state := scanMain; state != nil; {
		state = state(s)
	}
	return s.tokens
}

func (s *Scanner) emit(kind TokenKind) {
	s.tokens = append(s.tokens, Token{kind, s.input[s.start:s.pos]})
	s.start = s.pos
}

func (s *Scanner) next() rune {
	if s.pos >= len(s.input) {
		return 0
	}
	r, w := utf8.DecodeRuneInString(s.input[s.pos:])
	s.pos += w
	return r
}

func (s *Scanner) backup() {
	if s.pos > 0 {
		_, w := utf8.DecodeLastRuneInString(s.input[:s.pos])
		s.pos -= w
	}
}

func (s *Scanner) peek() rune {
	r := s.next()
	if r != 0 {
		s.backup()
	}
	return r
}

func (s *Scanner) peekAt(offset int) rune {
	pos := s.pos
	for i := 0; i < offset; i++ {
		if pos >= len(s.input) {
			return 0
		}
		_, w := utf8.DecodeRuneInString(s.input[pos:])
		pos += w
	}
	if pos >= len(s.input) {
		return 0
	}
	r, _ := utf8.DecodeRuneInString(s.input[pos:])
	return r
}

func (s *Scanner) accept(valid string) bool {
	if strings.ContainsRune(valid, s.peek()) {
		s.next()
		return true
	}
	return false
}

func (s *Scanner) acceptRun(valid string) {
	for strings.ContainsRune(valid, s.peek()) {
		s.next()
	}
}

// scanMain is the initial and main state.
func scanMain(s *Scanner) stateFn {
	s.start = s.pos
	r := s.next()
	if r == 0 {
		return nil
	}

	switch {
	case r == '\n':
		s.emit(tokNewline)
		return scanMain

	case r == ' ' || r == '\t':
		return scanWhitespace

	case r == '-' && s.peek() == '-':
		return scanLineComment

	case r == '/' && s.peek() == '-':
		return scanBlockComment

	case r == '"':
		return scanString

	case r == '\'':
		return scanChar

	case r == ':':
		return scanColon

	case r == '=' && s.peek() == '>':
		s.next()
		s.emit(tokOperator)
		return scanMain

	case r == '|':
		// Check for || (or) or |> (pipe) - should not add spacing around each |
		p := s.peek()
		if p == '|' || p == '>' {
			s.next()
			s.emit(tokIdent) // || and |> are not formatting operators
		} else {
			s.emit(tokOperator)
		}
		return scanMain

	case r == '<' && s.peek() == '|':
		// <| (backwards pipe) - should not add spacing
		s.next()
		s.emit(tokIdent)
		return scanMain

	case r == '(' || r == '[' || r == '{':
		s.emit(tokOpen)
		return scanMain

	case r == ')' || r == ']' || r == '}':
		s.emit(tokClose)
		return scanMain

	case unicode.IsLetter(r) || r == '_':
		return scanIdent

	case unicode.IsDigit(r):
		return scanNumber

	default:
		s.emit(tokIdent)
		return scanMain
	}
}

func scanWhitespace(s *Scanner) stateFn {
	for {
		r := s.peek()
		if r == ' ' || r == '\t' {
			s.next()
		} else {
			break
		}
	}
	s.emit(tokWhitespace)
	return scanMain
}

func scanLineComment(s *Scanner) stateFn {
	s.next() // consume second '-'
	for {
		r := s.peek()
		if r == 0 || r == '\n' {
			break
		}
		s.next()
	}
	s.emit(tokLineComment)
	return scanMain
}

func scanBlockComment(s *Scanner) stateFn {
	s.next() // consume '-' after '/'
	depth := 1
	for depth > 0 {
		r := s.next()
		if r == 0 {
			break
		}
		if r == '/' && s.peek() == '-' {
			s.next()
			depth++
		} else if r == '-' && s.peek() == '/' {
			s.next()
			depth--
		}
	}
	s.emit(tokBlockComment)
	return scanMain
}

func scanString(s *Scanner) stateFn {
	for {
		r := s.next()
		if r == 0 || r == '"' {
			break
		}
		if r == '\\' {
			s.next() // skip escaped char
		}
	}
	s.emit(tokString)
	return scanMain
}

func scanChar(s *Scanner) stateFn {
	for {
		r := s.next()
		if r == 0 || r == '\'' {
			break
		}
		if r == '\\' {
			s.next()
		}
	}
	s.emit(tokChar)
	return scanMain
}

func scanColon(s *Scanner) stateFn {
	r := s.peek()
	switch r {
	case '=':
		s.next()
		s.emit(tokOperator)
	case ':':
		s.next()
		s.emit(tokIdent) // :: is not an operator we format
	case '>':
		s.next()
		s.emit(tokIdent) // :> coercion
	case '<':
		s.next()
		s.emit(tokIdent) // :<
	default:
		s.emit(tokOperator) // single :
	}
	return scanMain
}

func scanIdent(s *Scanner) stateFn {
	for {
		r := s.peek()
		// Lean identifiers can contain letters, digits, underscores, and trailing primes (')
		if unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_' || r == '\'' {
			s.next()
		} else {
			break
		}
	}
	text := s.input[s.start:s.pos]
	if keywords[text] {
		s.emit(tokKeyword)
	} else {
		s.emit(tokIdent)
	}
	return scanMain
}

func scanNumber(s *Scanner) stateFn {
	for {
		r := s.peek()
		if unicode.IsDigit(r) || unicode.IsLetter(r) || r == '_' || r == '.' {
			s.next()
		} else {
			break
		}
	}
	s.emit(tokIdent)
	return scanMain
}

// ---------------------------------------------------------------------------
// Formatter state
// ---------------------------------------------------------------------------

type FormatOptions struct {
	IndentSize int
}

type Formatter struct {
	opts         FormatOptions
	indent       int
	pendingIndent int
	blankCount   int
	out          strings.Builder
	lineCount    int
}

func NewFormatter(opts FormatOptions) *Formatter {
	if opts.IndentSize <= 0 {
		opts.IndentSize = DefaultIndentSize
	}
	return &Formatter{opts: opts}
}

// FormatLine processes a single line of tokens and writes to the output.
func (f *Formatter) FormatLine(tokens []Token, isLast bool) {
	// Check if line is blank
	if isBlankTokens(tokens) {
		f.blankCount++
		if f.blankCount <= 2 {
			if f.lineCount > 0 {
				f.out.WriteByte('\n')
			}
			f.lineCount++
		}
		// Reset indent after blank line (heuristic: blank lines separate top-level items)
		f.indent = 0
		f.pendingIndent = 0
		return
	}
	f.blankCount = 0

	// Normalize whitespace and operator spacing
	tokens = f.normalizeTokens(tokens)

	// Strip leading whitespace (we'll rebuild it)
	if len(tokens) > 0 && tokens[0].Kind == tokWhitespace {
		tokens = tokens[1:]
	}
	if len(tokens) == 0 {
		if f.lineCount > 0 {
			f.out.WriteByte('\n')
		}
		f.lineCount++
		return
	}

	// Apply pending indent from previous line
	f.indent += f.pendingIndent
	f.pendingIndent = 0

	// Reset indent if line starts with a top-level declaration keyword
	first := firstNonWSToken(tokens)
	if first.Kind == tokKeyword && toplevelKeywords[first.Text] {
		f.indent = 0
	}

	// Dedent if line starts with closer
	if tokens[0].Kind == tokClose {
		f.indent -= f.opts.IndentSize
		if f.indent < 0 {
			f.indent = 0
		}
	}

	// Write newline before this line (if not first)
	if f.lineCount > 0 {
		f.out.WriteByte('\n')
	}
	f.lineCount++

	// Write indentation
	if f.indent > 0 {
		f.out.WriteString(strings.Repeat(" ", f.indent))
	}

	// Write tokens
	for _, t := range tokens {
		f.out.WriteString(t.Text)
	}

	// Check for indent triggers at end of line
	last := lastNonWSToken(tokens)
	if last.Kind == tokKeyword && indentKeywords[last.Text] {
		f.pendingIndent = f.opts.IndentSize
	}
	if last.Kind == tokOpen {
		f.pendingIndent = f.opts.IndentSize
	}
	if last.Kind == tokOperator && (last.Text == ":=" || last.Text == "=>") {
		f.pendingIndent = f.opts.IndentSize
	}
}

func (f *Formatter) normalizeTokens(tokens []Token) []Token {
	if len(tokens) == 0 {
		return tokens
	}

	// Keep leading whitespace for now (will be stripped later)
	var leading Token
	toks := tokens
	if toks[0].Kind == tokWhitespace {
		leading = toks[0]
		toks = toks[1:]
	}

	var normalized []Token
	if leading.Text != "" {
		normalized = append(normalized, leading)
	}

	for i := 0; i < len(toks); i++ {
		t := toks[i]
		switch t.Kind {
		case tokWhitespace:
			// Collapse to single space, skip if trailing
			if i+1 < len(toks) {
				normalized = append(normalized, Token{tokWhitespace, " "})
			}

		case tokOperator:
			// Special handling for single colon - only add spacing if it looks like type annotation
			// Skip spacing for range syntax like [1:10]
			if t.Text == ":" {
				// Check if previous token is a bracket/number (range syntax)
				prevIsRange := len(normalized) > 0 && isRangeContext(normalized[len(normalized)-1])
				nextIsRange := i+1 < len(toks) && isRangeContext(toks[i+1])
				if prevIsRange || nextIsRange {
					// Don't add spacing - this is likely range syntax
					normalized = append(normalized, t)
					continue
				}
			}

			// Ensure space before operator
			if len(normalized) > 0 {
				last := normalized[len(normalized)-1]
				if last.Kind != tokWhitespace && last.Text != "" {
					normalized = append(normalized, Token{tokWhitespace, " "})
				}
			}
			normalized = append(normalized, t)
			// Ensure space after operator
			if i+1 < len(toks) && toks[i+1].Kind != tokWhitespace {
				normalized = append(normalized, Token{tokWhitespace, " "})
			}

		default:
			normalized = append(normalized, t)
		}
	}

	// Strip trailing whitespace
	for len(normalized) > 0 && normalized[len(normalized)-1].Kind == tokWhitespace {
		normalized = normalized[:len(normalized)-1]
	}

	return normalized
}

func (f *Formatter) String() string {
	return f.out.String()
}

func (f *Formatter) BreakLongLines() string {
	result := f.out.String()
	rawLines := strings.Split(result, "\n")
	var out []string

	for _, rl := range rawLines {
		if len(rl) <= 100 {
			out = append(out, rl)
			continue
		}

		trimmed := strings.TrimLeft(rl, " ")
		currentIndent := len(rl) - len(trimmed)
		newIndent := strings.Repeat(" ", currentIndent+f.opts.IndentSize)

		opEnd := -1
		restStart := -1
		scanner := NewScanner(rl)
		toks := scanner.Scan()
		pos := 0
		for ti, t := range toks {
			if t.Kind == tokOperator && (t.Text == ":=" || t.Text == "=>" || t.Text == "|") {
				afterOp := pos + len(t.Text)
				afterSpace := afterOp
				if ti+1 < len(toks) && toks[ti+1].Kind == tokWhitespace {
					afterSpace += len(toks[ti+1].Text)
				}
				if afterSpace < len(rl) {
					opEnd = afterOp
					restStart = afterSpace
				}
			}
			pos += len(t.Text)
		}

		if opEnd > 0 {
			out = append(out, strings.TrimRight(rl[:opEnd], " "))
			out = append(out, newIndent+strings.TrimLeft(rl[restStart:], " "))
		} else {
			out = append(out, rl)
		}
	}

	return strings.Join(out, "\n")
}

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

// isRangeContext returns true if the token suggests range syntax context
// (e.g., [1:10] where colon should not be spaced)
func isRangeContext(t Token) bool {
	// Numbers or brackets suggest range context
	if t.Kind == tokOpen || t.Kind == tokClose {
		return true
	}
	// Check if token looks like a number
	if t.Kind == tokIdent && len(t.Text) > 0 {
		first := rune(t.Text[0])
		if first >= '0' && first <= '9' {
			return true
		}
	}
	return false
}

func isBlankTokens(tokens []Token) bool {
	for _, t := range tokens {
		if t.Kind != tokWhitespace {
			return false
		}
	}
	return true
}

func lastNonWSToken(tokens []Token) Token {
	for i := len(tokens) - 1; i >= 0; i-- {
		if tokens[i].Kind != tokWhitespace {
			return tokens[i]
		}
	}
	return Token{}
}

func firstNonWSToken(tokens []Token) Token {
	for _, t := range tokens {
		if t.Kind != tokWhitespace {
			return t
		}
	}
	return Token{}
}

// ---------------------------------------------------------------------------
// Streaming formatter - processes input line by line
// ---------------------------------------------------------------------------

// StreamFormatter processes input line-by-line without loading entire file.
type StreamFormatter struct {
	opts      FormatOptions
	formatter *Formatter
	scanner   *bufio.Scanner
	writer    *bufio.Writer
}

func NewStreamFormatter(r io.Reader, w io.Writer, opts FormatOptions) *StreamFormatter {
	if opts.IndentSize <= 0 {
		opts.IndentSize = DefaultIndentSize
	}
	return &StreamFormatter{
		opts:      opts,
		formatter: NewFormatter(opts),
		scanner:   bufio.NewScanner(r),
		writer:    bufio.NewWriter(w),
	}
}

func (sf *StreamFormatter) Format() error {
	for sf.scanner.Scan() {
		line := sf.scanner.Text()
		lexer := NewScanner(line)
		tokens := lexer.Scan()
		sf.formatter.FormatLine(tokens, false)
	}

	if err := sf.scanner.Err(); err != nil {
		return err
	}

	// Apply long-line breaking and write output
	result := sf.formatter.BreakLongLines()
	if _, err := sf.writer.WriteString(result); err != nil {
		return err
	}

	return sf.writer.Flush()
}

// formatReader streams from reader to writer.
func formatReader(r io.Reader, w io.Writer, opts FormatOptions) error {
	sf := NewStreamFormatter(r, w, opts)
	return sf.Format()
}

// formatString is the in-memory version for testing.
func formatString(input string) string {
	return formatStringWithOptions(input, FormatOptions{IndentSize: DefaultIndentSize})
}

func formatStringWithOptions(input string, opts FormatOptions) string {
	defer func() { recover() }()

	if opts.IndentSize <= 0 {
		opts.IndentSize = DefaultIndentSize
	}

	// Split input into lines (preserving the line structure)
	lines := strings.Split(input, "\n")
	
	// Track if input ends with newline
	inputEndsWithNewline := strings.HasSuffix(input, "\n")
	
	// Remove the empty string that Split creates for trailing newline
	if inputEndsWithNewline && len(lines) > 0 && lines[len(lines)-1] == "" {
		lines = lines[:len(lines)-1]
	}

	formatter := NewFormatter(opts)

	for i, line := range lines {
		lexer := NewScanner(line)
		tokens := lexer.Scan()
		formatter.FormatLine(tokens, i == len(lines)-1)
	}

	result := formatter.BreakLongLines()

	// Handle trailing newline
	resultEndsWithNewline := strings.HasSuffix(result, "\n")
	if inputEndsWithNewline && !resultEndsWithNewline && result != "" {
		result += "\n"
	} else if !inputEndsWithNewline && resultEndsWithNewline {
		result = strings.TrimRight(result, "\n")
	}

	return result
}

// ---------------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------------

func main() {
	var r io.Reader

	if len(os.Args) < 2 {
		r = os.Stdin
	} else {
		f, err := os.Open(os.Args[1])
		if err != nil {
			fmt.Fprint(os.Stderr, err)
			os.Exit(1)
		}
		defer f.Close()
		r = f
	}

	// Stream directly to stdout
	if err := formatReader(r, os.Stdout, FormatOptions{IndentSize: DefaultIndentSize}); err != nil {
		fmt.Fprint(os.Stderr, err)
		os.Exit(1)
	}
}

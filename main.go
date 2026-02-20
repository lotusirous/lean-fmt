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

// keywordFlags: single table for lexer (presence = keyword) and formatter (bits below).
const (
	flagIndentAfter = 1 << iota // line ending with this adds pending indent
	flagToplevel                // line starting with this resets indent to 0
)
const flagNone = 0 // keyword for lexer only, no formatter behavior

var keywordFlags = map[string]uint{
	"theorem":   flagToplevel,
	"def":       flagToplevel,
	"lemma":     flagToplevel,
	"instance":  flagToplevel,
	"structure": flagToplevel,
	"inductive": flagToplevel,
	"deriving":  flagToplevel,
	"abbrev":    flagToplevel,
	"class":     flagToplevel,
	"example":   flagToplevel,
	"axiom":     flagToplevel,
	"constant":  flagToplevel,
	"opaque":    flagToplevel,
	"namespace": flagToplevel,
	"section":   flagToplevel,
	"end":       flagToplevel,
	"where":     flagToplevel | flagIndentAfter,
	"by":        flagIndentAfter,
	"do":        flagIndentAfter,
	"match":     flagIndentAfter,
	"with":      flagIndentAfter,
	"let":       flagNone,
	"have":      flagNone,
	"cases":     flagIndentAfter,
}

type stateFn func(*Scanner) stateFn

type Scanner struct {
	input  string
	start  int
	pos    int
	tokens []Token
}

func NewScanner(input string) *Scanner {
	return &Scanner{input: input}
}

func (s *Scanner) Scan() []Token {
	for state := parse; state != nil; {
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

func parse(s *Scanner) stateFn {
	s.start = s.pos
	r := s.next()
	if r == 0 {
		return nil
	}

	switch {
	case r == '\n':
		s.emit(tokNewline)
		return parse

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
		return parse

	case r == '|':
		p := s.peek()
		if p == '|' || p == '>' {
			s.next()
			s.emit(tokIdent)
		} else {
			s.emit(tokOperator)
		}
		return parse

	case r == '<' && s.peek() == '|':
		s.next()
		s.emit(tokIdent)
		return parse

	case r == '(' || r == '[' || r == '{':
		s.emit(tokOpen)
		return parse

	case r == ')' || r == ']' || r == '}':
		s.emit(tokClose)
		return parse

	case unicode.IsLetter(r) || r == '_':
		return scanIdent

	case unicode.IsDigit(r):
		return scanNumber

	default:
		s.emit(tokIdent)
		return parse
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
	return parse
}

func scanLineComment(s *Scanner) stateFn {
	s.next()
	for {
		r := s.peek()
		if r == 0 || r == '\n' {
			break
		}
		s.next()
	}
	s.emit(tokLineComment)
	return parse
}

func scanBlockComment(s *Scanner) stateFn {
	s.next()
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
	return parse
}

func scanString(s *Scanner) stateFn {
	for {
		r := s.next()
		if r == 0 || r == '"' {
			break
		}
		if r == '\\' {
			s.next()
		}
	}
	s.emit(tokString)
	return parse
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
	return parse
}

func scanColon(s *Scanner) stateFn {
	r := s.peek()
	switch r {
	case '=':
		s.next()
		s.emit(tokOperator)
	case ':':
		s.next()
		s.emit(tokIdent)
	case '>':
		s.next()
		s.emit(tokIdent)
	case '<':
		s.next()
		s.emit(tokIdent)
	default:
		s.emit(tokOperator)
	}
	return parse
}

func scanIdent(s *Scanner) stateFn {
	for {
		r := s.peek()
		if unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_' || r == '\'' {
			s.next()
		} else {
			break
		}
	}
	text := s.input[s.start:s.pos]
	if _, ok := keywordFlags[text]; ok {
		s.emit(tokKeyword)
	} else {
		s.emit(tokIdent)
	}
	return parse
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
	return parse
}

type FormatOptions struct {
	IndentSize int
}

type Formatter struct {
	opts             FormatOptions
	indent           int
	pendingIndent    int
	matchArmStack    []int // stack of arm indents for nested match (per do-notation spec)
	blankCount       int
	prevLineWasBlank bool
	out              strings.Builder
	lineCount        int
}

func NewFormatter(opts FormatOptions) *Formatter {
	if opts.IndentSize <= 0 {
		opts.IndentSize = DefaultIndentSize
	}
	return &Formatter{opts: opts}
}

func matchWithLine(first, last Token) bool {
	return first.Kind == tokKeyword && first.Text == "match" &&
		last.Kind == tokKeyword && last.Text == "with"
}

func casesWithLine(first, last Token) bool {
	return first.Kind == tokKeyword && first.Text == "cases" &&
		last.Kind == tokKeyword && last.Text == "with"
}

type lineInfo struct {
	first, last Token
	inputIndent int
	startsClose bool
	commentOnly bool
	afterBlank  bool
	tokens      []Token
}

func (f *Formatter) FormatLine(tokens []Token) {
	if isBlankTokens(tokens) {
		f.emitBlank()
		return
	}
	f.blankCount = 0
	tokens = f.normalizeTokens(tokens)
	info := f.parseLineInfo(tokens)
	if info == nil {
		return
	}

	f.indent += f.pendingIndent
	f.pendingIndent = 0
	f.prevLineWasBlank = false

	f.applyIndent(info)
	f.emitLine(info)
	f.setNext(info)
}

func (f *Formatter) emitBlank() {
	f.blankCount++
	if f.blankCount <= 2 && f.lineCount > 0 {
		f.out.WriteByte('\n')
	}
	if f.blankCount <= 2 {
		f.lineCount++
	}
	f.prevLineWasBlank = true
}

func (f *Formatter) parseLineInfo(tokens []Token) *lineInfo {
	inputIndent := -1
	if len(tokens) > 0 && tokens[0].Kind == tokWhitespace {
		inputIndent = len(tokens[0].Text)
		tokens = tokens[1:]
	}
	if len(tokens) == 0 {
		if f.lineCount > 0 {
			f.out.WriteByte('\n')
		}
		f.lineCount++
		return nil
	}
	return &lineInfo{
		first:       firstNonWSToken(tokens),
		last:        lastCodeToken(tokens),
		inputIndent: inputIndent,
		startsClose: tokens[0].Kind == tokClose,
		commentOnly: isCommentOnlyTokens(tokens),
		afterBlank:  f.prevLineWasBlank,
		tokens:      tokens,
	}
}

func (f *Formatter) applyIndent(info *lineInfo) {
	if info.afterBlank && info.commentOnly {
		f.indent = 0
	}
	if info.first.Kind == tokKeyword && (keywordFlags[info.first.Text]&flagToplevel) != 0 {
		f.indent = 0
	}
	if info.first.Kind == tokIdent && len(info.first.Text) > 0 && info.first.Text[0] == '#' {
		f.indent = 0
	}
	if info.first.Kind == tokOperator && info.first.Text == ":=" {
		f.indent = 0
	}
	if info.startsClose {
		f.indent -= f.opts.IndentSize
		if f.indent < 0 {
			f.indent = 0
		}
	}
	if info.first.Kind == tokOperator && info.first.Text == "|" {
		for len(f.matchArmStack) > 0 && info.inputIndent >= 0 && f.matchArmStack[len(f.matchArmStack)-1] > info.inputIndent {
			f.matchArmStack = f.matchArmStack[:len(f.matchArmStack)-1]
		}
		if len(f.matchArmStack) > 0 {
			f.indent = f.matchArmStack[len(f.matchArmStack)-1]
		} else if f.indent > 2*f.opts.IndentSize {
			f.indent -= f.opts.IndentSize
		}
	} else {
		for len(f.matchArmStack) > 0 && f.matchArmStack[len(f.matchArmStack)-1] > f.indent {
			f.matchArmStack = f.matchArmStack[:len(f.matchArmStack)-1]
		}
	}
}

func (f *Formatter) emitLine(info *lineInfo) {
	if f.lineCount > 0 {
		f.out.WriteByte('\n')
	}
	f.lineCount++
	outIndent := f.indent
	if info.inputIndent >= 2*f.opts.IndentSize {
		outIndent = info.inputIndent
	}
	if outIndent > 0 {
		f.out.WriteString(strings.Repeat(" ", outIndent))
	}
	for _, t := range info.tokens {
		f.out.WriteString(t.Text)
	}
}

func (f *Formatter) setNext(info *lineInfo) {
	if matchWithLine(info.first, info.last) || casesWithLine(info.first, info.last) {
		f.matchArmStack = append(f.matchArmStack, f.indent)
	}
	if info.last.Kind == tokKeyword && (keywordFlags[info.last.Text]&flagIndentAfter) != 0 {
		if matchWithLine(info.first, info.last) || casesWithLine(info.first, info.last) {
			// arms at same level as match
		} else if info.last.Text == "do" && info.first.Kind == tokKeyword && info.first.Text == "do" {
			// "do" on its own line: do items at same indent (per spec)
		} else {
			f.pendingIndent = f.opts.IndentSize
		}
	}
	if info.last.Kind == tokOpen {
		f.pendingIndent = f.opts.IndentSize
	}
	if info.last.Kind == tokOperator && (info.last.Text == ":=" || info.last.Text == "=>" || info.last.Text == ":") {
		f.pendingIndent = f.opts.IndentSize
	}
}

func (f *Formatter) normalizeTokens(tokens []Token) []Token {
	if len(tokens) == 0 {
		return tokens
	}

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
			if i+1 < len(toks) {
				normalized = append(normalized, Token{tokWhitespace, " "})
			}

		case tokOperator:
			if t.Text == ":" {
				prevIsRange := len(normalized) > 0 && isRangeContext(normalized[len(normalized)-1])
				nextIsRange := i+1 < len(toks) && isRangeContext(toks[i+1])
				if prevIsRange || nextIsRange {
					normalized = append(normalized, t)
					continue
				}
			}

			if len(normalized) > 0 {
				last := normalized[len(normalized)-1]
				if last.Kind != tokWhitespace && last.Text != "" {
					normalized = append(normalized, Token{tokWhitespace, " "})
				}
			}
			normalized = append(normalized, t)
			if i+1 < len(toks) && toks[i+1].Kind != tokWhitespace {
				normalized = append(normalized, Token{tokWhitespace, " "})
			}

		default:
			normalized = append(normalized, t)
		}
	}

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

	for _, line := range rawLines {
		if len(line) <= 100 {
			out = append(out, line)
			continue
		}

		trimmed := strings.TrimLeft(line, " ")
		currentIndent := len(line) - len(trimmed)
		newIndent := strings.Repeat(" ", currentIndent+f.opts.IndentSize)

		opEnd := -1
		restStart := -1
		scanner := NewScanner(line)
		toks := scanner.Scan()
		pos := 0
		for ti, t := range toks {
			if t.Kind == tokOperator && (t.Text == ":=" || t.Text == "=>" || t.Text == "|") {
				afterOp := pos + len(t.Text)
				afterSpace := afterOp
				if ti+1 < len(toks) && toks[ti+1].Kind == tokWhitespace {
					afterSpace += len(toks[ti+1].Text)
				}
				if afterSpace < len(line) {
					opEnd = afterOp
					restStart = afterSpace
				}
			}
			pos += len(t.Text)
		}

		if opEnd > 0 {
			out = append(out, strings.TrimRight(line[:opEnd], " "))
			out = append(out, newIndent+strings.TrimLeft(line[restStart:], " "))
		} else {
			out = append(out, line)
		}
	}

	return strings.Join(out, "\n")
}

func isRangeContext(t Token) bool {
	if t.Kind == tokOpen || t.Kind == tokClose {
		return true
	}
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

func isCommentOnlyTokens(tokens []Token) bool {
	for _, t := range tokens {
		switch t.Kind {
		case tokWhitespace, tokLineComment, tokBlockComment:
			// ok
		default:
			return false
		}
	}
	return true
}

// lastCodeToken returns the last token that is not whitespace and not a comment.
// Used for indent triggers so that "where -- comment" still triggers indent.
func lastCodeToken(tokens []Token) Token {
	for i := len(tokens) - 1; i >= 0; i-- {
		t := tokens[i]
		if t.Kind != tokWhitespace && t.Kind != tokLineComment && t.Kind != tokBlockComment {
			return t
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

// StreamFormatter formats a stream of lines from a reader to a writer.
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
		sc := NewScanner(line)
		tokens := sc.Scan()
		sf.formatter.FormatLine(tokens)
	}

	if err := sf.scanner.Err(); err != nil {
		return err
	}

	result := sf.formatter.BreakLongLines()
	if _, err := sf.writer.WriteString(result); err != nil {
		return err
	}

	return sf.writer.Flush()
}

func formatReader(r io.Reader, w io.Writer, opts FormatOptions) error {
	sf := NewStreamFormatter(r, w, opts)
	return sf.Format()
}

func formatString(input string) string {
	return formatStringWithOptions(input, FormatOptions{IndentSize: DefaultIndentSize})
}

func formatStringWithOptions(input string, opts FormatOptions) string {
	if opts.IndentSize <= 0 {
		opts.IndentSize = DefaultIndentSize
	}

	lines := strings.Split(input, "\n")
	inputEndsWithNewline := strings.HasSuffix(input, "\n")
	if inputEndsWithNewline && len(lines) > 0 && lines[len(lines)-1] == "" {
		lines = lines[:len(lines)-1]
	}

	formatter := NewFormatter(opts)

	for _, line := range lines {
		lexer := NewScanner(line)
		tokens := lexer.Scan()
		formatter.FormatLine(tokens)
	}

	result := formatter.BreakLongLines()

	resultEndsWithNewline := strings.HasSuffix(result, "\n")
	if inputEndsWithNewline && !resultEndsWithNewline && result != "" {
		result += "\n"
	} else if !inputEndsWithNewline && resultEndsWithNewline {
		result = strings.TrimRight(result, "\n")
	}

	return result
}

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

	if err := formatReader(r, os.Stdout, FormatOptions{IndentSize: DefaultIndentSize}); err != nil {
		fmt.Fprint(os.Stderr, err)
		os.Exit(1)
	}
}

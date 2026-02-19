# lean-fmt

A fast, lightweight code formatter for [Lean 4](https://lean-lang.org/) source files.

- **Fast**: Single static binary, streaming I/O
- **Safe**: Only normalizes whitespace; never reorders or deletes code
- **Simple**: Plain text processing, no AST parsing

## Simple Formatting Rules

- 2-space indentation, auto-indent after `by`, `do`, `match`, `where`, `with`, `:=`, `=>`
- Spaces around operators (`:`, `:=`, `=>`, `|`)
- Collapses multiple blank lines (max 2), strips trailing whitespace
- Breaks lines >100 chars at operators

## Installation

Download from [GitHub Releases](https://github.com/lotusirous/lean-fmt/releases), or build from source:

```bash
go install github.com/lotusirous/lean-fmt@latest
```

## Usage

```bash
lean-fmt file.lean          # format file to stdout
cat file.lean | lean-fmt    # read from stdin
```

## Editor Integration

### Neovim (conform.nvim)

```lua
require("conform").setup({
  formatters_by_ft = { lean = { "lean_fmt" } },
  formatters = {
    lean_fmt = { command = "lean-fmt", stdin = true },
  },
})
```

## License

MIT

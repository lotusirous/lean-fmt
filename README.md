## Lean Format

`lean-fmt` is a small command-line formatter for Lean 4 source files.

- Fast & simple: works on plain text instead of the full Lean AST.
- Safe: only inserts or normalizes whitespace; it never reorders or deletes code.
- Composable: formatting behavior is driven by a small set of rewrite rules.

### Build from sources


Ensure that you're using latest Haskell version of GCH 9.14. From the project root:

```bash
just compile   # builds and copies binary to bin/lean-fmt
```

You can also use the cabal commands directly:

```bash
cabal build exe:lean-fmt
cabal list-bin exe:lean-fmt   # show compiled binary path
```

If you use GitHub Releases, prebuilt binaries for Linux and macOS are attached to each `vX.Y.Z` tag.

### Usage

Format files passed as arguments:

```bash
bin/lean-fmt path/to/file.lean
```

Or read from stdin and write to stdout:

```bash
cat file.lean | bin/lean-fmt
```

### Neovim (`conform.nvim`)

Add `lean-fmt` as a formatter for Lean files in your Neovim config:

```lua
require("conform").setup({
  formatters_by_ft = {
    lean = { "lean_fmt" },
  },
  format_on_save = {
    lsp_fallback = true,
  },
})

require("conform").formatters.lean_fmt = {
  command = "lean-fmt",   -- or absolute path if not on $PATH
  stdin = true,
}
```
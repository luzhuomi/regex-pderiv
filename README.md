# regex-pderiv

PCRE-style regular expression matching using **Antimirov's partial derivatives** in Haskell.

## Overview

`regex-pderiv` is a Haskell library that implements regular expression pattern matching based on the theory of partial derivatives of regular expressions. Unlike classical approaches (Thompson NFA, Glushkov NFA), partial derivatives compute a compact set of derivative expressions for each input character, enabling efficient on-the-fly automaton construction.

The library also includes a **bitcoded transducer** for encoding parse trees as bit sequences, enabling efficient submatch extraction.

### Key Features

- **Multiple matching strategies**: Left-to-right, right-to-left, two-pass, and POSIX semantics
- **Greedy and non-greedy matching**: Full support for `*?`, `+?`, `??` quantifiers
- **Submatch extraction**: Capture groups with binder-based environment extraction
- **ByteString and String support**: Optimized `ByteString` backends and `String` interface
- **`regex-base` compatible**: Implements `RegexLike` and `RegexOptions` type classes
- **Bitcoded parse trees**: Efficient parse tree representation via bit encoding

## Installation

### Using Cabal

```bash
cabal update
cabal install regex-pderiv
```

### Using Stack

```bash
stack build
```

### From source

```bash
git clone https://github.com/luzhuomi/regex-pderiv.git
cd regex-pderiv
cabal build
```

## Quick Start

### ByteString matching (default)

```haskell
import qualified Data.ByteString.Char8 as S
import Text.Regex.PDeriv.ByteString

main :: IO ()
main = do
  let Right regex = compile defaultCompOpt defaultExecOpt (S.pack "(a|b)*c")
  let input = S.pack "ababc"
  case regexec regex input of
    Right (Just (pre, match, post, groups)) ->
      putStrLn $ "Match: " ++ S.unpack match
      -- Output: Match: ababc
    Right Nothing ->
      putStrLn "No match"
    Left err ->
      putStrLn $ "Error: " ++ err
```

### String matching

```haskell
import Text.Regex.PDeriv.String

main :: IO ()
main = do
  let Right regex = compile defaultCompOpt defaultExecOpt "(foo|bar)+baz"
  case regexec regex "foobarbaz" of
    Right (Just (pre, match, post, groups)) ->
      putStrLn $ "Match: " ++ match
      -- Output: Match: foobarbaz
    Right Nothing ->
      putStrLn "No match"
    Left err ->
      putStrLn $ "Error: " ++ err
```

### Choosing a matching strategy

```haskell
-- Default (left-to-right with DFA):
import Text.Regex.PDeriv.ByteString             -- re-exports LeftToRightD

-- Left-to-right NFA:
import Text.Regex.PDeriv.ByteString.LeftToRight

-- Right-to-left (single pass):
import Text.Regex.PDeriv.ByteString.RightToLeft

-- Two-pass algorithm:
import Text.Regex.PDeriv.ByteString.TwoPasses

-- POSIX longest match:
import Text.Regex.PDeriv.ByteString.Posix
```

All backends export the same API: `compile`, `execute`, `regexec`, `defaultCompOpt`, `defaultExecOpt`.

### Bitcoded transducer

```haskell
import qualified Data.ByteString.Char8 as S
import Text.Regex.PDeriv.BitCode.Transduce
import Text.Regex.PDeriv.IntPattern
import Text.Regex.PDeriv.RE
import Text.Regex.PDeriv.Common (GFlag(..))

main :: IO ()
main = do
  -- Compile a regex pattern with capture groups
  let Right regex = compile (S.pack "(a+)(b+)")
  case regexec regex (S.pack "aaabb") of
    Right (Just (pre, match, post, groups)) -> do
      putStrLn $ "Match: " ++ S.unpack match
      mapM_ (putStrLn . ("  Group: " ++) . S.unpack) groups
      -- Output:
      --   Match: aaabb
      --   Group: aaa
      --   Group: bb
    Right Nothing ->
      putStrLn "No match"
    Left err ->
      putStrLn $ "Error: " ++ err
```

### Parsing regex patterns directly

```haskell
import Text.Regex.PDeriv.Parse

main :: IO ()
main = do
  case parsePat "(a|b)*c+" of
    Left err  -> print err
    Right pat -> print pat
```

## Supported Regex Syntax

| Syntax      | Description                         |
|-------------|-------------------------------------|
| `a`         | Literal character                   |
| `.`         | Any character                       |
| `[abc]`     | Character class                     |
| `[^abc]`    | Negated character class             |
| `[a-z]`     | Character range                     |
| `r1\|r2`    | Alternation                         |
| `r1r2`      | Concatenation                       |
| `r*`        | Kleene star (greedy)                |
| `r*?`       | Kleene star (non-greedy)            |
| `r+`        | One or more (greedy)                |
| `r+?`       | One or more (non-greedy)            |
| `r?`        | Optional (greedy)                   |
| `r??`       | Optional (non-greedy)               |
| `r{n}`      | Exactly n repetitions               |
| `r{n,m}`    | Between n and m repetitions         |
| `r{n,}`     | At least n repetitions              |
| `(r)`       | Capturing group                     |
| `(?:r)`     | Non-capturing group                 |
| `^`         | Start anchor                        |
| `$`         | End anchor                          |
| `\n`, `\t`  | Escape sequences                    |

## Module Structure

```
Text.Regex.PDeriv
  |-- RE                          -- Core regex AST and partial derivatives
  |-- Parse                       -- POSIX regex parser (via Parsec)
  |-- Common                      -- Shared types and utilities
  |-- IntPattern                  -- Internal pattern with binders
  |-- ExtPattern                  -- External pattern syntax
  |-- Translate                   -- ExtPattern -> IntPattern translation
  |-- Dictionary                  -- Trie-based hash dictionary
  |-- Nfa                         -- NFA construction
  |-- Pretty                      -- Pretty printing
  |-- Word                        -- Word type class
  |-- ByteString                  -- Default ByteString backend (LeftToRightD)
  |   |-- LeftToRight             -- Left-to-right NFA matching
  |   |-- LeftToRightD            -- Left-to-right DFA matching
  |   |-- LeftToRightS            -- Left-to-right simplified
  |   |-- RightToLeft             -- Right-to-left matching
  |   |-- TwoPasses               -- Two-pass matching
  |   |-- Posix                   -- POSIX longest match
  |-- String                      -- String backend (LeftToRightD)
  |   |-- LeftToRightD            -- Left-to-right DFA for String
  |-- BitCode
      |-- Bit                     -- Bitcoded partial derivatives
      |-- ParseTree               -- Universal parse tree representation
      |-- Transduce               -- Bitcoded transducer construction
```

## Building and Testing

```bash
# Build
cabal build

# Run tests
cabal test

# Build with Stack
stack build
stack test
```

## Requirements

- GHC >= 9.2 (tested with 9.6, 9.8, 9.10)
- Dependencies: `parsec`, `mtl`, `containers`, `bytestring`, `deepseq`, `parallel`, `regex-base`

## References

- V. Antimirov, "Partial Derivatives of Regular Expressions and Finite Automaton Constructions," *Theoretical Computer Science*, 1996.
- K. Z. M. Lu and M. Sulzmann, "POSIX Regular Expression Parsing with Derivatives," *FLOPS 2012*.

## License

BSD-3-Clause. See [LICENSE](LICENSE) for details.

## Authors

Kenny Zhuo Ming Lu and Martin Sulzmann

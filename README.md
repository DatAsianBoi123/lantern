# Lantern 🏮

A fast, typed, interpreted programming language.

**This is not a full-fledged language yet! Many features are missing, including an fully functional runtime. Expect bugs!**

## Installation

### From Source

First, install `git` and `cargo` if you haven't already.

Then, execute the following in your terminal:

```sh
git clone https://github.com/DatAsianBoi123/lantern.git
cd lantern
cargo install --path cli/
```
## Usage

```
Usage: lantern [OPTIONS] <FILE>

Arguments:
  <FILE>  

Options:
  -v, --verbose                  
  -n, --no-run                   
      --stack-dump <STACK_DUMP>  
  -h, --help                     Print help
  -V, --version                  Print version
```

A basic syntax file for `neovim` can be found at `syntax/la.vim`.

A treesitter server and LSP server is coming soon(tm).

For examples on basic syntax, please check out the files in the `scripts` directory.

## Architecture Details

A `lantern` file is first lexed and parsed into a list of statements, separated by newlines.
These statements are then compiled into the language-specific bytecode, called `flame`.
Finally, the runtime executes these `flame` instructions as simple, low-level instructions.

The `lantern` stack is made up of 8-byte slots, where each piece of data takes up 1 full slot.
This was mainly used in order to simplify alignment issues, not requiring the calculation of padding bytes.
This decision mimicks other interpreted languages, such as python (CPython), JS, and Java.

The stack is essentially a fixed-size array that lives in the rust heap (though, if you look at the code the implementation is much more low-level and is similar to rust's `Vec` struct).
This was done both to allow more flexibility within the code (no `const` generics), but also in order to guarantee that the stack is aligned to 8 bytes.
The performance impact of this decision is minimal, since the stack never needs to be reallocated and only allocates once per runtime.


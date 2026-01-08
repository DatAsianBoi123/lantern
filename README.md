# Lantern 🏮

A fast, typed, interpreted programming language.

**This is not a full-fledged language yet! Many features are missing, including an fully functional runtime. Expect bugs!**

**Also! The runtime is likely broken on 32 bit systems due to pointer size, though this was not tested and may still function properly**

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
  -h, --help     Print help
  -V, --version  Print version
```

A basic syntax file for `neovim` can be found at `syntax/la.vim`.

A treesitter server and LSP server are coming soon(tm).

For examples on basic syntax, please check out the files in the `scripts` directory.

## Architecture Details

A `lantern` is first parsed into a list of statements.
These statements are then compiled into the language-specific bytecode, called `flame`.
Finally, the runtime executes these `flame` instructions as simple, low-level instructions.

The `lantern` stack is made up of 16-byte slots, where each piece of data takes up 1 full slot.
The first 8 bytes are the data itself, while the next byte indicates whether the data is a primitive or a reference for GC purposes. The last 7 bytes are padding to keep alignment.

The GC is a simple implementation of [Cheney's algorithm](https://en.wikipedia.org/wiki/Cheney%27s_algorithm), with plans of a generational GC in the future.


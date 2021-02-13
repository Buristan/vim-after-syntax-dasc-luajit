LuaJIT's DynASM after Syntax for Vim
====================================

Syntax file extension for <vm_*.dasc> files in the
[LuaJIT project](https://luajit.org) to symplifing their reading.

Install
-------

Can be installed via a Vim add-in manager such as
[Plug](https://github.com/junegunn/vim-plug),
[Vundle](https://github.com/gmarik/vundle) or
[Pathogen](https://github.com/tpope/vim-pathogen).

Usage
-----

Once the files are installed the syntax highlighting will be
automatically enabled anytime you edit a `vm_*.dasc` file.

Also global variable `g:DascLuaJITFile` is set to the part of the filename like:
- `"x86"`
- `"x64"`
- `"arm"`
- `"arm64"`
- `"mips"`
- `"mips64"`
- `"ppc"`

and can be used for your own needs. If a filename is not matched `vm_*.dasc`,
the variable contains an empty string and the script finishes.

Syntax highlighting
-------------------

![Obligatory screenshot](https://raw.github.com/Buristan/vim-after-syntax-dasc-luajit/master/img-examples/example.png)

This script contains the following features:

# Available LuaJIT constants highlighting

Different macro definitions from <lua.h> <lj_obj.h> <lj_gc.h> are
highlighted as constants.

# Macroses highlighting

Different macrocommands from the corresponding <vm_*.dasc> file
are highlighted as functions.
Add this line to yours `.vimrc`, if you want to disable this behaviour:
```vim
let dasc_luajit_no_hl_macro=1
```

# Exclude extensions from highlighting

If you want to avoid definitions from extensions highlighting
add to yours `.vimrc`:
```vim
let dasc_luajit_no_extension_luavela=1
```
or/and
```vim
let dasc_luajit_no_extension_tarantool=1
```
for [LuaVela](https://github.com/luavela/luavela.git)
or/and [Tarantool](https://github.com/tarantool/luajit.git))
fork correspondingly.

# Colorised registers

Host and guest registers are highlighting.

For x86 and x84 also different colors for different register size
is used. Size of host and guest registers are matched by the color.

Add this line to yours `.vimrc`, if you want to disable this feature:
```vim
let dasc_luajit_no_hl_registers=1
```

If you want to enable LuaVela registers coloring (which is different from
original LuaJIT and Tarantool) edit this line to yours `.vimrc`:
```vim
let dasc_luajit_luavela_regcol=1
```

Feedback
--------

Please report bugs or asked questions at
[issues](https://github.com/Buristan/vim-after-syntax-dasc-luajit/issues).
Pull requests are welcome!

See also
--------

You may be interested in
[after syntax extension](https://github.com/Buristan/vim-syntax-dynasm)
for basic DynASM highlighting.

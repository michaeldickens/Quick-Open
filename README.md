Quick Open
==========

An Emacs utility for quickly finding and opening a file.

Installation
------------

To install, add `quick-open` to your load path. Then put this line
into your `.emacs` file: 

    (load "quick-open.el")

At the top of `quick-open.el` is the declaration for the
`*quick-open-path*` constant. Change its value appropriately.

Although I haven't tested this on any machines but my own, I believe
it should work on any Unix system.

Usage
-----

Quick Open works by searching directories for a file. In order for
it to do this, you must supply it with paths in which to search. Add a
new path by typing `M-x quick-open-add-path RET <absolute path> RET`.
Quick Open will remember paths even if you close and re-open Emacs.

I suggest that you only add directories with relatively few
files. Quick Open is still fairly slow, and if you add a path like
`~/`, it will spend minutes or hours traversing the file system and
use a lot of memory.

Invoke Quick Open by typing `M-x quick-open RET <filename> RET`. This
will search all previously-named paths for a file with the given name.

If Quick Open fails to find a file, this may be because its records
are out of date. It will ask you if you want it to search for the
file. This will cause it to traverse the paths you gave it and update
its records. The first time you invoke `quick-open`, it will always
say that the file is not found; simply say "yes" when it asks if you
want to search, and it should find the file.


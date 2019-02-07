# Command Options notes

## Short and Long args

Unlike GetOpt and derivatives we have to work with old programs that might
not have a consistent distinction between short and long option strings.

## Lists

List syntax is actually pretty nice for generating lists of things.
Concatenation (cf. SLFormat.Pretty) is seductive, but if something is a list
why hide it?

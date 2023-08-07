See also [Missing Features](missing_features.md).

The preprocessor algorithm I came up with is too clever for my own good;
it *will* break on complex replacement sequences. Maybe I can figure out
how to fix it at some point, or I'll just need to dumb it down.

Contrary to 6.2.7.1, for determining type compatibility, anonymous structures and
unions are _not_ considered a regular member of the containing structure
or union type; only their transmitted members are considered. This
actually may lead to alignment problems and will need to be mended at
some point.


The `inline` specifier is not quite handled correctly.

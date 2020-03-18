# Implementation notes

## Terminology

"url" refers to instances of the net/url struct, and "link" refers to the
string representation of a URL. A link can be "full" (includes scheme and
host) or "local" (no scheme or host, but absolute path including prefix). If
"full" is not specified, "local" is implicit. A "rel-www" is like a local
link with the prefix removed (and no initial "/").

```
Examples:
- "https://mysite.com/prefix/feeds/neato.atom.xml"   -- full link
- "/prefix/feeds/neato.atom.xml"                     -- local link
- "feeds/neato.atom.xml"                             -- rel-www
```

## Naming conventions

Function and method names ending in
- `-html` : returns a String of encoded HTML
- `-xexpr` : returns an XExpr (`xexpr?`, `xexpr/c` from the `xml` library)
- `-xexprs` : returns a list of XExpr
- `-url` : returns a URL struct (from `net/url-structs`)
- `-link` : returns a String containing an encoded URL (see above about local vs full)


## HTML Generation and Templates

Methods ending in `-html` and `-link` don't need escaping, but the
results of any other function or method needs to be escaped/converted;
otherwise, data may be misinterpreted as markup.

For example, suppose a post has the following metadata:

```
Title: When to use <article> vs <section>?
```

The posts's `get-title` method will return the string exactly as
written, so if it is inserted into a template (say, in a `title`
element), the *mentions* of elements will be misinterpreted as *uses*
of elements.

Use `@$h` to convert an XExpr to HTML --- and remember that a String
is also an XExpr! --- and use `$a` to encode a String as an attribute
value. (See `jeremiah/util` for more options.) See the included
templates for examples.

# Synopsis

BEncode is JSON-like format used in bittorrent protocol but might be
used anywhere else.

# Description

This package implements fast seamless encoding/decoding to/from
bencode format for many native datatypes. To achive
[more performance][1] we use new bytestring builders and hand
optimized attoparsec parser so this library is considered as
replacement for BEncode and AttoBencode packages.

## Format

Bencode is pretty similar to JSON: it has dictionaries(JSON objects),
lists(JSON arrays), strings and integers. However bencode has a few
advantages:

* No spaces in between any values — nor lists nor dicts nor anywhere else.
* Dictionaries always sorted lexicographically by the keys. This allow
  us to test data on equality without decoding from raw bytestring.
  Moreover this allow to hash encoded data (this property is heavily
  used by core bittorrent protocol).
* All strings prefixed with its length. This simplifies and speed up
  string parsing.


As always, each advantage has its disadvantage:

* Bencode is certainly less human readable than JSON.
* Bencode is rarely used. Except bittorrent protocol of course. But
  this might be not a problem if you are searching for format for
  internal use only.


# Documentation

For documentation see haddock generated documentation.

# Build Status

[![Build Status][2]][3]

# Authors

This library is written and maintained by Sam T. <pxqr.sta@gmail.com>
Feel free to report bugs and suggestions via github issue tracker or the mail.


[1]: http://htmlpreview.github.com/?https://github.com/pxqr/bencoding/master/bench/comparison.html
[2]: https://travis-ci.org/pxqr/bencoding.png
[3]: https://travis-ci.org/pxqr/bencoding

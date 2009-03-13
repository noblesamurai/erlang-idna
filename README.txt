A pure Erlang IDNA implementation.

Quick start:

  $ make
  ...
  $ erl -pa ebin
  ...
  1> inets:start(), idna:start(). % downloads UnicodeData.txt from unicode.org
  ...
  2> Domain = xmerl_ucs:from_utf8("www.詹姆斯.com").
  ...
  3> idna:to_ascii(Domain).
  ...

Reference material:

  RFC3490 (IDNA)
  http://www.ietf.org/rfc/rfc3490.txt

  RFC3492 (Punycode)
  http://www.ietf.org/rfc/rfc3492.txt

  addressable (Ruby URI implementation)
  http://github.com/sporkmonger/addressable

  punycode4r (Ruby punycode implementation)
  http://raa.ruby-lang.org/project/punycode4r/

  Unicode Character Database
  http://www.unicode.org/Public/UNIDATA/UCD.html

  UAX #15 (Unicode Normalization Forms)
  http://www.unicode.org/reports/tr15/

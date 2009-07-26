An Erlang implementation of URI Templates.

Quick start:

  $ make
  ...
  $ erl -pa ebin
  ...
  1> uri_template:sub([{foo, "fred"}], "http://example.org/?q={foo}").
  "http://example.org/?q=fred"


See src/uri_template_test.erl for more examples.

NFKC normalization and Unicode conversion (section 4.4. of the spec)
are currently unimplemented.

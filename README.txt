===================
erlang-uri-template
===================


What is this?
-------------

An Erlang implementation of the URI Template spec.


What are URI Templates?
-----------------------

Quoting http://bitworking.org/projects/URI-Templates/:

  URI Templates are strings that contain embedded variables that are
  transformed into URIs after embedded variables are substituted.


What do I need?
---------------

Erlang, and erlang-fmt (http://tfletcher.com/dev/erlang-fmt).

The Makefile assumes that erlang-fmt is contained in the parent directory
of this one, so you might want to edit the Makefile if you have it elsewhere.


How do I use it?
----------------

By calling uri_template:sub/2. The first argument should be a proplist
containing the values that need to be substituted. The second argument
should be a URI Template string. For example:

  "fred" = uri_template:sub([{foo, "fred"}], "{foo}").


Note: list values must be wrapped in a tuple so that they can be
distinguished from strings. For example, this will raise an error:

  uri_template:sub([{foo, ["bar", "baz"]}], "{-list|,|foo}").


Instead, do this:

  uri_template:sub([{foo, {list, ["bar", "baz"]}}], "{-list|,|foo}").


See src/uri_template_test.erl for more examples. NFKC normalization and
Unicode conversion (section 4.4. of the spec) are currently unimplemented.


Who can I contact if I have another question?
---------------------------------------------

Tim Fletcher (http://tfletcher.com/).

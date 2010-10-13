-module(uri_template_test).

-compile(export_all).

-import(lists, [reverse/1, reverse/2, foldl/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

suite_test_() ->
  [
  test_var_substitution(),
  test_the_opt_operator(),
  test_the_neg_operator(),
  test_the_prefix_operator(),
  test_the_suffix_operator(),
  test_the_join_operator(),
  test_the_list_operator(),
  test_examples()
  ].

test_var_substitution() ->
  test("('var') substitution", [{foo, "fred"}], [
    {"{foo}", "fred"},
    {"{bar=wilma}", "wilma"},
    {"{baz}", ""}
  ]).

test_the_opt_operator() ->
  test("the 'opt' operator", [{foo, "fred"}], [
    {"{-opt|fred@example.org|foo}", "fred@example.org"},
    {"{-opt|fred@example.org|bar}", ""}
  ]).

test_the_neg_operator() ->
  test("the 'neg' operator", [{foo, "fred"}], [
    {"{-neg|fred@example.org|foo}", ""},
    {"{-neg|fred@example.org|bar}", "fred@example.org"}
  ]).

test_the_prefix_operator() ->
  test("the 'prefix' operator", [{foo, "fred"}, {bar, {list, ["fee", "fi", "fo", "fum"]}}, {baz, []}], [
    {"{-prefix|/|foo}", "/fred"},
    {"{-prefix|/|bar}", "/fee/fi/fo/fum"},
    {"{-prefix|/|baz}", ""},
    {"{-prefix|/|qux}", ""}
  ]).

test_the_suffix_operator() ->
  test("the 'suffix' operator", [{foo, "fred"}, {bar, {list, ["fee", "fi", "fo", "fum"]}}, {baz, []}], [
    {"{-suffix|/|foo}", "fred/"},
    {"{-suffix|/|bar}", "fee/fi/fo/fum/"},
    {"{-suffix|/|baz}", ""},
    {"{-suffix|/|qux}", ""}
  ]).

test_the_join_operator() ->
  test("the 'join' operator", [{foo, "fred"}, {bar, "barney"}, {baz, ""}], [
    {"{-join|&|foo,bar,baz,qux}", "foo=fred&bar=barney&baz="},
    {"{-join|&|bar}", "bar=barney"},
    {"{-join|&|qux}", ""}
  ]).

test_the_list_operator() ->
  Def = [
    {foo, {list, ["fred", "barney", "wilma"]}},
    {bar, {list, ["a", "", "c"]}},
    {baz, {list, ["betty"]}},
    {qux, {list, []}}
  ],
  test("the 'list' operator", Def, [
    {"{-list|/|foo}", "fred/barney/wilma"},
    {"{-list|/|bar}", "a//c"},
    {"{-list|/|baz}", "betty"},
    {"{-list|/|qux}", ""},
    {"{-list|/|corge}", ""}
  ]).

test_examples() ->
  Def = [
    {foo, u(["03D3"])},
    {bar, "fred"},
    {baz, "10,20,30"},
    {qux, {list, ["10","20","30"]}},
    {corge, {list, []}},
    {grault, []},
    {garply, "a/b/c"},
    {waldo, "ben & jerrys"},
    {fred, {list, ["fred", "", "wilma"]}},
    {plugh, {list, [u(["017F", "0307"]), u(["0073", "0307"])]}},
    {'1-a_b.c', 200}
  ],
  test("examples", Def, [
    {"http://example.org/?q={bar}", "http://example.org/?q=fred"},
    {"/{xyzzy}", "/"},
    %{"http://example.org/?{-join|&|foo,bar,xyzzy,baz}", "http://example.org/?foo=%CE%8E&bar=fred&baz=10%2C20%2C30"}
    {"http://example.org/?d={-list|,|qux}", "http://example.org/?d=10,20,30"},
    {"http://example.org/?d={-list|&d=|qux}", "http://example.org/?d=10&d=20&d=30"},
    {"http://example.org/{bar}{bar}/{garply}", "http://example.org/fredfred/a%2Fb%2Fc"},
    {"http://example.org/{bar}{-prefix|/|fred}", "http://example.org/fred/fred//wilma"},
    %{"{-neg|:|corge}{-suffix|:|plugh}", ":%E1%B9%A1:%E1%B9%A1:"},
    {"../{waldo}/", "../ben%20%26%20jerrys/"},
    {"telnet:192.0.2.16{-opt|:80|grault}", "telnet:192.0.2.16:80"},
    {":{1-a_b.c}:", ":200:"}
  ]).


u(Chars) ->
  reverse(foldl(fun(Hex, Acc) -> reverse(uchr(Hex), Acc) end, [], Chars)).

uchr(Hex) ->
  xmerl_ucs:to_utf8(erlang:list_to_integer(Hex, 16)).

test(Tag, Def, Tests) ->
    {Tag, [test(Def, Test) || Test <- Tests]}.

test(Def, {Template, URI}) ->
    {Template, ?_assertEqual(URI, uri_template:sub(Def, Template))}.

-endif.

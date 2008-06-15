-module(uri_template_parse).

-export([template/1]).

-import(lists, [map/2, reverse/1, splitwith/2]).
-import(string, [tokens/2]).


template(String) ->
  template(String, []).

template([], Segments) ->
  reverse(Segments);
template([${|Etc], Segments) ->
  {E, Rem} = break($}, Etc),
  template(Rem, [expansion(E)|Segments]);
template([C|Etc], Segments) ->
  template(Etc, [C|Segments]).

expansion(String) ->
  case tokens(String, "|") of
    [String] ->
      var(String);
    [[$-|Op], Args, Vars] ->
      {list_to_atom(Op), Args, vars(Vars)}
  end.

vars(String) ->
  map(fun var/1, tokens(String, ",")).

var(String) ->
  case tokens(String, "=") of
    [String] ->
      {var, list_to_atom(String), []};
    [Var, Default] ->
      {var, list_to_atom(Var), Default}
  end.

break(Sep, List) ->
  case splitwith(fun(C) -> C =/= Sep end, List) of
    {Taken, []} ->
      {Taken, []};
    {Taken, [Sep|Etc]} ->
      {Taken, Etc}
  end.

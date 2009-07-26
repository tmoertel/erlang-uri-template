-module(uri_template_parse).

-export([template/1]).


template(String) ->
  template(String, []).

template([], Segments) ->
  lists:reverse(Segments);
template([${|Etc], Segments) ->
  {E, Rem} = break($}, Etc),
  template(Rem, [expansion(E)|Segments]);
template([C|Etc], Segments) ->
  template(Etc, [C|Segments]).

expansion(String) ->
  case string:tokens(String, "|") of
    [String] ->
      var(String);
    [[$-|Op], Args, Vars] ->
      {list_to_atom(Op), Args, vars(Vars)}
  end.

vars(String) ->
  [var(Token) || Token <- string:tokens(String, ",")].

var(String) ->
  case string:tokens(String, "=") of
    [String] ->
      {var, list_to_atom(String), []};
    [Var, Default] ->
      {var, list_to_atom(Var), Default}
  end.

break(Sep, List) ->
  case lists:splitwith(fun(C) -> C =/= Sep end, List) of
    {Taken, []} ->
      {Taken, []};
    {Taken, [Sep|Etc]} ->
      {Taken, Etc}
  end.

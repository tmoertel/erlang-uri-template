-module(uri_template).

-export([new/1]).
-export([sub/2]).

-import(lists, [foldl/3, reverse/1, reverse/2]).

-define(is_empty_list(Value), is_tuple(Value) andalso element(2, Value) =:= {list, []}).


new(String) ->
  {uri_template, uri_template_parse:template(String)}.

sub(Vars, Template) when is_list(Template) ->
  sub(encode(Vars), new(Template));
sub(Vars, {uri_template, Segments}) ->
  sub(Segments, Vars, []).

sub([], _Vars, URI) ->
  reverse(URI);
sub([Segment|Segments], Vars, URI) when is_integer(Segment) ->
  sub(Segments, Vars, [Segment|URI]);
sub([Segment|Segments], Vars, URI) ->
  sub(Segments, Vars, reverse(expand(Segment, Vars), URI)).

encode(Vars) ->
  [encode_var(V) || V <- Vars].

encode_var({Key, {list, List}}) ->
  {Key, {list, [fmt:percent_encode(Value) || Value <- List]}};
encode_var({Key, Value}) ->
  {Key, fmt:percent_encode(Value)}.

expand({var, Var, Default}, Values) ->
  case proplists:lookup(Var, Values) of
    none -> Default;
    {Var, Value} -> Value
  end;
expand({opt, Arg, Vars}, Values) ->
  expand_list(Vars, Values, fun(Value, Acc) -> expand_opt(Arg, Value, Acc) end);
expand({neg, Arg, Vars}, Values) ->
  expand_list(Vars, Values, fun(Value, Acc) -> expand_neg(Arg, Value, Acc) end);
% expand({prefix, _Arg, Vars}, _Values) when length(Vars) =/= 1 ->
%   todo; % this is an error
expand({prefix, Arg, [{var, Var, []}]}, Values) ->
  case proplists:lookup(Var, Values) of
    none -> [];
    {Var, []} -> [];
    {Var, {list, List}} -> expand_prefix(Arg, List);
    {Var, Value} -> expand_prefix(Arg, [Value])
  end;
% expand({suffix, _Arg, Vars}, _Values) when length(Vars) =/= 1 ->
%   todo; % this is an error
expand({suffix, Arg, [{var, Var, []}]}, Values) ->
  case proplists:lookup(Var, Values) of
    none -> [];
    {Var, []} -> [];
    {Var, {list, List}} -> expand_suffix(Arg, List);
    {Var, Value} -> expand_suffix(Arg, [Value])
  end;
expand({join, Arg, Vars}, Values) ->
  expand_list(Vars, Values, fun(Value, Acc) -> expand_join(Arg, Value, Acc) end);
% expand({list, _Arg, Vars}, _Values) when length(Vars) =/= 1 ->
%   todo; % this is an error
expand({list, Arg, [{var, Var, []}]}, Values) ->
  case proplists:lookup(Var, Values) of
    none -> [];
    {Var, []} -> [];
    {Var, {list, List}} -> string:join(List, Arg)
  end.

expand_opt(_Arg, Value, Acc) when Value =:= none; ?is_empty_list(Value) ->
  Acc;
expand_opt(Arg, _Value, Acc) ->
  reverse(Arg, Acc).

expand_neg(Arg, Value, Acc) when Value =:= none; ?is_empty_list(Value) ->
  reverse(Arg, Acc);
expand_neg(_Arg, _Value, Acc) ->
  Acc.

expand_prefix(Arg, List) ->
  reverse(foldl(fun(Value, Acc) -> reverse(Value, reverse(Arg, Acc)) end, [], List)).

expand_suffix(Arg, List) ->
  reverse(foldl(fun(Value, Acc) -> reverse(Arg, reverse(Value, Acc)) end, [], List)).

expand_join(_Arg, none, Acc) ->
  Acc;
expand_join(Arg, {Var, Value}, Acc) ->
  reverse([$=|Value], reverse(atom_to_list(Var), case Acc of [] -> []; _ -> reverse(Arg, Acc) end)).

expand_list(Vars, Values, Expand) ->
  reverse(foldl(fun({var, Var, []}, Acc) -> Expand(proplists:lookup(Var, Values), Acc) end, [], Vars)).

%%% @author Thomas Arts, Laura Castro, Macías López, Henrique Ferreiro
%%% @copyright (C) 2013 Prowess
%%% @doc
%%% From http://www.w3schools.com/schema/schema_dtypes_string.asp
%%% All elements may have restrictions, we encode them by a property list
%%%
%%% Restrictions that can be used with String data types:
%%%     enumeration  -- This is taken care of by parsing from WSDL, regardless of the base
%%%     length
%%%     maxLength
%%%     minLength
%%%     pattern (NMTOKENS, IDREFS, and ENTITIES cannot use this constraint)
%%%     whiteSpace
%%%
%%% @end
%%% Created : 17 Jun 2013 by Thomas Arts

-module(wsdl_dsl).
-include_lib("eqc/include/eqc.hrl").

-compile(export_all).  %% pick the API later
-export([wsdlType/1,
         length/2, maxLength/2, minLength/2, whiteSpace/2,
				 string/0]).

% FIXME: properly manage bounds
-define(UNBOUND_NEGINT, -1000).
-define(UNBOUND_POSINT, 1000).
-define(UNBOUND_LENGTH, 10).
-define(UNBOUND_OCCURS, 5).
-define(BASIC_TYPES, [bool, int, decimal, string, date]). % more to be added

%% AST :: Empty | Bool | Int | Decimal | String | Sequence | Union | List | {Tag, Attributes, AST}
%% Empty :: {empty, [], []}
%% Bool :: {bool, [], boolean()|[]}
%% Int :: {int, [], int()|[]}
%% Decimal :: {decimal, [], float()|[]}
%% String :: {string, Attributes, string()}
%% Sequence :: {sequence, Attributes, [AST]}
%% Union:: {union, Attributes, [AST]}
%% List :: {list, Attributes, AST}
%% Tag :: string() 
%% Attributes :: [{string(), int() | string() | String}]

%%% Constructors for WSDL 'types' (structures)

empty() ->
    {empty, [], []}.

% TODO: implement whitespace and pattern support
xbool() ->
    {bool, [], []}.

xbool(0) ->
    xbool(false);
xbool(1) ->
    xbool(true);
xbool(B) when B == true orelse B == false ->
    {bool, [], B}.

xint() ->
    {int, [], []}.

xint(I) when is_integer(I) ->
    {int, [], I}.

xdecimal() ->
    {decimal, [], []}.

xdecimal(D) when is_float(D) ->
    {decimal, [], D}.

xchar() ->
    {char, [], []}.
xchar(C) when 0 =< C andalso C =< 255 ->
    {char, [], C}.

string() ->
    {string, [], ""}.

string(S) ->
    {string, [], S}.

% TODO: more datatypes
%date() ->
%    {date, [], []}.

sequence(Elements) when is_list(Elements) ->
    {sequence, [], Elements}.

union(Elements) when is_list(Elements) ->
    {union, [], Elements}.

% we may not need to rename list once this is in a separate module (with no QC props)
xlist({_T, _A, _C} = Element) ->
    {list, [], Element}.

attributes(Attributes, {Tag, [], Content}) when is_list(Attributes) ->
    {Tag, Attributes, Content}.

tag(Name, Attributes, [H|_] = S) when is_integer(H) ->
    {Name, Attributes, string(S)};
tag(Name, Attributes, L) when is_list(L) ->
    {Name, Attributes, sequence(L)};
tag(Name, Attributes, {_,_,_} = C) ->
    {Name, Attributes, C}.

tag(Name, [{_Tag, _Content}|_] = Attrs) ->
    {Name, Attrs, empty()};
tag(Name, [H|_] = S) when is_integer(H) ->
	  {Name, [], string(S)};
% "" == [] so the empty string is treated as an empty sequence instead of the literal ""
tag(Name, L) when is_list(L) ->
    {Name, [], sequence(L)};
tag(Name, {_,_,_} = C) ->
    {Name, [], C}.

tag(Name) ->
    {Name, [], empty()}.

%%% Utility functions for WSDL constraints

length(N, {Tag, Attributes, Content}) when is_integer(N) ->
    %check_length(Attributes), % TODO: check_* in all constructors
    {Tag, [{length,N} | Attributes], Content}.

minLength(N, {Tag, Attributes, Content}) ->
    {Tag, [{minLength,N} | Attributes], Content}.

maxLength(N, {Tag, Attributes, Content}) ->
    {Tag, [{maxLength,N} | Attributes], Content}.

minInclusive(N, {Tag, Attributes, Content}) ->
    {Tag, [{minInclusive,N} | Attributes], Content}.

maxInclusive(N, {Tag, Attributes, Content}) ->
    {Tag, [{maxInclusive,N} | Attributes], Content}.

whiteSpace(Kind, {Tag, Attributes, Content}) ->
    {Tag, [{whiteSpace,Kind} | Attributes], Content}.

pattern(RegExp, {Tag, Attributes, Content}) ->
    {Tag, [{pattern,RegExp} | Attributes], Content}.

enumeration(Enum, {Tag, Attributes, Content}) ->
    {Tag, [{enumeration,Enum} | Attributes], Content}.

minOccurs(N, {Tag, Attributes, Content}) ->
    {Tag, [{minOccurs,N} | Attributes], Content}.

maxOccurs(N, {Tag, Attributes, Content}) ->
    {Tag, [{maxOccurs,N} | Attributes], Content}.

%%% Main WSDL constructor: ought to be called once, as external wrapper
wsdlType(WSDLType) ->
    T = check_constraints(WSDLType),
    generate(T).

%%% Internal stuff: checking WSDL structure coherence

% TODO: check enumeration

check_constraints(L) when is_list(L) ->
    [check_constraints(E) || E <- L];
check_constraints({empty, _, _} = WSDLType) ->
    WSDLType;
check_constraints({bool, _, _} = WSDLType) ->
    WSDLType;
check_constraints({int, _, _} = WSDLType) ->
    check_int_pattern(check_size(WSDLType));
check_constraints({decimal, _, _} = WSDLType) ->
    check_decimal_pattern(check_size(WSDLType));
check_constraints({char, _, _} = WSDLType) ->
    WSDLType;
check_constraints({string, _, ""} = WSDLType) ->
    check_pattern(check_whitespace(check_length(WSDLType)));
% TODO: check attributes?
check_constraints({string, _, _} = WSDLType) ->
    WSDLType;
check_constraints({Tag, Attributes, Content}) ->
    F = fun({K,V}) when is_tuple(V) ->
                            {K, check_constraints(V)};
                        (Else) ->
                            Else
        end,
    Min = proplists:get_value(minOccurs, Attributes),
    Max = proplists:get_value(maxOccurs, Attributes),
    check_occurences(Min, Max,
                     {Tag, [ F(A) || A <- Attributes], check_constraints(Content)}).

check_occurences(undefined,undefined,{list,_,_}=WSDLType) ->
    minOccurs(0,maxOccurs(?UNBOUND_OCCURS, WSDLType));
check_occurences(undefined,undefined,WSDLType) ->
    minOccurs(1,maxOccurs(1, WSDLType));
check_occurences(Min,unbound,WSDLType) when is_integer(Min) ->
    minOccurs(Min,maxOccurs(?UNBOUND_OCCURS,WSDLType));
check_occurences(Min,undefined,WSDLType) when is_integer(Min) ->
    minOccurs(Min,maxOccurs(?UNBOUND_OCCURS,WSDLType));
check_occurences(undefined,Max,WSDLType) when is_integer(Max) ->
    minOccurs(0,maxOccurs(Max,WSDLType));
check_occurences(Min,Max,WSDLType) when is_integer(Min),
                                        is_integer(Max), Min =< Max ->
    WSDLType;
check_occurences(_,_,WSDLType) ->
    erlang:error({conflicting_occurences,WSDLType}).

%% Auxiliary functions for checking WSDL structure (poor mans' typechecker)

% TODO: min/maxExclusive
check_size({int, Attributes, []} = WSDLType) ->
    Min = proplists:get_value(minInclusive, Attributes),
    Max = proplists:get_value(maxInclusive, Attributes),
    check_length_attributes(undefined, Min, Max, WSDLType);
% TODO: allow decimal bounds
check_size({decimal, Attributes, []} = WSDLType) ->
    Min = proplists:get_value(minInclusive, Attributes),
    Max = proplists:get_value(maxInclusive, Attributes),
    check_length_attributes(undefined, Min, Max, WSDLType).

check_size_attributes(undefined,undefined,undefined,WSDLType) ->
    minInclusive(?UNBOUND_NEGINT,maxInclusive(?UNBOUND_POSINT,WSDLType));
check_size_attributes(N,undefined,undefined,WSDLType) when is_integer(N) ->
    minInclusive(N,maxInclusive(N,WSDLType));
check_size_attributes(undefined,Min,undefined,WSDLType) when is_integer(Min) ->
    check_size(maxInclusive(?UNBOUND_POSINT,WSDLType));
check_size_attributes(undefined,undefined,Max,WSDLType) when is_integer(Max) ->
    check_size(minInclusive(?UNBOUND_NEGINT, WSDLType));
check_size_attributes(undefined,Min,Max,WSDLType) when is_integer(Min),
                                                       is_integer(Max), Min =< Max ->
    WSDLType;
check_size_attributes(_,_,_,WSDLType) ->
    erlang:error({conflicting_lengths,WSDLType}).

check_length({string, Attributes, []} = WSDLType) ->
    N = proplists:get_value(length, Attributes),
    Min = proplists:get_value(minLength, Attributes),
    Max = proplists:get_value(maxLength, Attributes),
    check_length_attributes(N, Min, Max, WSDLType).

check_length_attributes(undefined,undefined,undefined,WSDLType) ->
    minLength(0,maxLength(?UNBOUND_LENGTH,WSDLType));
check_length_attributes(N,undefined,undefined,WSDLType) when is_integer(N) ->
    minLength(N,maxLength(N,WSDLType));
check_length_attributes(undefined,Min,undefined,WSDLType) when is_integer(Min) ->
    check_length(maxLength(?UNBOUND_LENGTH,WSDLType));
check_length_attributes(undefined,undefined,Max,WSDLType) when is_integer(Max) ->
    check_length(minLength(0, WSDLType));
check_length_attributes(undefined,Min,Max,WSDLType) when is_integer(Min),
                                                         is_integer(Max), Min =< Max ->
    WSDLType;
check_length_attributes(_,_,_,WSDLType) ->
    erlang:error({conflicting_lengths,WSDLType}).

check_whitespace(WSDLType) ->
    WSDLType.
%check_whitespace(#wsdltype{constructor = base, type = string, whiteSpace = undefined} = WSDLType) ->
%    WSDLType#wsdltype{whiteSpace = preserve};
%check_whitespace(WSDLType) ->
%    WSDLType.

check_pattern({_Tag, Attributes, _Content} = WSDLType) ->
    Pat = proplists:get_value(pattern, Attributes),
    regexp_gen:check_pattern(Pat, WSDLType).

check_int_pattern({int, Attributes, _Content} = WSDLType) ->
    Pat = proplists:get_value(pattern, Attributes),
    regexp_gen:check_int_pattern(Pat, WSDLType).

check_decimal_pattern({decimal, Attributes, _Content} = WSDLType) ->
    Pat = proplists:get_value(pattern, Attributes),
    regexp_gen:check_decimal_pattern(Pat, WSDLType).

%% Once WSDL structure has been checked coherent, we may generate values for it
%% (we might want to make a generator that given a regexp, produces strings of that kind)
generate([]) ->
    [];
generate({Tag, Attributes, Content}) ->
    {Min, Max, Attrs} = expand_constraints(Attributes),
    G = generate_({Tag, Attrs, Content}),
    case {Min, Max} of
        {0, 0} ->
            none;
        {1, 1} ->
            G;
        _ ->
            ?LET(N, choose(Min, Max), vector(N, G))
    end.

generate_({empty, _, _}) ->
    [];
generate_({bool, _, []}) ->
    {bool, oneof([true, false])};
generate_({bool, _, B}) when B == true orelse B == false ->
    {bool, B};
% FIXME: how to match patterns with other attribures
generate_({int, Attributes, []}) ->
    Min = proplists:get_value(minInclusive, Attributes),
    Max = proplists:get_value(maxInclusive, Attributes),
    Pat = proplists:get_value(pattern, Attributes),
    Enum = proplists:get_value(enumeration, Attributes),
    N = if
            Enum /= undefined ->
                oneof(Enum);
            Pat /= undefined ->
                ?LET(S, regexp_gen:generate(Pat),
                     try list_to_integer(lists:flatten(S))
                     catch
                         error:badarg ->
                             erlang:error("Pattern is not an integer", Pat)
                     end);
            true ->
                choose(Min, Max)
        end,
    {int, N};
% TODO: Attributes == []?
generate_({int, _, I}) when is_integer(I) ->
    {int, I};
% TODO: attributes
generate_({decimal, Attributes, []}) ->
    Enum = proplists:get_value(enumeration, Attributes),
    N = if
            Enum /= undefined ->
                oneof(Enum);
            true ->
                real()
        end,
    {decimal, N};
generate_({decimal, _, D}) when is_float(D) ->
    {decimal, D};
generate_({string, Attributes, ""}) ->
    Min = proplists:get_value(minLength, Attributes),
    Max = proplists:get_value(maxLength, Attributes),
    WS  = proplists:get_value(whiteSpace, Attributes),
    Pat = proplists:get_value(pattern, Attributes),
    Enum = proplists:get_value(enumeration, Attributes),
    Gen = ascii_char(),
    S = if
            Enum /= undefined ->
                oneof(Enum);
            true ->
                ?SUCHTHAT(String,
                    ?LET(N, choose(Min,Max),
                         case Pat/=undefined of
                            true ->
                                ?LET(S, regexp_gen:generate(Pat), lists:flatten(S));
                            _ ->
                                escaped_string(
                                    if
                                        N<3  -> vector(N,non_whitespace(Gen,WS));
                                        true -> [non_whitespace(Gen,WS)] ++
                                                vector(N-2,non_tabcrlf(Gen,WS)) ++
                                                [non_whitespace(Gen,WS)]
                                    end)
                         end),
                    (WS/=collapse orelse no_dupl_spaces(String)))
        end,
    {string, S};
generate_({string, _, S}) ->
    {string, S};
generate_({sequence, [], Content}) ->
    [generate(C) || C <- Content];
generate_({union, [], Content}) ->
    oneof([ generate(C) || C <- Content]);
% TODO: fix list (?MAX_BOUND..)
generate_({list, [], Content}) ->
    % min/maxBounds was already added
    generate(Content);
generate_({Tag, Attrs, Content}) ->
    % Base cases need to be wrapped in a list
    C = case Content of
            {bool,_,_}    -> [generate(Content)];
            {int,_,_}     -> [generate(Content)];
            {decimal,_,_} -> [generate(Content)];
            {string,_,_}  -> [generate(Content)];
            _             ->  generate(Content)
        end,
    {Tag, [{K,generate(V)} || {K,V} <- Attrs], C}.

%% Last step of generation, expand restriction attributes such as minOccuexrs, maxOccurs
expand_constraints(Attributes) ->
    Min = proplists:get_value(minOccurs, Attributes),
    Max = proplists:get_value(maxOccurs, Attributes),
    {Min, Max, Attributes -- [{minOccurs, Min}, {maxOccurs, Max}]}.

%% Auxiliary generators
ascii_char() ->
    ?SHRINK(frequency([ {1,elements(lists:seq(33,33)++
                                    lists:seq(35,37)++
                                    lists:seq(39,47)++
                                    lists:seq(58,59)++
                                    lists:seq(61,61)++
                                    lists:seq(63,64)++
                                    lists:seq(91,96)++   %% ++ WARNING!! we have removed a lot of
                                    lists:seq(123,126))} %% symbols because XMERL SUCKS -> try erlsom!!
             % WARNING!!, {1,elements([32,9,10])}        %% whitespaces and tabs (same as above)
                        , {1,elements([32])}             %% (only whitespace) XMERL SUCKS!!
                        , {1,choose(48,57)}              %% digits
                        , {1,choose(65,90)}              %% uppercase
                        , {2,choose(97,122)}             %% lowercase
                      ]),
            [$a]).


escaped_string(Gen) ->
    ?LET(String,Gen,
         xmerl_lib:export_text(String)).

%% Auxiliary utility functions
non_whitespace(G,WS)
    when WS==replace orelse WS==collapse -> %%
    ?SUCHTHAT(C,G,not lists:member(C,[16#20,16#09,16#0A,16#0D]));
non_whitespace(G,_WS) ->
    G.

non_tabcrlf(G,WS) when WS==replace orelse WS==collapse ->
    ?SUCHTHAT(C,G,not lists:member(C,[16#09,16#0A,16#0D]));
non_tabcrlf(G,_WS) ->
    G.

no_dupl_spaces([32,32|_]) ->
    false;
no_dupl_spaces([_|Cs]) ->
    no_dupl_spaces(Cs);
no_dupl_spaces([]) ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%

prop_whitespace() ->
    ?FORALL(String,
        wsdlType(tag("Nombre", whiteSpace(collapse,minLength(1,maxLength(13,string()))))),
        collect(String, length(String) =< 13 andalso hd(String) /= 32 andalso lists:last(String) /= 32 andalso
                        no_dupl_spaces(String))).

prop_regexp() ->
    ?FORALL(RegExp, regexp(),
            ?FORALL(String,
                    wsdlType(tag("Nombre", pattern(RegExp,minLength(1,maxLength(13,string()))))),
                    collect(String,
                            length(String) =< 13
                            andalso re:run(String,RegExp) /= nomatch))).

regexp() ->
    regexp_gen:from_string("[a-z]*").


prop_attributes() ->
    ?FORALL(WSDLType,
            %wsdlType(attributes(list({name(),minLength(8,string())}),
            %                    tag("Name"))),
            wsdlType(tag("Name", list({name(),minLength(8,string())}))),
            lists:all(fun(X) -> tuple_size(X)==2 end, element(2, WSDLType))).

name() ->
    non_empty(list(choose($a, $z))).

non_duplicates_list(G) ->
    ?SIZED(N, utuple(N,G)).

utuple(0,_G) ->
    [];
utuple(N,G) ->
    ?LET(List,utuple(N-1,G),
         ?LET(Tuple, ?SUCHTHAT({Tag,_Body}, G, not lists:keymember(Tag,1,List)),
              [Tuple|List])).


canonize(WSDLValue) when is_list(WSDLValue) ->
    join(lists:flatten([canonize(E) || E <- WSDLValue]));
canonize({Tag, Attr, Value}) ->
    {Tag, Attr, canonize(Value)};
canonize({bool, Value}) ->
    {string, atom_to_list(Value)};
canonize({int, Value}) ->
    {string, integer_to_list(Value)};
canonize({decimal, Value}) ->
    {string, float_to_list(Value)};
canonize({string, []}) ->
    [];
canonize({string, Value}) ->
    {string, Value}.

join([{string,A},{string,B}|T]) ->
    join([{string, A ++ " " ++ B} | T]);
join([H|T]) ->
    [H|join(T)];
join([]) ->
    [].

% <outerName name1="xxxxxxxx"
%            name2="yyyyyyyy"
%  ...
%            nameN="zzzzzzzz">
% </outerName> (the property would compare this sample's structure with our AST)
% {"outerName",
%  [{"r", "blabla88"},
%   {"on", "bleble88"},
%   {"xcg", "blibli88"}],
%  []}
sample1() ->
    ?LET(Attributes, non_duplicates_list({name(),length(8,string())}),
         %wsdlType(attributes(Attributes,tag("outerName")))).
         wsdlType(tag("outerName", Attributes))).

% <roomId>IDofARoom</roomId>
sample2() ->
    wsdlType(tag("roomId", xint())).

% <room>
% <roomId>IDofARoom</roomId>
% <description>OptionalDescription</description>
% </room>
sample3() ->
    wsdlType(tag("room", [tag("roomId",xint()),
                          minOccurs(0,maxOccurs(1,tag("description", string())))])).

% <roomIDS>ID1 ID2 ID3 ID4 ID5</roomIDS>
sample4() ->
    wsdlType(tag("roomId",xlist(xint()))).

%<rooms>
% <room>
%  <roomId>strilakjsdf</roomId>
%  <description>lajfñsld</description>
% </room>
% <room>
%  <roomId>strilakjsdf</roomId>
% </room>
%<rooms>
sample5() ->
    wsdlType(tag("rooms",
                 xlist(tag("room", [tag("roomId",string()),
                                    minOccurs(0,maxOccurs(1,tag("description", string())))])))).

sample6() ->
    wsdlType(minOccurs(0, maxOccurs(1, tag("room",
                                           pattern(regexp_gen:regexp(regexp_gen:concat([$a,$b])), string()))))).

prop_samples() ->
    conjunction([{Tag, ?FORALL(WSDL, G, equals(wsdl_dsl_pp:dexmlize(wsdl_dsl_pp:xmlize(WSDL)),
                                               canonize(WSDL)))} ||
                 {Tag, G} <- [{sample1, sample1()},
                              {sample2, sample2()},
                              {sample3, sample3()},
                              {sample4, sample4()},
                              {sample5, sample5()}]]).

%<rooms atr="NN">
% <room atr="MM">
%  <roomId atr="ZZ">strilakjsdf</roomId>
%  <description>lajfñsld</description>
% </room>
% <room>
%  <roomId atr="ZZ">strilakjsdf</roomId>
% </room>
%<rooms>

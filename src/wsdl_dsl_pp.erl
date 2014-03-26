%%% @author Macías López
%%% @copyright (C) 2013 Prowess
%%% @doc
%%% AST pretty printer
%%% @end
%%% Created : 19 Jun 2013 by Macías López
-module(wsdl_dsl_pp).
%-export([xmlize/1,dexmlize/1]).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

-define(BASIC_TYPES, [int, string, date]). % more to be added

%% AST :: Int | String | {Tag, Attributes, [AST]}
%% Int :: {int, int()}
%% String :: {string, string()}
%% Tag :: string() 
%% Attributes :: [{string(), string()}]


% -type(AST | [AST] -> string())
xmlize(L) when is_list(L) ->
		[xmlize(E) || E <- join(lists:flatten(L))];
xmlize({Tag, Attrs, C}) ->
    open_tag(Tag, Attrs) ++
		lists:concat(xmlize(lists:flatten(C))) ++
    close_tag(Tag);
xmlize({int, Value}) ->
		integer_to_list(Value);
xmlize({string, Value}) ->
		Value;
xmlize(Any) ->
    Any.



join([{T1,_}=A,{T2,_}=B|T]) ->
		case (lists:member(T1,?BASIC_TYPES) andalso
					lists:member(T2,?BASIC_TYPES)) of
			true  -> [A,{string," "} | join([B | T])];
      false -> [A | join([B | T])]
 	  end;
join([H|T]) ->
		[H|join(T)];
join([]) ->
		[].

lt() ->
   "<".
gt() ->
    ">".
lt_slash() ->
    "</".
open_tag(Tag, Attrs) ->
    lists:concat([lt(), Tag,
                  lists:concat(lists:map(fun format_attr/1, Attrs)),
                  gt()]).
format_attr({K,V}) ->
    " " ++ K ++ "=\"" ++ replace_slash(V) ++ "\"".
close_tag(Tag) ->
    lists:concat([lt_slash(),
                  Tag,
                  gt()]).

replace_slash({string, Str}) ->
    lists:flatten(lists:map(fun(S) ->
                                    if S==92 -> "\\";
                                       true  -> S
                                  end end,Str)).

% -type(string() | [string()] -> AST)
dexmlize([H|_] = L) when is_list(H) ->
    [dexmlize(E) || E <- L];
dexmlize(String) ->
    {XML, []} = xmerl_scan:string(String),
    from_xml(XML).

from_xml(#xmlElement{name=Name, attributes = Attributes, content = Content}) ->
    {atom_to_list(Name), [from_xml(A) || A <- Attributes], [from_xml(C) || C <- Content]};
from_xml(#xmlAttribute{name = Name, value = Value}) ->
    {atom_to_list(Name), {string, Value}};
from_xml(#xmlText{value = Value}) ->
    {string, Value}; % could be other thing than string be we can't possibly know
from_xml(Str) ->
    {string, Str}.


%%% Examples

sample1() ->
    {"tag1",[],["content1"]}.

sample2() ->
    {"tag1",[{"a","aaaa"},{"b","bbbb"}],["content1"]}.

sample3() ->
    {"tag1",[],[{"tag2",[],["content2"]}]}.

sample4() ->
    {"tag1",
     [{"a","aaaa"},{"b","bbbb"}],
     [{"tag2",
       [{"c","cccc"},{"d","dddd"}],["content1"]}]}.

sample5() ->
    {"tag1",[],
     [{"tag2",[{"c","cccc"},{"d","dddd"}],["content2"]}]}.

sample6() ->
    {"tag1",[],
     [{"tag2",[],
       [{"tag21",[],["content21"]}]}
     ]}.

sample7() ->
    {"tag1", [],
     [{"tag2", [],
       [{"tag21", [], ["content21"] },
        {"tag22", [], ["content22"] }]  }
     ]}.

sample8() ->
    {"tag1", [],
     [{"tag2", [],
       [{"tag21", [], ["content21"] },
        {"tag22", [], ["content22"] }]  },
      {"tag3", [],
       [{"tag31", [], ["content31"]},
        {"tag32", [], ["content32"]}] }
     ]}.

sample9() ->
    {"tag1", [],
     [{"tag2", [],
       [{"tag21", [{"c", "cccc"}, {"d", "dddd"}], ["content21"] },
        {"tag22", [], ["content22"] }]  },
      {"tag3", [],
       [{"tag31", [{"a", "aaaa"}, {"b", "bbbb"}], ["content31"]},
        {"tag32", [], ["content32"]}] }
     ]}.

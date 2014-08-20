%%% @author Macías López <macias.lopez@udc.es>
%%% @copyright (C) 2013, Macías López
%%% @doc
%%% Regular expressions generator for XSD Schema
%%% @end
%%% Created :  4 Oct 2013 by Macías López <macias.lopez@udc.es>

-module(regexp_gen).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-define(MAXINT, 100).

%% A metacharacter in XML regular expressions is either '.', '\', '?', '*', '+', '{', '}', '(', ')', '|', '[', or ']'. 
%% The dot matches a single character, without caring what that character is. The only exception are line break characters
%% ^, $, word boundaries -> do not exist
%% ? -> zero or once any char. Can be applied to groups
%% . -> once any char

%% http://www.regular-expressions.info/xml.html
%% http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/#regexs

%% http://nemesis.lonestar.org/reference/telecom/codes/ascii.html
%% http://www.regular-expressions.info/posixbrackets.html
ascii_char() ->
    lists:seq(32,126).

ascii_char_gen() ->
    ?SHRINK(frequency([ {1,elements(lists:seq(32,47)++
                                        lists:seq(58,64)++
                                        lists:seq(91,96)++
                                        lists:seq(123,126))}
                        , {2,choose(48,57)}                  %% digits
                        , {2,choose(65,90)}                  %% uppercase
                        , {2,choose(97,122)}                 %% lowercase
                      ]),
            [$a]).


%% main function to build a regexp from a string
%% FIXME: we should check for incompatibilities
from_string(Pat) ->
    regexp(convert(Pat)).

% TODO
check_pattern(_Pat, WSDLType) ->
    WSDLType.

% TODO
check_int_pattern(_Pat, WSDLType) ->
    WSDLType.

% TODO
check_decimal_pattern(_Pat, WSDLType) ->
    WSDLType.

convert(Pattern) ->
    Pat = preprocess_pat(Pattern),
    case erlang_regexp:parse(Pat) of
        {ok, ParseRes} ->
            convert1(ParseRes);
        {error, Reason} ->
            erlang:error({wrong_regexp,Pattern,Reason})
     end.


% TODO: needed?
preprocess_pat(Pat) ->
    lists:flatten([case C of
                       127 -> "[0-9]";
                       C -> C
                   end||C<-Pat]).

convert1(Pat) when is_integer(Pat) ->
    Pat;
convert1({'or', E1, E2}) ->
    branch([convert1(E1), convert1(E2)]);
convert1({concat, E1, E2}) ->
    concat([convert1(E1), convert1(E2)]);
convert1({kclosure, E1}) ->
    star(convert1(E1));
convert1({pclosure, E1}) ->
    plus(convert1(E1));
convert1({char_class, Scope}) ->
    All = [case R of
               {S, E} -> range(S,E);
               _ -> R
           end||R<-Scope],
    charClass(All);
convert1({comp_class, E1}) ->
    neg(convert1({char_class, E1}));
convert1({optional,E1}) ->
    question(convert1(E1));
convert1({repeat, N, E}) ->
    repeat(N, convert1(E)).


%%

regexp(Pat) ->
    {regexp, Pat}.

%% L :: list of regexps to concat
%% Regexp: ab -> concat([$a,$b]).
concat(L) when is_list(L) ->
    {concat, L}.

%% L :: list of regexps, one must be selected
%% Regexp: a|b -> branch([$a,$b])
branch(L) when is_list(L) ->    
    {branch, L}.

%% Regexp: .
dot() ->
    dot.

%% RE :: regexp to be repeated
%% Regexp: a*
%% Repeat RE between 0 and n times. To repeat any char, use .*
star(RE) ->
    {star, RE}.

%% RE :: regexp to be repeated
%% Regexp: a+
%% Repeat RE between 1 and n times
plus(RE) ->
    {plus, RE}.

%% RE : regexp
%% Regexp: (ab)?
%% RE present once or absent
question(RE) ->
    repeat(0, 1, RE).


%% Min :: integer(), Max :: integer() | infinite | undefined , Pat :: pattern to be repeated
%% Regexp: a{0,5} | a{2,} | a{3}
repeat(0, 0, _RE) ->
    {repeat, {0,0}, ""};
repeat(Min, infinite, RE) ->        % {Min,} , between Min and infinite times
    {repeat, {Min, infinite}, RE};
repeat(Min, undefined, RE) ->       % {Min} , Min times exactly
    {repeat, {Min, undefined}, RE};
repeat(Min, Max, RE) ->             % {Min,Max} , between Min and Max times
    {repeat, {Min, Max}, RE}.

repeat(N, RE) ->
    repeat(N, undefined, RE).

%% Es :: List
%% Regexp: [ab] 
%% Selects one of the elements of the list.
%% The writing order does not matter
charClass(L) when is_list(L) ->
    NL = lists:foldr(fun(X,Acc) ->
                             case X of
                                 {charClass, V} ->
                                     [V|Acc];
                                 _Other ->
                                     [_Other|Acc]
                             end 
                     end, [], L),
    {charClass, lists:flatten(NL)}.

%% From, To :: limits in range
%% Regexp: 0-9
%% Builds a range of literals
range(From, To) ->
    charClass(lists:seq(From,To)).

%% L :: list
%% Regexp: [^ab]
%% Builds a charClass of all elements excluding chars in L
neg({charClass, L}) when is_list(L) ->
    charClass([lists:subtract(ascii_char(), lists:flatten(L))]).

%%
%% Shorthands to character classes
%%

%% Digits
%% Regexp: \d -> [0-9]
sh_d() ->
    charClass([range($0,$9)]).

%% Regexp: \D -> [^\d]
sh_D() ->
   neg(charClass([range($0,$9)])).
                  
%% Word character
%% Regexp: \w ->  [a-zA-Z0-9_]
sh_w() ->
    charClass([range($a,$z),
               range($A,$Z),
               range($0,$9),
               $_]).

%% Regexp: \W ->  [^a-zA-Z0-9_]
sh_W() ->
    neg(charClass([range($a,$z),
                   range($A,$Z),
                   range($0,$9),
                   $_])).

%% Whitespace character
%% Regexp: \s -> \t, \r, \n, SPC (09, 0D, 0A, 20)
sh_s() ->
    charClass([16#09,16#0D,16#0A,16#20]).

%% Regexp: \S -> [^\s]
sh_S() ->
    neg(charClass([16#09,16#0D,16#0A,16#20])).


%% Regexp: \i -> any character that may be the first character of an XML name
%% [_:A-Za-z]
sh_i() ->
    charClass([$_, $:, range($A,$Z), range($a,$z)]).

sh_I() ->
    neg(charClass([$_, $:, range($A,$Z), range($a,$z)])).

%% Regexp: \c -> any character that may occur after the first character in an XML name
%% [-._:A-Za-z0-9]
sh_c() ->
    charClass([$-,$.,$_,$:,range($A,$Z), range($a,z), range($0,$9)]).

sh_C() ->
    neg(charClass([$-,$.,$_,$:,range($A,$Z), range($a,z), range($0,$9)])).

%% Character class subtraction
%% Regexp: [a-z-[aeiou]]
subtract({charClass, L1}, {charClass, L2}) ->
    charClass([lists:subtract(L1, L2)]).


generate({regexp, R}) ->
    generate(R);
generate({concat, L}) when is_list(L) ->
    [generate(X) || X <- L];
generate({branch, L}) when is_list(L) ->
    ?LET(E, elements(L), generate(E));
generate(dot) ->
    ?SUCHTHAT(E, ascii_char_gen(), not lists:member(E, [16#0A,16#0D]));
generate({star, RE}) ->
    ?LET(Val, list(RE), generate(Val));
generate({plus, RE}) ->
    ?LET(Val, non_empty(list(RE)), generate(Val));

generate({repeat, {0,0}, _RE}) ->
    return("");
generate({repeat, {Min, infinite}, RE}) ->
    ?LET(N, choose(Min, ?MAXINT),
         ?LET(L, vector(N, RE), generate(L)));
generate({repeat, {Min, undefined}, RE}) ->
    ?LET(N, vector(Min, RE), generate(N));
generate({repeat, {Min, Max}, RE}) ->
    ?LET(N, choose(Min, Max),
         ?LET(L, vector(N, RE), generate(L)));

generate({charClass, L}) when is_list(L) ->
    ?LET(R, elements(L), generate(R));

generate(L) when is_list(L) ->
    [generate(X) || X <- L];
generate(Lit) ->
    [Lit].

%% Samples
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Regexp: [0-9-[0-6-[0-3]]]
sample_sub3() ->
    regexp(subtract(charClass([range($0,$9)]),
                    subtract(charClass([range($0,$6)]),
                             charClass([range($0,$3)])
                            ))).

%% Regexp: [0-9a-f-[4-6]]
sample_sub2() ->
    regexp(subtract(charClass([range($0,$9),
                               range($a,$f)]),
                    charClass([range($4,$6)])
                   )).



%% Regexp: [a-z-[aeiou]]
sample_sub1() ->
    regexp(subtract(charClass([range($a,$z)]),
                    charClass([$a,$e,$i,$o,$u])
                   )).

prop_sub1() ->
    ?FORALL(Str, wsdl_dsl:wsdlType(sample_sub1()),collect(Str,
            Str /= "a" andalso
            Str /= "e" andalso
            Str /= "i" andalso
            Str /= "o" andalso
            Str /= "u" )).


%% Regexp: \d{3}-[A-Z]{2}
sample_sh3() ->
    regexp(concat([repeat(3, undefined, sh_d()),
                   $-,
                   repeat(2, undefined, range($A,$Z))]
                 )).

%% [^\d\s]
sample_sh2() ->
    regexp(neg(charClass(([sh_d(), sh_s()])))).


%% [\D\S] -> matches all symbols
sample_sh1() ->
    regexp(charClass([sh_D(),
                      sh_S()
                     ]
                    )).

%% \D
sample_sh_D() ->
    regexp(sh_D()).

%% sh whitespace: A\sB
sample_sh_s1() ->
    regexp(concat([$A,sh_s(),$B])).


%% word: \w
sample_sh_w() ->
    regexp(sh_w()).

%% digit: [\da-fA-F]
sample_sh_d() ->
    regexp(charClass([sh_d(),
                      range($a,$f),
                      range($A,$F)
                     ]
                    )).

%% negCharClass: [^0-9a-z]
sample_negCharClass1() ->
    regexp(neg(charClass([range($0,$9),range($a,$z)]))).


%% charClass: [0-9]{2,4}-([A-Z1-5])?
sample_charClass9() ->
    regexp(concat([
                   repeat(2, 4, charClass([range($0,$9)])),
                   $-,
                   question(charClass([
                                       range($A,$Z),
                                       range($1,$5)
                                      ]))

                  ]
                 )).


%% charClass: [0-9]{3}-[0-9]{2}-[0-9]{4} 
sample_charClass8() ->
    regexp(concat([
                   repeat(3, undefined, charClass([range($0,$9)])),
                   $-,
                   repeat(2, undefined, charClass([range($0,$9)])),
                   $-,                 
                   repeat(4, undefined, charClass([range($0,$9)]))
                  ]
                 )).


%% charClass: [AF]-[BG]-[1-9]-[1-9a-f]
sample_charClass7() ->
    regexp(concat([
                   charClass([$A,$F]),
                   $-,
                   charClass([$B,$G]),
                   $-,
                   charClass([range($1,$9)]),
                   $-,
                   charClass([
                              range($1,$9),
                              range($a,$f)
                              ])
                  ]
                 )).


%% charClass html tag, <[a-zA-Z][a-zA-Z0-9]*>
sample_charClass6() ->
    regexp(concat([
                   $<,
                   charClass([
                              range($a,$z),
                              range($A,$Z)
                             ]),
                   star(charClass([
                                   range($a,$z),
                                   range($A,$Z),
                                   range($0,$9)
                                  ])
                       ),
                   $>        
                  ]
                 )
          ).

%% charClass: li[cs]en[cs]e
sample_charClass5() ->
    regexp(concat([
                   $l, $i,
                   charClass([$c, $s]),
                   $e, $n,
                   charClass([$c, $s]),
                   $e
                  ]
                 )
          ).

%% charClass: [0-9]*
sample_charClass4() ->
    regexp(star(charClass([range($0,$9)]))).


%% charClass: [0-9A-F]
sample_charClass3() ->
    regexp(charClass([
                      range($0,$9),
                      range($A,$F)
                      ]
                     )
           ).

%% charClass: [0-9]
sample_charClass2() ->
    regexp(charClass([range($0,$9)])).




% charClass: [ab]
sample_charClass1() ->
    regexp(
      charClass([$a,$b])).


%% repeat: a{2,4}b{3}c{1,}
sample_repeat2() ->
    regexp(concat([
                   repeat(2, 4, $a),
                   repeat(3, undefined, $b),
                   repeat(1, infinite, $c)
                  ]
                 )).


%% repeat: a{1}b{3}e*f*c{2}-.
sample_repeat1() ->
    regexp(concat([
                   repeat(1,undefined, $a),
                   repeat(3, undefined, $b),
                   concat([star($e), plus($f)]),
                   repeat(2, undefined, $c),
                   $-,
                   dot()
                  ]
                 )
          ).


%% plus: a+b | a*(cd)*(AC)+
sample_plus1() ->
    regexp(branch([
                   concat([plus($a), $b]) ,

                   concat( [star($a), 
                            star(concat([$c, $d])),
                            plus(concat([$A, $C]))
                           ]
                         )
                  ]
                 )
          ).


%% plus: a*(cd)*(AC)+
sample_plus2() ->
    regexp(concat( [star($a), 
                    star(concat([$c, $d])),
                    plus(concat([$A, $C]))
                   ]
                 )).


%% plus: a*word(ab)+
sample_plus3() ->
    regexp(concat([
                   star($a),
                   $w,$o,$r,$d,
                   plus(concat([$a,$b]))
                  ]
                 )).


%% star a*(cd)*
sample_star1() ->
    regexp(concat( [star($a), 
                    star(concat([$c, $d]))
                   ]
                 )).

%% star a*c*
sample_star2() ->
    regexp(concat( [star($a), 
                    star($c)
                   ]
                 )).

sample_re() ->
    concat([$a,$b]).

%% dot: ab.cd
sample_dot() ->
    regexp(concat([$a,$b,dot(),$c,$d])).


%% concat: ab
sample_concat(L) ->
    regexp(concat(L)).

prop_concat(L) ->
    ?FORALL(Str, wsdl_dsl:wsdlType(sample_concat(L)),
            lists:append([L]) == Str).


%% branch: cd|ab
sample_branch(L) ->
    regexp(branch(L)).

prop_branch(L) ->
    ?FORALL(Str, wsdl_dsl:wsdlType(sample_branch(L)),
            lists:member(hd(Str),L)).

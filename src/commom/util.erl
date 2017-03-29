%%Module  : util
%%% @Description: 
%%%-----------------------------------
-module(util).
-export([
		 get_fight_score_by_usr/1,
        log/5,
		to_binary/1,
		f2s/1,
		to_integer/1,
		to_atom/1,
		list_to_atom2/1,
		to_list/1,
		to_number/1,
		show/5,
        unixtime/0,
        longunixtime/0,
		date/0,
		date/1,
        md5/1,
		hexstring/1,
        rand/2,
		abs/1,
		ceil/1,
        floor/1,
        sleep/1,
        sleep/2,
        get_list/2,
        implode/2,
        implode/3,
        explode/2,
        explode/3,
        for/3,
        for/4,
		get_list_from_str/1,
		get_str_from_list/1,
		get_tuple_list_from_str/1,
		get_str_from_tuple_list/1,
		get_uid_by_name/1,
		get_lev_by_uid/1,
		get_prof_by_uid/1,
		get_name_by_uid/1,
        string_to_term/1,
        bitstring_to_term/1,
        term_to_string/1,
		unix_to_localtime/1,
		get_index_of/2,
        term_to_bitstring/1,
		map/2,
		get_bit/2,
		set_bit/3,
		escape_uri/1,
		unescape_string/1,
		unescape_other/1,
		timestamp/1,
		test_call/2,
		fun_call/2,
		key_max/2,
		key_min/2,
		gen_ip_list/1,
		gen_ip_list/2, 
		range/2,
		sample/2,
		gregorian_seconds_to_unixtime/1,
		get_tomorrow_date/0,
		get_tomorrow_date/1,
		get_relative_day/1,
		get_relative_week/2,
		unixtime_to_relative_day/2,
		relative_day_to_unixtime/2,
		is_in_time/2,
		random_by_weight/1,
		shuffle/1,
		random_string/1,
		intersection/1,intersection/2,
		union/2,union/1,
		eval/2,
		get_values/2,
		record_to_proplist/3,
		gen_order_id/0,
		gen_token/1,
		verify_token/2,
		get_usr_online/1,
		check_copy_play_sort/1,
		merge_list/2,
		 merge_three_arg_list/2,
		 combine/2
    ]).

-include("common.hrl").

-define(SECONDS_FROM_0_TO_1970, 62167248000).
-define(SECONDS_PER_DAY, 86400).

-define(TOKEN_KEY, "zFg0jqhBy2").
-define(TOKEN_LIFETIME, 3600).

%% List
implode(_S, [])->
	[<<>>];
implode(S, L) when is_list(L) ->
    implode(S, L, []).
implode(_S, [H], NList) ->
    lists:reverse([thing_to_list(H) | NList]);
implode(S, [H | T], NList) ->
    L = [thing_to_list(H) | NList],
    implode(S, T, [S | L]).

%% ->
explode(S, B)->
    re:split(B, S, [{return, list}]).
explode(S, B, int) ->
    [list_to_integer(Str) || Str <- explode(S, B), length(Str) > 0].

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_binary(X)  -> binary_to_list(X);
thing_to_list(X) when is_list(X)    -> X.

%% 
log(T, F, A, Mod, Line) ->
    {ok, Fl} = file:open("logs/error_log.txt", [write, append]),
    Format = list_to_binary("#" ++ T ++" ~s[~w:~w] " ++ F ++ "\r\n~n"),
    {{Y, M, D},{H, I, S}} = erlang:localtime(),
    Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":", integer_to_list(S)]),
    io:format(Fl, unicode:characters_to_list(Format), [Date, Mod, Line] ++ A),
	file:close(Fl).    
show(T, F, A, Mod, Line) ->
     Format = list_to_binary("#" ++ T ++" ~s[~w:~w] " ++ F ++ "\r\n~n"),
    {{Y, M, D},{H, I, S}} = erlang:localtime(),
    Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":", integer_to_list(S)]),
    io:format(unicode:characters_to_list(Format), [Date, Mod, Line] ++ A).

%% unix
unixtime() ->
   % {M, S, _} = erlang:now(),
   % M * 1000000 + S.
   M=erlang:systime(),
   trunc(M/1000000000).

longunixtime() ->
   % {M, S, Ms} = erlang:now(),
   % M * 1000000000 + S*1000 + Ms div 1000.
   M=erlang:systime(),
   trunc(M/1000000).


date() -> unix_to_localtime(unixtime()).
date(Type)->
	{{Year,Mon,Day},{Hour,Min,Sec}}=unix_to_localtime(unixtime()),
	case Type of
		all ->
			(Year*10000000000)+(Mon*100000000)+(Day*1000000)+(Hour*10000)+(Min*100)+Sec;
		year->
			Year;
		yearmonday->
			Year*10000 + Mon*100 +Day;
		_->
			{{Year,Mon,Day},{Hour,Min,Sec}}
	end.

unix_to_localtime(UnixTime) when is_integer(UnixTime) ->
	MegaSecs = UnixTime div 1000000,
	Secs = UnixTime rem 1000000,
	calendar:now_to_local_time({MegaSecs, Secs, 0}).
timestamp(UnixTime) when is_integer(UnixTime)->
	{{Y,M,D},{H,Min,S}}=unix_to_localtime(UnixTime),
	integer_to_list(Y) ++ "-"  ++ integer_to_list(M) ++ "-" ++
	integer_to_list(D) ++ "  " ++ integer_to_list(H) ++ ":" ++ 
	integer_to_list(Min) ++ ":"  ++ integer_to_list(S).

%% 获取当前时间相对于2010.1.1 Hour:00:00 的相对天数
get_relative_day(Hour) when is_integer(Hour)->
	unixtime_to_relative_day(unixtime(), Hour).

%% 获取当前时间相对于2010年第一个DayofWeek Hour:00:00 的相对周数
get_relative_week(DayofWeek, Hour) when is_integer(Hour) andalso is_integer(DayofWeek) andalso (DayofWeek >= 1 andalso DayofWeek =< 7) ->
	Day = (1 + 7 - calendar:day_of_the_week(2010, 1, 1) + DayofWeek) rem 7,
	{RelativeDay, {_Hour, _Min, _Sec}} = calendar:time_difference({{2010,1,Day}, {Hour, 0, 0}}, calendar:local_time()),
	RelativeDay div 7.

unixtime_to_relative_day(Timestamp, Hour) ->
	{Day, {_Hour, _Min, _Sec}} = calendar:time_difference({{2010,1,1}, {Hour, 0, 0}}, unix_to_localtime(Timestamp)),
	Day.

relative_day_to_unixtime(Day, Hour)  when is_integer(Day) andalso is_integer(Hour)->
	BaseTime = calendar:datetime_to_gregorian_seconds({{2010,1,1}, {Hour, 0, 0}}),
	Day * ?SECONDS_PER_DAY + BaseTime - ?SECONDS_FROM_0_TO_1970.

get_tomorrow_date() ->
	get_tomorrow_date(calendar:local_time()).

get_tomorrow_date(DateTime) ->
	Secs = calendar:datetime_to_gregorian_seconds(DateTime) + ?SECONDS_PER_DAY,
	calendar:gregorian_seconds_to_datetime(Secs).

gregorian_seconds_to_unixtime(Secs) ->
	Secs - ?SECONDS_FROM_0_TO_1970.


%% 判断当前时间是否在StartHour:StartMin到EndHour:EndMin这个时间段中
is_in_time({StartHour, StartMin}, {EndHour, EndMin}) when StartHour < EndHour orelse (StartHour == EndHour andalso StartMin < EndMin)->
	Now = {Date,_} = calendar:local_time(),
	CurSecs = calendar:datetime_to_gregorian_seconds(Now),
	StartSecs = calendar:datetime_to_gregorian_seconds({Date,{StartHour, StartMin, 0}}),
	if
		CurSecs >= StartSecs ->
			EndSecs = calendar:datetime_to_gregorian_seconds({Date,{EndHour, EndMin, 0}}),
			CurSecs =< EndSecs;
		true -> false
	end.

%% HEXmd5
md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

hexstring(Binary) when is_binary(Binary) ->
    lists:flatten(lists:map(
        fun(X) -> io_lib:format("~2.16.0b", [X]) end, 
        binary_to_list(Binary))).

%%
ceil(N) ->
    T = trunc(N),
    case N == T of
        true  -> T;
        false -> 1 + T
    end.

%%
floor(X) ->
    T = trunc(X),
    case (X < T) of
        true -> T - 1;
        _ -> T
    end.

abs(X) when X < 0 -> -1 * X;
abs(X) -> X.

 sleep(T) ->
    receive
    after T -> ok
    end.

 sleep(T, F) ->
    receive
    after T -> F()
    end.

get_list([], _) ->
    [];
get_list(X, F) ->
    F(X).

%% for
for(Max, Max, F) ->
    F(Max);
for(I, Max, F)   ->
    F(I),
    for(I+1, Max, F).


get_fight_score_by_usr(Uid)->
	case db:dirty_get(ply, Uid) of
		[#ply{fight_score=S}]->S;
		_->case db:dirty_get(usr, Uid) of
			   [#usr{fight_score=S}]->S;
			   _->0
		   end
	end.
%%合并物品列表 
merge_list(L1,List)->
	F=fun({Type,Num},L)->
			  case lists:keyfind(Type, 1, L) of
				  false->lists:keystore(Type, 1, L, {Type, Num});
				  {Type,New_Num}->lists:keyreplace(Type, 1, L, {Type,Num+New_Num})
			  end end,
	lists:foldl(F, L1, List).
merge_three_arg_list(L1,List)->
	F=fun({Type,Num,Bind},L)->
			  case lists:keyfind(Type, 1, L) of
				  false->lists:keystore(Type, 1, L, {Type, Num,Bind});
				  {Type,New_Num}->lists:keyreplace(Type, 1, L, {Type,Num+New_Num,Bind})
			  end end,
	lists:foldl(F, L1, List).
%% for
%% @return {ok, State}
for(Max, Min, _F, State) when Min<Max -> {ok, State};
for(Max, Max, F, State) -> F(Max, State);
for(I, Max, F, State)   -> {ok, NewState} = F(I, State), for(I+1, Max, F, NewState).

%% termtermstringe.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

%% termtermbitstringe.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~p", [Term])).

%% termstringterme.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.

%% termbitstringterme.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
    string_to_term(binary_to_list(BitString)).


%%All To int
to_integer(Msg) when is_integer(Msg) -> 
    Msg;
to_integer(Msg) when is_binary(Msg) ->
	Msg2 = binary_to_list(Msg),
    list_to_integer(Msg2);
to_integer(Msg) when is_list(Msg) -> 
    list_to_integer(Msg);
to_integer(Msg) when is_float(Msg) -> 
    round(Msg);
to_integer(_Msg) ->
    throw(other_value).

%% @doc convert other type to binary
to_binary(Msg) when is_binary(Msg) -> 
    Msg;
to_binary(Msg) when is_atom(Msg) ->
	list_to_binary(atom_to_list(Msg));
to_binary(Msg) when is_list(Msg) ->
	list_to_binary(Msg);
to_binary(Msg) when is_integer(Msg) -> 
	list_to_binary(integer_to_list(Msg));
to_binary(Msg) when is_float(Msg) -> 
	list_to_binary(f2s(Msg));
to_binary(Msg) when is_tuple(Msg) ->
	list_to_binary(tuple_to_list(Msg));
to_binary(_Msg) ->
    throw(other_value).

%% @doc convert other type to atom
to_atom(Msg) when is_atom(Msg) -> 
	Msg;
to_atom(Msg) when is_binary(Msg) -> 
	util:list_to_atom2(binary_to_list(Msg));
to_atom(Msg) when is_list(Msg) -> 
    util:list_to_atom2(Msg);
to_atom(_) -> 
    throw(other_value). 

list_to_atom2(List) when is_list(List) ->
	case catch(list_to_existing_atom(List)) of
		{'EXIT', _} -> erlang:list_to_atom(List);
		Atom when is_atom(Atom) -> Atom
	end.

%% @doc convert other type to list
to_list(Msg) when is_list(Msg) -> 
    Msg;
to_list(Msg) when is_atom(Msg) -> 
    atom_to_list(Msg);
to_list(Msg) when is_binary(Msg) -> 
    binary_to_list(Msg);
to_list(Msg) when is_integer(Msg) -> 
    integer_to_list(Msg);
to_list(Msg) when is_float(Msg) -> 
    f2s(Msg);
to_list(Msg) when is_tuple(Msg) ->
	tuple_to_list(Msg);
to_list(_) ->
    throw(other_value).

to_number(Msg) when is_list(Msg) ->
	case (catch list_to_integer(Msg)) of
		Value when is_integer(Value) -> Value;
		_ ->
			list_to_float(Msg)
	end;
to_number(Msg) when is_integer(Msg) ->
	Msg;
to_number(Msg) when is_float(Msg) ->
	Msg;
to_number(Msg) when is_binary(Msg) ->
	to_number(binary_to_list(Msg)).		
		
%% @doc convert float to string,  f2s(1.5678) -> 1.57
f2s(N) when is_integer(N) ->
    integer_to_list(N) ++ ".00";
f2s(F) when is_float(F) ->
    [A] = io_lib:format("~.2f", [F]),
	A.

len_pos( X,[ H|T ] ) when X =:=H -> length(T)+1;
len_pos( X,[ H|T ] ) when X =/=H -> len_pos( X,T ).
	
get_index_of( X, List ) ->
	NewList = lists:reverse( List ),
	Index = len_pos( X,NewList ),
	Index.

-spec map(fun((D) -> [R]), [D]) -> [R].
map(F, [H|T]) ->
    lists:append([F(H),map(F, T)]);
map(F, []) when is_function(F, 1) -> [].

% start from 1...
get_bit(Int, Pos) ->
	Index = Pos - 1,
	case Int band (1 bsl Index) of
		0 -> 0;
		_ -> 1
	end.

% start from 1...
set_bit(Int, Pos, 1) ->
	Index = Pos - 1,
	Int bor (1 bsl Index);
set_bit(Int, Pos, 0) ->
	Index = Pos - 1,
	Int band (bnot (1 bsl Index)).

rand(Same, Same)-> Same;
rand(Min, Max) ->
    case get('rand_seed') of
        undefined ->
            RandSpeed=random:seed(unixtime()),
            put('rand_seed', RandSpeed);
        _ ->skip
    end,
	if 
		Max > Min->	random:uniform(Max - (Min - 1)) + (Min - 1);
		Max < Min->	random:uniform(Min - (Max - 1)) + (Max - 1)
	end.

%% url_encode(Data) ->
%%     url_encode(Data,"").
%% 
%% url_encode([],Acc) ->
%%     Acc;
%% 
%% url_encode([{Key,Value}|R],"") ->
%%     url_encode(R, edoc_lib:escape_uri(Key) ++ "=" ++ edoc_lib:escape_uri(Value));
%% url_encode([{Key,Value}|R],Acc) ->
%%     url_encode(R, Acc ++ "&" ++ edoc_lib:escape_uri(Key) ++ "=" ++ edoc_lib:escape_uri(Value)).

escape_uri(S) when is_list(S) ->
%%     escape_uri(unicode:characters_to_binary(S));
	escape_uri(list_to_binary(S));
escape_uri(<<C:8, Cs/binary>>) when C >= $a, C =< $z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $A, C =< $Z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $0, C =< $9 ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $. ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $- ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $_ ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) ->
    escape_byte(C) ++ escape_uri(Cs);
escape_uri(<<>>) ->
    "".

escape_byte(C) ->
    "%" ++ hex_octet(C).

hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $A].


%% Converts '%3c' to \<
unescape_other(Str) ->
	unescape_other(Str, []).
unescape_other([], Acc) ->   
   	lists:reverse(Acc);
unescape_other([$%, H1, H2 |T], Acc) ->
    I1 = hex2int(H1),
    I2 = hex2int(H2),
    I = I1 * 16 + I2,
	unescape_other(T, [I, $\ |Acc]);
unescape_other([H|T], Acc) ->
    unescape_other(T, [H|Acc]).

unescape_string(Str) ->
	unescape_string(Str, []).
unescape_string([], Acc) ->   
   	lists:reverse(Acc);
unescape_string([$%, H1, H2 |T], Acc) ->
    I1 = hex2int(H1),
    I2 = hex2int(H2),
    I = I1 * 16 + I2,
	unescape_string(T, [I, []|Acc]);
unescape_string([H|T], Acc) ->
    unescape_string(T, [H|Acc]).


fun_call(_Num,_N) -> ok.
test_call(_Num,_N)-> ok.

hex2int(H) when H >= $a -> 10 + H - $a;
hex2int(H) when H >= $A -> 10 + H -$A;
hex2int(H) ->  H - $0.

%% key_max([], KeyPos) -> Tuple | {}
key_max(List, KeyPos) ->
	key_max(List, KeyPos, {}).

key_max([], _KeyPos, Ret) ->
	Ret;
key_max([H|T], KeyPos, {}) ->
	key_max(T, KeyPos, H);
key_max([H|T], KeyPos, Ret) ->
	case element(KeyPos, H) > element(KeyPos, Ret) of
	true -> key_max(T, KeyPos, H);
	false -> key_max(T, KeyPos, Ret)
	end.

%% key_min([], KeyPos) -> Tuple | {}
key_min(List, KeyPos) ->
	key_min(List, KeyPos, {}).

key_min([], _KeyPos, Ret) ->
	Ret;
key_min([H|T], KeyPos, {}) ->
	key_min(T, KeyPos, H);
key_min([H|T], KeyPos, Ret) ->
	case element(KeyPos, H) < element(KeyPos, Ret) of
	true -> key_min(T, KeyPos, H);
	false -> key_min(T, KeyPos, Ret)
	end.


%% range(Min, Max) -> [Min,Min+1,Min+2,...,Max]
range(Min, Max) when Min>=0,Max>=Min->
	range(Min, Max, []).
range(Min, Min, L)->
	[Min|L];
range(_, 0, L)->
	L;
range(Min, Max, L)->
	range(Min, Max-1, [Max|L]).

%% gen_ip_list("1.2.3.4/30") -> IpList
gen_ip_list(CidrIp) when erlang:is_list(CidrIp)->
	[A,B,C,D,MaskBits] = string:tokens(CidrIp, "./"),
	gen_ip_list({list_to_integer(A), list_to_integer(B), list_to_integer(C), list_to_integer(D)}, list_to_integer(MaskBits)).
	
gen_ip_list(Ip, MaskBits) ->
	{A, B, C, D} = Ip,
	{M1, M2, M3, M4} = cidr_netmask(MaskBits),
	NetworkAddr = {A band M1, B band M2, C band M3, D band M4},
	BroadcastAddr = {A bor ((bnot M1) band 16#ff), B bor ((bnot M2) band 16#ff), C bor ((bnot M3) band 16#ff), D bor ((bnot M4) band 16#ff)},
	gen_ip_list_by_range(NetworkAddr,BroadcastAddr).
	
gen_ip_list_by_range(NetworkAddr, BroadcastAddr) ->
	{Na1, Na2, Na3, Na4} = NetworkAddr,
	{Ba1, Ba2, Ba3, Ba4} = BroadcastAddr,

	F3 = fun(V) ->
			lists:map(fun(_V)->erlang:list_to_tuple(V++[_V]) end, range(Na4, Ba4))
		end,
	
	F2 = fun(V) ->
			List = lists:map(fun(_V)->V++[_V] end, range(Na3, Ba3)),
			lists:map(F3, List)
		end,

	F1 = fun(V) ->
			List = lists:map(fun(_V)->[V,_V] end, range(Na2, Ba2)),
			lists:map(F2, List)
		 end,
	lists:flatten(lists:map(F1, range(Na1, Ba1))).
		
cidr_netmask(Bits) when is_integer(Bits) andalso Bits =< 32 ->
    ZeroBits = 8 - (Bits rem 8),
    Last = (16#ff bsr ZeroBits) bsl ZeroBits,
    
    case (Bits div 8) of
        0 ->
            {(255 band Last), 0, 0, 0};
        1 ->
            {255, (255 band Last), 0, 0};
        2 ->
            {255, 255, (255 band Last), 0};
        3 ->
            {255, 255, 255, (255 band Last)};
        4 ->
            {255, 255, 255, 255}
    end.

sample(List, N) when erlang:is_list(List) andalso erlang:is_integer(N) ->
	if 
		N < 0 orelse length(List) < N ->
			erlang:error(badarg);
		true ->
			sample(List, N, [])
	end.
sample(_List, N, RetList) when N =< 0 ->
	RetList;
sample(List, N, RetList) ->
  	Len = length(List),
	Index = random:uniform(Len),
	{L1, L2} = lists:split(Index, List),
	{L3, [Elem]} = lists:split(length(L1) - 1, L1),
	sample(L2++L3, N-1, [Elem|RetList]).

random_by_weight(List) ->
	F = fun({_Data, Weight}, Total) ->
			Total + Weight
		end,
	TotalWeight = lists:foldl(F, 0, List),
	random_by_weight(List, random:uniform() * TotalWeight, 0).

random_by_weight([{Data, Weight}|Next], R, Total) ->
	if 
		R =< Weight + Total ->
			Data;
		true ->
			random_by_weight(Next, R, Total + Weight)
	end;
random_by_weight([], _R, _Total) ->
	erlang:error(badarg).

shuffle(List) ->
	L1 = [{random:uniform(),N}|| N <- List],
	L2 = lists:keysort(1, L1),
	[N || N <- L2].

random_string(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).

intersection(L1, L2) ->
	lists:filter(fun(Elem)->lists:member(Elem, L1) end, L2).
intersection([L1,L2|Rest]) ->
	intersection1(intersection(L1,L2),Rest);
intersection([L]) -> L.

intersection1(L1, [L2|Rest]) ->
	intersection1(intersection(L1,L2), Rest);
intersection1(L, []) -> L.

union(L1, L2) ->
	union1(L2, L1).
union(ListOfList) ->
	union2(ListOfList, []).

union1([Elem|L2], Ret) ->
	case lists:member(Elem, Ret) of
		true -> union1(L2, Ret);
		_ -> union1(L2, [Elem|Ret])
	end;
union1([], Ret) -> Ret.

union2([L1|Rest], Ret) ->
	union2(Rest, union(L1, Ret));
union2([], Ret) -> Ret.

eval(S,Environ) ->
    {ok,Scanned,_} = erl_scan:string(S),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    erl_eval:exprs(Parsed,Environ).

get_values(KeyList, KvList) ->
	get_values(KeyList, KvList, []).
get_values([K|Rest], List, Ret) ->
	case lists:keyfind(K, 1, List) of
		{K, V} -> get_values(Rest, List, [V|Ret]);
		_ -> false
	end;
get_values([], _, Ret) -> lists:reverse(Ret).

record_to_proplist(Tag, Record, KeyList) when element(1, Record) == Tag ->
	[Tag|ValueList] = erlang:tuple_to_list(Record),
	lists:zip(KeyList, ValueList).

gen_order_id() ->
	% server_id(1-3bytes) + timestamp(13bytes) + index(1-3bytes)
	GroupId = db:get_all_config(serverid),
	Idx = 
	case get(order_id_idx) of
		N when erlang:is_integer(N) -> N;
		_ -> 0
	end,
	put(order_id_idx,Idx+1),
	string:join([util:to_list(GroupId), util:to_list(longunixtime()), util:to_list(Idx rem 1000)], "").
get_lev_by_uid(Uid)->
	case db:dirty_get(usr, Uid) of
		[Usr|_]->
			Usr#usr.lev;
		_->0
	end.
get_prof_by_uid(Uid)->
	case db:dirty_get(usr, Uid) of
		[Usr|_]->
			Usr#usr.prof;
		_->0
	end.
get_uid_by_name(Name)->
	BinName = util:to_binary(Name), 
	case db:dirty_get(usr, BinName,#usr.name) of
		[Usr|_]->
			Usr#usr.id;
		_->case db:find(usr, name, list_to_binary(Name)) of
					[Usr|_]-> Usr#usr.id;
			_->0
		   end
	end.
get_name_by_uid(Uid)->
	case db:dirty_get(usr, Uid) of
		[Usr|_]->
			Usr#usr.name;
		_->case db:find(usr, id,Uid) of
			   [Usr|_]->
				   Usr#usr.name;
			   _->""
		   end
	end.
gen_token({Aid,TimeStamp}) ->
	S = string:join([util:to_list(Aid), util:to_list(TimeStamp), ?TOKEN_KEY], "#"),
	util:md5(S).

verify_token({Aid,TimeStamp}, Token) ->
	case string:equal(gen_token({Aid,TimeStamp}), util:to_list(Token)) of
		true ->
			unixtime() - util:to_integer(TimeStamp) < ?TOKEN_LIFETIME;
		_ -> false
	end.
get_tuple_list_from_str(Str)->
	case util:to_list(Str) of
		[]->[];
		S->Reward_str_list=string:tokens(S, ","),
		   F=fun(Reward_id_str)->
					 R_str_list=string:tokens(Reward_id_str, "|"),
					 F1=fun(R_STR)->
								util:to_integer(R_STR) end,
					 Undue=lists:map(F1, R_str_list),
					 list_to_tuple(Undue) end,
		   lists:map(F, Reward_str_list)
	end.
get_str_from_tuple_list(List)->
F=fun({Arg1,Arg2})->
			 string:join([integer_to_list(Arg1)]++[integer_to_list(Arg2)],"|") end,
	Str_list=lists:map(F, List),
  	string:join(Str_list, ",").
get_list_from_str(Str)->
	case util:to_list(Str) of
		[]->[];
		S->Reward_str_list=string:tokens(S, ","),
		   	  F=fun(Reward_id_str)->
							util:to_integer(Reward_id_str) end,
		   	  lists:map(F, Reward_str_list)
	end.
get_str_from_list([]) ->"";
get_str_from_list([S]) ->to_list(S);
get_str_from_list(List) when length(List)>1->
F=fun(ID)->
			  integer_to_list(ID) end,
	Str_list=lists:map(F, List),
		
  	string:join(Str_list, ",").

get_usr_online(Uid)->
	case db:dirty_get(ply, Uid) of
		[_Ply|_]->?STATE_ONLINE;
		_P->?STATE_OFFLINE
	end.

check_copy_play_sort(Play_Sort)->
	if Play_Sort >=1 andalso Play_Sort =< 5 andalso Play_Sort =/= 0->?COPY_SORT_SINGLE_MANY;
	   Play_Sort ==6 -> ?COPY_SORT_ENDLESS;
	   Play_Sort ==8 -> ?COPY_SORT_QUELL_DEMON;
	   Play_Sort == 10 -> ?COPY_SORT_GUILD_GUARD;
	   Play_Sort == 14 ->?COPY_SORT_SEAL_BOSS;
	   true -> ?SCENE_SORT_COPY
	end.	

% reward_pack(Reward) ->
% 	F= fun({Type,Num}) ->
% 			   Pt1 =pt_public_class:reward_list_new(),
% 			   Pt1#pt_public_reward_list{type=Type,num=Num}
% 	   end,
% 	lists:map(F, Reward).




%% 生成有Num个元素的所有组合
combine(List, Num) when is_list(List),is_integer(Num),Num >= 1 ->
	if
		length(List) < Num -> erlang:error(badarg);
		true -> do_combine(List,Num)
	end.

merge_combine(Elem, List) ->
	lists:map(fun(L) -> [Elem|L] end, List).


do_combine(List, 1) ->
	[[E] || E <- List];
do_combine(List, Num) when length(List) == Num -> [List];
do_combine(List, Num) ->
	Max = length(List) - Num + 1,
	lists:foldl(
		fun(N, Ret) ->
			Ret ++ merge_combine(lists:nth(N,List), do_combine(lists:nthtail(N,List), Num-1))
		end, [], lists:seq(1,Max)).


-module(lib_protocol).
-include("common.hrl").

-export([init_pt/0,regist_pt/1,fill_pt/1]).
-export([from_binary/2,to_binary/2,make_pt/3]).

-record(proto_def, {id,module}).

init_pt() -> ets:new(all_protos, [set,public,named_table,{keypos, #proto_def.id},{read_concurrency,true}]).
regist_pt(Module) ->
%% 	Module = erlang:list_to_atom("pt_" ++ erlang:atom_to_list(Name)),
	Id = Module:get_id(),
	case ets:lookup(all_protos, Id) of
		[#proto_def{module=OldModule}] -> {registed, Id, OldModule};
		_ ->
			ets:insert(all_protos, #proto_def{id=Id,module=Module}),
			ok
	end.
fill_pt(<<Id:16/unsigned-integer,Seq:32/unsigned-integer,Binary/binary>>)->
	case ets:lookup(all_protos, Id) of
		[#proto_def{module=Module}] -> 
			case from_binary_by_module(Binary,Module) of
				null -> null;
				Pt -> {ok,Id,Module,Seq,Pt}
			end;
		_ -> {no_regist,Id}
	end;
fill_pt(_)-> no_pt.

from_binary_by_module(Binary,Module)->
	Module:from_binary(Binary).

from_binary(<<Data:8/signed-integer,Next/binary>>,int8) -> {Next,Data};
from_binary(<<Data:8/unsigned-integer,Next/binary>>,uint8) -> {Next,Data};
from_binary(<<Data:16/signed-integer,Next/binary>>,int16) -> {Next,Data};
from_binary(<<Data:16/unsigned-integer,Next/binary>>,uint16) -> {Next,Data};
from_binary(<<Data:32/signed-integer,Next/binary>>,int32) -> {Next,Data};
from_binary(<<Data:32/unsigned-integer,Next/binary>>,uint32) -> {Next,Data};
from_binary(<<Data:64/signed-integer,Next/binary>>,int64) -> {Next,Data};
from_binary(<<Data:64/unsigned-integer,Next/binary>>,uint64) -> {Next,Data};
from_binary(<<Data:32/float,Next/binary>>,float) -> {Next,Data};
from_binary(<<Len:16/unsigned-integer,Data:Len/binary-unit:8,Next/binary>>,string) -> {Next,erlang:binary_to_list(Data)};
from_binary(<<Len:16/unsigned-integer,Next/binary>>,{list,int8}) ->
	F = fun(_I,{Objs,GetNext}) ->
				case from_binary(GetNext,int8) of
					{NewNext,Obj} ->
						{ok,{Objs ++ [Obj],NewNext}};
					_ ->
						{ok,{Objs,Next}}
				end
		end,
	case util:for(1, Len, F,{[],Next}) of
		{ok,{GetObjs,AllNext}} ->
			{AllNext,GetObjs};
		_ -> 
			{Next,[]}
	end;
from_binary(<<Len:16/unsigned-integer,Next/binary>>,{list,uint8}) ->
	F = fun(_I,{Objs,GetNext}) ->
				case from_binary(GetNext,uint8) of
					{NewNext,Obj} ->
						{ok,{Objs ++ [Obj],NewNext}};
					_ ->
						{ok,{Objs,Next}}
				end
		end,
	case util:for(1, Len, F,{[],Next}) of
		{ok,{GetObjs,AllNext}} ->
			{AllNext,GetObjs};
		_ -> 
			{Next,[]}
	end;
from_binary(<<Len:16/unsigned-integer,Next/binary>>,{list,int16}) ->
	F = fun(_I,{Objs,GetNext}) ->
				case from_binary(GetNext,int16) of
					{NewNext,Obj} ->
						{ok,{Objs ++ [Obj],NewNext}};
					_ ->
						{ok,{Objs,Next}}
				end
		end,
	case util:for(1, Len, F,{[],Next}) of
		{ok,{GetObjs,AllNext}} ->
			{AllNext,GetObjs};
		_ -> 
			{Next,[]}
	end;
from_binary(<<Len:16/unsigned-integer,Next/binary>>,{list,uint16}) ->
	F = fun(_I,{Objs,GetNext}) ->
				case from_binary(GetNext,uint16) of
					{NewNext,Obj} ->
						{ok,{Objs ++ [Obj],NewNext}};
					_ ->
						{ok,{Objs,Next}}
				end
		end,
	case util:for(1, Len, F,{[],Next}) of
		{ok,{GetObjs,AllNext}} ->
			{AllNext,GetObjs};
		_ -> 
			{Next,[]}
	end;
from_binary(<<Len:16/unsigned-integer,Next/binary>>,{list,int32}) ->
	F = fun(_I,{Objs,GetNext}) ->
				case from_binary(GetNext,int32) of
					{NewNext,Obj} ->
						{ok,{Objs ++ [Obj],NewNext}};
					_ ->
						{ok,{Objs,Next}}
				end
		end,
	case util:for(1, Len, F,{[],Next}) of
		{ok,{GetObjs,AllNext}} ->
			{AllNext,GetObjs};
		_ -> 
			{Next,[]}
	end;
from_binary(<<Len:16/unsigned-integer,Next/binary>>,{list,uint32}) ->
	F = fun(_I,{Objs,GetNext}) ->
				case from_binary(GetNext,uint32) of
					{NewNext,Obj} ->
						{ok,{Objs ++ [Obj],NewNext}};
					_ ->
						{ok,{Objs,Next}}
				end
		end,
	case util:for(1, Len, F,{[],Next}) of
		{ok,{GetObjs,AllNext}} ->
			{AllNext,GetObjs};
		_ -> 
			{Next,[]}
	end;
from_binary(<<Len:16/unsigned-integer,Next/binary>>,{list,int64}) ->
	F = fun(_I,{Objs,GetNext}) ->
				case from_binary(GetNext,int64) of
					{NewNext,Obj} ->
						{ok,{Objs ++ [Obj],NewNext}};
					_ ->
						{ok,{Objs,Next}}
				end
		end,
	case util:for(1, Len, F,{[],Next}) of
		{ok,{GetObjs,AllNext}} ->
			{AllNext,GetObjs};
		_ -> 
			{Next,[]}
	end;
from_binary(<<Len:16/unsigned-integer,Next/binary>>,{list,uint64}) ->
	F = fun(_I,{Objs,GetNext}) ->
				case from_binary(GetNext,uint64) of
					{NewNext,Obj} ->
						{ok,{Objs ++ [Obj],NewNext}};
					_ ->
						{ok,{Objs,Next}}
				end
		end,
	case util:for(1, Len, F,{[],Next}) of
		{ok,{GetObjs,AllNext}} ->
			{AllNext,GetObjs};
		_ -> 
			{Next,[]}
	end;
from_binary(<<Len:16/unsigned-integer,Next/binary>>,{list,string}) ->
	F = fun(_I,{Objs,GetNext}) ->
				case from_binary(GetNext,string) of
					{NewNext,Obj} ->
						{ok,{Objs ++ [Obj],NewNext}};
					_ ->
						{ok,{Objs,Next}}
				end
		end,
	case util:for(1, Len, F,{[],Next}) of
		{ok,{GetObjs,AllNext}} ->
			{AllNext,GetObjs};
		_ -> 
			{Next,[]}
	end;
from_binary(<<Len:16/unsigned-integer,Next/binary>>,{list,float}) ->
	F = fun(_I,{Objs,GetNext}) ->
				case from_binary(GetNext,float) of
					{NewNext,Obj} ->
						{ok,{Objs ++ [Obj],NewNext}};
					_ ->
						{ok,{Objs,Next}}
				end
		end,
	case util:for(1, Len, F,{[],Next}) of
		{ok,{GetObjs,AllNext}} ->
			{AllNext,GetObjs};
		_ -> 
			{Next,[]}
	end;
from_binary(<<Len:16/unsigned-integer,Next/binary>>,{list,FunName}) ->
%% 	FunName = erlang:list_to_atom(erlang:atom_to_list(ListName) ++ "_from_binary"),
	F = fun(_I,{Objs,GetNext}) ->
				case erlang:apply(pt_public_class, FunName, [GetNext]) of
					{ok,NewNext,Obj} ->
						{ok,{Objs ++ [Obj],NewNext}};
					_R -> 
						{ok,{Objs,GetNext}}
				end
		end,
	case util:for(1, Len, F,{[],Next}) of
		{ok,{GetObjs,AllNext}} ->
			{AllNext,GetObjs};
		_ -> 
			{Next,[]}
	end.

to_binary(Data,int8) when erlang:is_integer(Data) -> <<Data:8/signed-integer>>;
to_binary(Data,uint8) when erlang:is_integer(Data) -> <<Data:8/unsigned-integer>>;
to_binary(Data,int16) when erlang:is_integer(Data) -> <<Data:16/signed-integer>>;
to_binary(Data,uint16) when erlang:is_integer(Data) -> <<Data:16/unsigned-integer>>;
to_binary(Data,int32) when erlang:is_integer(Data) -> <<Data:32/signed-integer>>;
to_binary(Data,uint32) when erlang:is_integer(Data) -> <<Data:32/unsigned-integer>>;
to_binary(Data,int64) when erlang:is_integer(Data) -> <<Data:64/signed-integer>>;
to_binary(Data,uint64) when erlang:is_integer(Data) -> <<Data:64/unsigned-integer>>;
to_binary(Data,float) when erlang:is_number(Data) -> <<Data:32/float>>;
to_binary(Data,string) when erlang:is_binary(Data) -> 
	Bsize = to_binary(byte_size(Data),uint16),
	Bdata = Data,
	<<Bsize/binary,Bdata/binary>>;
to_binary(Data,string) when erlang:is_list(Data) -> 
	Bsize = to_binary(byte_size(erlang:list_to_binary(Data)),uint16),
	Bdata = erlang:list_to_binary(Data),
	<<Bsize/binary,Bdata/binary>>;
to_binary(Data,{list,int8}) ->
	Bsize = to_binary(erlang:length(Data),uint16),
	Fun = fun(Obj) -> to_binary(Obj,int8) end,
	Bdata = erlang:list_to_binary(lists:map(Fun, Data)),
	<<Bsize/binary,Bdata/binary>>;
to_binary(Data,{list,uint8}) ->
	Bsize = to_binary(erlang:length(Data),uint16),
	Fun = fun(Obj) -> to_binary(Obj,uint8) end,
	Bdata = erlang:list_to_binary(lists:map(Fun, Data)),
	<<Bsize/binary,Bdata/binary>>;
to_binary(Data,{list,int16}) ->
	Bsize = to_binary(erlang:length(Data),uint16),
	Fun = fun(Obj) -> to_binary(Obj,int16) end,
	Bdata = erlang:list_to_binary(lists:map(Fun, Data)),
	<<Bsize/binary,Bdata/binary>>;
to_binary(Data,{list,uint16}) ->
	Bsize = to_binary(erlang:length(Data),uint16),
	Fun = fun(Obj) -> to_binary(Obj,uint16) end,
	Bdata = erlang:list_to_binary(lists:map(Fun, Data)),
	<<Bsize/binary,Bdata/binary>>;
to_binary(Data,{list,int32}) ->
	Bsize = to_binary(erlang:length(Data),uint16),
	Fun = fun(Obj) -> to_binary(Obj,int32) end,
	Bdata = erlang:list_to_binary(lists:map(Fun, Data)),
	<<Bsize/binary,Bdata/binary>>;
to_binary(Data,{list,uint32}) ->
	Bsize = to_binary(erlang:length(Data),uint16),
	Fun = fun(Obj) -> to_binary(Obj,uint32) end,
	Bdata = erlang:list_to_binary(lists:map(Fun, Data)),
	<<Bsize/binary,Bdata/binary>>;
to_binary(Data,{list,int64}) ->
	Bsize = to_binary(erlang:length(Data),uint16),
	Fun = fun(Obj) -> to_binary(Obj,int64) end,
	Bdata = erlang:list_to_binary(lists:map(Fun, Data)),
	<<Bsize/binary,Bdata/binary>>;
to_binary(Data,{list,uint64}) ->
	Bsize = to_binary(erlang:length(Data),uint16),
	Fun = fun(Obj) -> to_binary(Obj,uint64) end,
	Bdata = erlang:list_to_binary(lists:map(Fun, Data)),
	<<Bsize/binary,Bdata/binary>>;
to_binary(Data,{list,string}) ->
	Bsize = to_binary(erlang:length(Data),uint16),
	Fun = fun(Obj) -> to_binary(Obj,string) end,
	Bdata = erlang:list_to_binary(lists:map(Fun, Data)),
	<<Bsize/binary,Bdata/binary>>;
to_binary(Data,{list,float}) ->
	Bsize = to_binary(erlang:length(Data),uint16),
	Fun = fun(Obj) -> to_binary(Obj,float) end,
	Bdata = erlang:list_to_binary(lists:map(Fun, Data)),
	<<Bsize/binary,Bdata/binary>>;
to_binary(Data,{list,FunName}) ->
%% 	FunName = erlang:list_to_atom(erlang:atom_to_list(ListName) ++ "_to_binary"),
	Bsize = to_binary(erlang:length(Data),uint16),
	Fun = fun(Obj) -> erlang:apply(pt_public_class, FunName, [Obj]) end,
	Bdata = erlang:list_to_binary(lists:map(Fun, Data)),
	<<Bsize/binary,Bdata/binary>>;
to_binary(_D,_T) ->
	erlang:error({badarg, _D,_T}).
	%?debug("******make_binary unmatch _T = ~p,_D = ~p******",[_T,_D]),<<>>.

make_pt(Data,ID,Seq) ->
	L = byte_size(Data) + 10,
	BL = to_binary(L,uint32),
	BId = to_binary(ID,uint16),
	BSeq = to_binary(Seq,uint32),
	<<BL/binary,BId/binary,BSeq/binary,Data/binary>>.

-module(lib_map_module).
-include("map.hrl").
-include("log.hrl").
%-include("common.hrl").

-export([set_module/1,get_module/0,check_point/1,check_dir/3,check_dir_by_point/2]).
-export([get_point/1]).


get_module() -> get(map_moudle).
set_module(Name) -> put(map_moudle,Name).
call_module(Fun,Args) -> 
	Module = get(map_moudle),
	erlang:apply(Module, Fun, Args).
get_point(Index) -> call_module(get_point,[Index]).
get_line(Index) -> call_module(get_line,[Index]).
get_face(Index) -> call_module(get_face,[Index]).
get_divide_info() -> call_module(get_divide_info,[]).
get_divide(Index) -> call_module(get_divide,[Index]).

check_point(Point) ->
	{PointFrom,PointTo,PerPoint} = get_divide_info(),
%% 	?debug("{PointFrom,PointTo,PerPoint} = ~p",[{PointFrom,PointTo,PerPoint}]),
	DivideIndex = get_index(Point,PointFrom,PointTo,PerPoint),
%% 	?debug("DivideIndex = ~p",[DivideIndex]),
	case get_divide(DivideIndex) of
		no -> false;
		FaceIndexs ->
%% 			?debug("FaceIndexs = ~p",[FaceIndexs]),
			find_face(Point,FaceIndexs)			
	end.
%% check_line(Point1,Point2) ->
%% 	case check_point(Point1) of
%% 		false -> false;
%% 		{true,FaceIndex1,CrossPoint1} ->
%% 			case check_point(Point2) of
%% 				false -> false;
%% 				{true,FaceIndex2,CrossPoint2} ->
%% 					if
%% 						FaceIndex1 == FaceIndex2 -> {true,FaceIndex1,{CrossPoint1,CrossPoint2}};
%% 						true -> 
%% 							case is_connect_face(FaceIndex1,FaceIndex2) of
%% 								true -> 
%% 									case check_in_polygon(Point1,FaceIndex2) of
%% 										{true,CrossPoint} -> {true,FaceIndex2,{CrossPoint,CrossPoint2}};
%% 										_ -> 
%% 											case check_in_polygon(Point2,FaceIndex1) of
%% 												{true,CrossPoint} -> {true,FaceIndex1,{CrossPoint1,CrossPoint}};
%% 												_ -> false
%% 											end
%% 									end;
%% 								_ -> false
%% 							end
%% 					end
%% 			end
%% 	end.

%%用于选择正方向有交点的线 dec_out -> start_point -> in -> end_point -> add_out
test_lines(_Point,_Dir,[]) -> no;
test_lines(Point,Dir,[This | Next]) ->
	Line = get_line(This),
	case test_dir_and_points(Point,Dir,get_point(Line#map_line.point1),get_point(Line#map_line.point2)) of		
		{ok,dec_out,_} -> test_lines(Point,Dir,Next);
		{ok,start_point,_} -> test_lines(Point,Dir,Next);
		{ok,_,GetPoint} -> {ok,This,GetPoint};
		_ -> test_lines(Point,Dir,Next)
	end.

%%ret:
%%end_no_in_scene 终点不在场景
%%{error,R} 出错
%%{find,NeedDis,ToPointCrossPoint} 找到了终点
%%{skip,ThisDis,NextPoint} 不能继续下去了
check_dir_by_point(Point,ToPoint) when is_record(Point,map_point) andalso is_record(ToPoint,map_point) ->
	Dir1 = tool_vect:dec(ToPoint, Point),
	Dir = Dir1#map_point{y=0},
	case tool_vect:is_null(Dir) of
		true -> {error,no_dis};
		_ ->			
			check_dir_by_point(Point,Dir,ToPoint,no,Point,tool_vect:length(tool_vect:dec(ToPoint#map_point{y=0}, Point#map_point{y=0})))
	end;
check_dir_by_point(_,_) -> {error,bad_args}.
check_dir_by_point(Point,Dir,ToPoint,FromFace,FromPoint,NeedDis) ->
%% 	?debug("Point,Dir,ToPoint,FromFace,FromPoint,NeedDis = ~p",[{Point,Dir,ToPoint,FromFace,FromPoint,NeedDis}]),
	case find_dir_point(FromPoint,Dir,FromFace) of
		{CanCantinue,NextFace,NextPoint} ->
%% 			?debug("CanCantinue,NextFace,NextPoint = ~p",[{CanCantinue,NextFace,NextPoint}]),
			ThisDis = tool_vect:length(tool_vect:dec(NextPoint#map_point{y = 0},Point#map_point{y = 0})),
%% 			?debug("ThisDis = ~p",[{ThisDis}]),
			if
				ThisDis >= NeedDis -0.01 ->
					case get_point_adjust(ToPoint,Dir,-0.01) of
						{true,_ToPointFaceIndex,ToPointCrossPoint} -> 
%% 							?debug("ToPointFaceIndex,ToPointCrossPoint = ~p",[{ToPointFaceIndex,ToPointCrossPoint}]),
							{find,NeedDis,ToPointCrossPoint};
						_ -> 
%% 							?debug("check_point(NoYLinePoint) ret no why?"),
							end_no_in_scene
					end;
				true ->
					case CanCantinue of
						true -> check_dir_by_point(Point,Dir,ToPoint,NextFace,NextPoint,NeedDis);
						_ -> {skip,ThisDis,NextPoint}
					end
			end;
		R -> 
%% 			?debug("find_dir_point error,R = ~p",[{R}]),
			{error,R}
	end.

%%ret:
%%end_no_in_scene 终点不在场景
%%{error,R} 出错
%%{find,NeedDis,ToPointCrossPoint} 找到了终点
%%{skip,ThisDis,NextPoint} 不能继续下去了
check_dir(Point,Dir,MaxDis)  when is_record(Point,map_point) -> 
	if
		MaxDis < 0.01 -> {error,no_dis};
		true ->
			check_dir(Point,Dir,MaxDis,no,Point)
	end;
check_dir(_,_,_) -> {error,bad_args}.
check_dir(Point,Dir,MaxDis,FromFace,FromPoint) ->
%% 	?debug("Point,Dir,MaxDis,FromFace,FromPoint = ~p",[{Point,Dir,MaxDis,FromFace,FromPoint}]),
	case find_dir_point(FromPoint,Dir,FromFace) of
		{CanCantinue,NextFace,NextPoint} ->
%% 			?debug("CanCantinue,NextFace,NextPoint = ~p",[{CanCantinue,NextFace,NextPoint}]),
			ThisDis = tool_vect:length(tool_vect:dec(NextPoint#map_point{y = 0},Point#map_point{y = 0})),
%% 			?debug("ThisDis = ~p",[{ThisDis}]),
			if
				ThisDis >= MaxDis -0.01 ->
					NoYMaxDisPoint = tool_vect:add(Point#map_point{y = 0},
					 tool_vect:ride(tool_vect:normal(Dir#map_point{y = 0}), MaxDis)),
					case get_point_adjust(NoYMaxDisPoint,Dir,-0.01) of
						{true,_ToPointFaceIndex,ToPointCrossPoint} -> 
%% 							?debug("ToPointFaceIndex,ToPointCrossPoint = ~p",[{_ToPointFaceIndex,ToPointCrossPoint}]),
							{find,MaxDis,ToPointCrossPoint};
						_ -> 
%% 							?debug("check_point(NoYLinePoint) ret no why?"),
							end_no_in_scene
					end;
				true ->
					case CanCantinue of
						true -> check_dir(Point,Dir,MaxDis,NextFace,NextPoint);
						_ -> {skip,ThisDis,NextPoint}
					end
			end;
		R -> 
%% 			?debug("find_dir_point error,R = ~p",[{R}]),
			{error,R}
	end.

%% is_connect_face(FaceIndex1,FaceIndex2) ->
%% 	case get_face(FaceIndex1) of
%% 		#map_face{point1 = P1,point2 = P2,point3 = P3} ->
%% 			case get_line(make_line_index(P1,P2)) of
%% 				#map_line{connect_face1 = FaceIndex2} -> true;
%% 				#map_line{connect_face2 = FaceIndex2} -> true;
%% 				_ ->
%% 					case get_line(make_line_index(P2,P3)) of
%% 						#map_line{connect_face1 = FaceIndex2} -> true;
%% 						#map_line{connect_face2 = FaceIndex2} -> true;
%% 						_ ->
%% 							case get_line(make_line_index(P1,P3)) of
%% 								#map_line{connect_face1 = FaceIndex2} -> true;
%% 								#map_line{connect_face2 = FaceIndex2} -> true;
%% 								_ -> false
%% 							end
%% 					end
%% 			end;
%% 		_ -> false
%% 	end.

%% find_connect_face(FaceIndex,LineIndex) ->
%% 	case get_line(LineIndex) of
%% 		#map_line{connect_face1 = FaceIndex,connect_face2 = OtherFaceIndex} -> OtherFaceIndex;
%% 		#map_line{connect_face2 = FaceIndex,connect_face1 = OtherFaceIndex} -> OtherFaceIndex;
%% 		_ -> no
%% 	end.
					

find_face(_,[]) -> false;
find_face(Point,[This | Next]) -> 
	case check_in_polygon(Point,This) of
		{true,CrossPoint} -> {true,This,CrossPoint};
		_ -> find_face(Point,Next)
	end.


check_in_polygon(Point,FaceIndex) ->
	case check_line_and_face(Point,tool_vect:add(Point,#map_point{x = 0 , y = 1 , z = 0}),FaceIndex) of
		{ok,_,CrossPoint} ->
			case check_point_in_face(CrossPoint,FaceIndex) of
				true -> {true,CrossPoint};
				_ -> false
			end;
		_ -> false
	end.

check_line_and_face(Point1,Point2,FaceIndex) ->
	Face = get_face(FaceIndex),
	A = get_point(Face#map_face.point1),
	B = get_point(Face#map_face.point2),
	C = get_point(Face#map_face.point3),
	tool_vect:check_line_and_face(Point1,Point2,A,B,C).

check_point_in_face(Point,FaceIndex) ->
	Face = get_face(FaceIndex),
	A = get_point(Face#map_face.point1),
	B = get_point(Face#map_face.point2),
	C = get_point(Face#map_face.point3),
	tool_vect:check_point_in_face(Point,A,B,C).



%% make_face_index(P1,P2,P3) when P1 == P2 orelse P2 == P3 orelse P1 == P3 -> error;
%% make_face_index(P1,P2,P3) when P1 < P2 andalso P2 < P3 -> {P1,P2,P3};
%% make_face_index(P1,P2,P3) when P1 > P2 -> make_face_index(P2,P1,P3);
%% make_face_index(P1,P2,P3) when P2 > P3 -> make_face_index(P1,P3,P2);
%% make_face_index(_,_,_) -> error.

make_line_index(P1,P2) when P1 == P2 -> error;
make_line_index(P1,P2) when P1 < P2 -> {P1,P2};
make_line_index(P1,P2) when P1 > P2 -> make_line_index(P2,P1);
make_line_index(_,_) -> error.

get_index(Point,DFrom,DTo,DPer) ->
	X = get_data_index_floor(Point#map_point.x,DFrom#map_point.x,DTo#map_point.x,DPer#map_point.x),
	Y = 0,%%get_data_index_floor(Point#map_point.y,DFrom#map_point.y,DTo#map_point.y,DPer#map_point.y),
	Z = get_data_index_floor(Point#map_point.z,DFrom#map_point.z,DTo#map_point.z,DPer#map_point.z),
	{X,Y,Z}.

get_data_index_floor(_Data,_From,_To,0) -> 0;
get_data_index_floor(_Data,_From,_To,0.0) -> 0;
get_data_index_floor(Data,From,_To,Per) -> 
	util:floor((Data - From) / Per).

%% get_data_index_ceil(_Data,_From,_To,0) -> 0;
%% get_data_index_ceil(_Data,_From,_To,0.0) -> 0;
%% get_data_index_ceil(Data,From,_To,Per) -> 
%% 	Value = ceil((Data - From) / Per) - 1,
%% 	if
%% 		Value < 0 -> 0;
%% 		true -> Value
%% 	end.

test_dir_and_points(Point,Dir,Point1,Point2)->
	NoYPoint = Point#map_point{y = 0},
	NoYDir = Dir#map_point{y = 0},
	NoYPoint1 = Point1#map_point{y = 0},
	NoYPoint2 = Point2#map_point{y = 0},
	NoYPoint3 = tool_vect:add(NoYPoint1,#map_point{x = 0 , y = 1, z = 0}),
%% 	?debug("NoYPoint,NoYDir,NoYPoint1,NoYPoint2 = ~p",[{NoYPoint,NoYDir,NoYPoint1,NoYPoint2}]),
	case tool_vect:check_line_and_face(NoYPoint,tool_vect:add(NoYPoint,NoYDir),NoYPoint1,NoYPoint2,NoYPoint3) of
		{ok,Dot,CrossPoint} ->
			case tool_vect:check_point_in_face(CrossPoint,NoYPoint1,NoYPoint2,NoYPoint3) of
				true -> {ok,Dot,CrossPoint};
				_ -> no
			end;
		_ -> no
	end.

get_point_adjust(Point,Dir,Award) ->
	case check_point(Point) of
		false -> get_point_award(Point,Dir,Award);
		R -> R
	end.
get_point_award(Point,Dir,Award) ->
	NoYPoint = Point#map_point{y = 0},
	NoYDir = Dir#map_point{y = 0},
	NoYAwardPoint = tool_vect:add(NoYPoint,tool_vect:ride(tool_vect:normal(NoYDir),Award)),
	check_point(NoYAwardPoint).

find_dir_point(Point,Dir,Face) ->
%% 	?debug("find_dir_point Point,Dir,Face = ~p",[{Point,Dir,Face}]),
	%%找起点和起点面
	{FromFace,FromPoint} = case Face of
		no -> 
			case get_point_adjust(Point,Dir,0.1) of
				false -> {no,no};
				{true,FaceIndex,CrossPoint} -> {FaceIndex,CrossPoint}
			end;
		_ -> {Face,Point}
	end,
%% 	?debug("FromFace,FromPoint = ~p",[{FromFace,FromPoint}]),
	case FromFace of
		no -> {no_from_face,Point};
		_ -> 
			case get_point_award(FromPoint,Dir,0.1) of
				{true,FromFace,_CrossPoint1} -> %%往前还是当前面，需要射线测试
%% 					?debug("CrossPoint1 = ~p",[CrossPoint1]),
					case get_face(FromFace) of
						#map_face{point1 = P1,point2 = P2,point3 = P3} ->
							Lines = [make_line_index(P1,P2),make_line_index(P2,P3),make_line_index(P1,P3)],
%% 							?debug("Lines = ~p",[Lines]),
							case test_lines(FromPoint,Dir,Lines) of								
								{ok,_CrossLine,NoYLinePoint} -> %%射线测试发现交点
%% 									?debug("test_lines ,_CrossLine = ~p,NoYLinePoint = ~p",[_CrossLine,NoYLinePoint]),
									case get_point_award(NoYLinePoint,Dir,0.1) of
										{true,NewFace,NewCrossPoint} -> %%有交点，交点后面还能继续走
%% 											?debug("NewFace,NewCrossPoint = ~p",[{NewFace,NewCrossPoint}]),
											{true,NewFace,NewCrossPoint};
										_ -> 
%% 											?debug("get_point_award no face"),
											case check_point(NoYLinePoint) of
												{true,_,LinePoint} -> 
%% 													?debug("LinePoint = ~p",[LinePoint]),
													{false,FromFace,LinePoint};
												_ -> 
													case get_point_award(NoYLinePoint,Dir, - 0.1) of
														{true,NewFace1,NewCrossPoint1} -> 
%% 															?debug("NewFace1,NewCrossPoint1 = ~p",[{NewFace1,NewCrossPoint1}]),
															{false,NewFace1,NewCrossPoint1};
														_ ->
%% 															?debug("check_point(NoYLinePoint) ret no why?"),
															{check_point_ret_no,NoYLinePoint}
													end
											end
									end;
								_R -> 
%% 									?debug("test_lines cannot find why?"),
									{test_lines_ret,{FromPoint,Dir,Lines,_R}}
							end;
						_R1 -> 
%% 							?debug("get_face cannot find why?"),
							{get_face_ret_no,{FromFace,_R1}}
					end;
				{true,NextFace,NextCrossPoint} -> %%后面是新的面
%% 					?debug("NextFace,NextCrossPoint = ~p",[{NextFace,NextCrossPoint}]),
					{true,NextFace,NextCrossPoint};
				_ -> 
					%%往前不能走，就算到最后的点
					case check_point(FromPoint) of
						{true,CheckFromFace,CheckCrossPoint} -> {false,CheckFromFace,CheckCrossPoint};
						R -> 
							%%还不能走
							{get_point_award_error,R}
					end
			end
	end.

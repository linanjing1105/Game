-module(tool_vect).
-include("common.hrl").

-export([radian2angle/1,angle2radian/1,is_null/1]).
-export([add/2,dec/2,ride/2,dot_ride/2,cross_ride/2,length/1,length_power/1,normal/1]).
-export([check_line_and_face/5,check_point_in_face/4]).
-export([get_radian/2,rotate_radian/2,get_vect_by_dir/1,dot_line_dis/2,get_dir_angle/1,to_map_point/1,to_point/1]).

-define(PI,3.1415926535897932).
-define(PI2, (?PI * 2)).

to_map_point(MapPoint = #map_point{}) -> MapPoint;
to_map_point({X,Y,Z}) -> #map_point{x = X,y = Y,z = Z}.
to_point({X,Y,Z}) -> {X,Y,Z};
to_point(#map_point{x = X,y = Y,z = Z}) -> {X,Y,Z}.

radian2angle(R) when R >= ?PI2 -> radian2angle(R - util:floor(R / ?PI2) * ?PI2);
radian2angle(R) when R < 0 -> radian2angle(R + util:ceil( (-1 * R) / ?PI2) * ?PI2);
radian2angle(R) -> R / ?PI2 * 360.

angle2radian(R) when R >= 360 -> angle2radian(R - util:floor(R / 360) * 360);
angle2radian(R) when R < 0 -> angle2radian(R + util:ceil( (-1 * R) / 360) * 360);
angle2radian(R) -> R / 360 * ?PI2.

is_null(#map_point{x = X,y = Y,z = Z})when X == 0 andalso Y == 0  andalso Z == 0  -> true;
is_null(_) -> false.

add(#map_point{x = X1,y = Y1,z = Z1},#map_point{x = X2,y = Y2,z = Z2}) -> #map_point{x = X1 + X2,y = Y1 + Y2,z = Z1 + Z2};
add({X1,Y1,Z1},{X2,Y2,Z2}) -> {X1 + X2,Y1 + Y2,Z1 + Z2}.
dec(#map_point{x = X1,y = Y1,z = Z1},#map_point{x = X2,y = Y2,z = Z2}) -> #map_point{x = X1 - X2,y = Y1 - Y2,z = Z1 - Z2};
dec({X1,Y1,Z1},{X2,Y2,Z2}) -> {X1 - X2,Y1 - Y2,Z1 - Z2}.
ride(#map_point{x = X1,y = Y1,z = Z1},Data) -> #map_point{x = X1 * Data,y = Y1 * Data,z = Z1 * Data};
ride({X1,Y1,Z1},Data) -> {X1 * Data,Y1 * Data,Z1 * Data}.
dot_ride(#map_point{x = X1,y = Y1,z = Z1},#map_point{x = X2,y = Y2,z = Z2}) -> X1 *X2 + Y1 * Y2 + Z1 * Z2;
dot_ride({X1,Y1,Z1},{X2,Y2,Z2}) -> X1 *X2 + Y1 * Y2 + Z1 * Z2.
cross_ride(#map_point{x = X1,y = Y1,z = Z1},#map_point{x = X2,y = Y2,z = Z2}) -> #map_point{x = Y1 * Z2 - Z1 * Y2,
																							y = Z1 * X2 - X1 * Z2,
																							z = X1 * Y2 - Y1 * X2};
cross_ride({X1,Y1,Z1},{X2,Y2,Z2}) -> {Y1 * Z2 - Z1 * Y2,
									  Z1 * X2 - X1 * Z2,
									  X1 * Y2 - Y1 * X2}.
	
length(#map_point{x = X,y = Y,z = Z})-> math:sqrt(X*X + Y*Y + Z*Z);
length({X,Y,Z}) -> math:sqrt(X*X + Y*Y + Z*Z).
length_power(#map_point{x = X,y = Y,z = Z})-> X*X + Y*Y + Z*Z;
length_power({X,Y,Z}) -> X*X + Y*Y + Z*Z.
normal(Point) -> ride(Point,1/tool_vect:length(Point)).

%%ret
%% {parallel,in} 平行点在面上
%% {parallel,out} 平行点不在面上
%% {ok,Dot,Point} 有交点,dot:dec_out -> start_point -> in -> end_point -> add_out
check_line_and_face(LinePoint1,LinePoint2,FacePoint1,FacePoint2,FacePoint3) ->
	Pop = FacePoint1,
	Nop = cross_ride(dec(FacePoint2,FacePoint1),dec(FacePoint3,FacePoint1)),
	
	U = dec(LinePoint2,LinePoint1),
	W = dec(LinePoint1,Pop),
	
	D = dot_ride(Nop,U),
	N = -1 * dot_ride(Nop,W),
	
	ADSD = abs(D),
	if
		ADSD < 0.0001 ->
			if
				N == 0 -> {parallel,in};
				true -> {parallel,out}
			end;
		true ->
			T = N / D,
			Dot = if
					 T < 0 -> dec_out;
					 T == 0 -> start_point;
					 T == 1 -> end_point;
					 T > 1 -> add_out;
					 true -> in
				  end,
			{ok,Dot,add(LinePoint1,ride(dec(LinePoint2,LinePoint1),T))}
	end.
check_point_in_face(Point,FacePoint1,FacePoint2,FacePoint3) ->
	V0 = dec(FacePoint3,FacePoint1),
	V1 = dec(FacePoint2,FacePoint1),
	V2 = dec(Point,FacePoint1),
	Dot00 = dot_ride(V0,V0),
	Dot01 = dot_ride(V0,V1),
	Dot02 = dot_ride(V0,V2),
	Dot11 = dot_ride(V1,V1),
	Dot12 = dot_ride(V1,V2),
	
	InverDeno = 1 / (Dot00 * Dot11 - Dot01 * Dot01),
	U = (Dot11 * Dot02 - Dot01 * Dot12) * InverDeno,
	if
		U < 0 -> false;
		U > 1 -> false;
		true ->
			V = (Dot00 * Dot12 - Dot01 * Dot02) * InverDeno,
			if
				V < 0 -> false;
				V > 1 -> false;
				true ->
					if
						U + V =< 1 -> true;
						true -> false
					end
			end
	end.


get_radian(V1,V2) ->
	NOYV1 = V1#map_point{y = 0},
	NOYV2 = V2#map_point{y = 0},
	Null1 = is_null(NOYV1),
	Null2 = is_null(NOYV2),
	if
		Null1 == false andalso Null2 == false -> 
			R = dot_ride(V1,V2) / (tool_vect:length(V1) * tool_vect:length(V2)),
			RR = if
					 R > 1 -> 1;
					 R < -1 -> -1;
					 true -> R
				 end,
			math:acos(RR);
		true -> 0
	end.

rotate_radian(#map_point{x = X,y = Y,z = Z},RY) -> #map_point{x = X * math:cos(RY) + Z * math:sin(RY),y = Y,z = Z * math:cos(RY) - X * math:sin(RY)}.

%% s3d_rotate_radian({X,Y,Z},{RX,RY,RZ})-> 
%% 	%%x
%% 	{Y1,Z1} = s2d_rotate_radian({Y,Z},RX),
%% 	%%y
%% 	{X1,Z2} = s2d_rotate_radian({X,Z1},RY),
%% 	%%z
%% 	{X2,Y2} = s2d_rotate_radian({X1,Y1},RZ),
%% 	{X2,Y2,Z2}.
%% s2d_rotate_radian({X,Y},R) ->  {X * math:cos(R) + Y * math:sin(R),Y * math:cos(R) - X * math:sin(R)}.

get_vect_by_dir(R) -> rotate_radian(#map_point{x = 0,y = 0,z = 1},R).

dot_line_dis(#map_point{x = X,z = _Z},#map_point{x = PX,z = _PZ}) when X == 0 -> util:abs(PX);
dot_line_dis(#map_point{x = _X,z = Z},#map_point{x = _PX,z = PZ}) when Z == 0 -> util:abs(PZ);
dot_line_dis(#map_point{x = LX,z = LZ},#map_point{x = PX,z = PZ}) -> 
	try
		util:abs((LZ / LX) * PX - PZ) / tool_vect:length(#map_point{x = (LZ / LX),y = 0,z = 1})
	catch _E:_R -> ?log_error("dot_line_dis l=~p,p=~p",[{LX,LZ},{PX,PZ}]),0
	end.

get_dir_angle(V = #map_point{x = X}) ->
	A1 = radian2angle(get_radian(#map_point{x = 0,y = 0,z = 1},V)),
	if
		X < 0 -> 360 - A1;
		true -> A1
	end.

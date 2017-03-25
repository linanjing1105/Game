-ifndef('__MAP_H__').
-define('__MAP_H__', true).


-record(map_info,{points,lines,faces,dict}).
-record(map_point,{x,y,z}).
-record(map_line,{point1,point2,centre_point,connect_face1 = null,connect_face2 = null}).
-record(map_face,{point1,point2,point3,mini_point,max_point}).



-endif. %%-ifndef('__MAP_H__').
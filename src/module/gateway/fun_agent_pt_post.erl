-module(fun_agent_pt_post).

-export([fill/1,fill_pt/1]).
%% 选择消息转发进程
fill(<<Id:16/unsigned-integer,_Binary/binary>>) -> fill_pt(Id).


fill_pt(16#A001) -> login;
fill_pt(16#A006) -> login;
fill_pt(16#A003) -> login;
fill_pt(16#B001) -> login;
fill_pt(16#C001) -> scene;
fill_pt(16#C004) -> scene;
fill_pt(16#C005) -> scene;
fill_pt(16#C010) -> scene;
fill_pt(16#C012) -> scene;
fill_pt(16#C013) -> scene;
fill_pt(16#C019) -> scene;
fill_pt(16#C01A) -> scene;
fill_pt(16#D214) -> scene;
fill_pt(16#D134) -> scene;
fill_pt(16#D47A) -> scene;
fill_pt(16#D610) -> scene;
fill_pt(16#D620) -> scene;
fill_pt(16#D630) -> scene;
fill_pt(16#D632) -> scene;
fill_pt(16#D636) -> scene;
fill_pt(16#D637) -> scene;
fill_pt(16#D01F) -> scene;
fill_pt(16#D025) -> agent_mng;
fill_pt(16#D549) -> agent_mng;
fill_pt(16#D551) -> agent_mng;
fill_pt(16#D552) -> agent_mng;
fill_pt(16#D553) -> agent_mng;
fill_pt(16#D600) -> agent_mng;
fill_pt(16#DA00) -> agent_mng;


%% fill_pt(16#BB03) -> {post,agent_mng};
fill_pt(_Id) -> agent.


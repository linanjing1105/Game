-ifndef('__DB_H__').
-define('__DB_H__', true).
-define(DB, sd_mysql_conn).
-define(DB_S, sd_mysql_conn_s).

-ifdef(debug).
-define(SQLCONNUM,5).
-else.
-define(SQLCONNUM,20).
-endif.

-ifdef(debug).
-define(SQLSCONNUM,1).
-else.
-define(SQLSCONNUM,12).
-endif.

-define(DBM_WORK_NAME, "work_dbm").
-define(DBM_WORK_MAX, 40).
-define(WORK_TIME, (1000)).
-define(INFO_WORK_NAME, "work_info").
-define(INFO_WORK_MAX, 10).


-record(config, {key, value}).
-record(sql_config, {key, sql=""}).%key={Tab,Fun}
-record(sql_key_static, {key, max=0}).%key=Tab
-record(sql_data, {key, opType=add,rec={}}).        %key={Tab,id} opType=change,add,del


-endif.  %% -ifndef('__DB_H__').




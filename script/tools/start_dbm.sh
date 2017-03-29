#！bin/bash
erl -boot dbm  -mnesia dir '"Data"' -config cnf -pa ../../ebin 

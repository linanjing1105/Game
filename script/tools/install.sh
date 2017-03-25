#! /bin/bash

erl -name dbm_ch@192.168.199.88 -setcookie ch_girl_ready -mnesia dir '"../Data"' -pa ../../ebin

PROJECT = mm

DEPS = cowboy gproc jiffy
dep_cowboy = pkg://cowboy master
dep_gproc = https://github.com/uwiger/gproc
dep_jiffy = https://github.com/davisp/jiffy

include erlang.mk

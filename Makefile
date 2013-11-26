PROJECT = mm

DEPS = cowboy gproc jiffy erlport
dep_cowboy = pkg://cowboy 0.9.0
dep_gproc = https://github.com/uwiger/gproc
dep_jiffy = https://github.com/davisp/jiffy 0.8.5
dep_erlport = https://github.com/hdima/erlport

include erlang.mk
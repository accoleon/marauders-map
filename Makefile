PROJECT = mm
PLT_APPS = mnesia

DEPS = cowboy gproc jsx
dep_cowboy = pkg://cowboy 0.9.0
dep_gproc = https://github.com/uwiger/gproc
dep_jsx = pkg://jsx

include erlang.mk
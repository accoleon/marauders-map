Marauder's Map
==============

Prerequisites
-------------
`erlang.mk` requires GNU make and expects to be ran in a standard unix environment with Erlang installed and in the `$PATH`.
`erlang.mk` uses `wget` for downloading the package index file when the `pkg://` scheme is used.
`git` is in the `$PATH`.

Setup
--------------------
To build Marauder's Map, run the following command:

	make

To start the release in the foreground:

	$./_rel/bin/mm console

Then point your browser to http://localhost:8080

{application, mmserver,
 [
  {description, "Marauder's Map Server"},
  {vsn, "1"},
  {modules, [analyzer, capturenode, mmserver_app, mmserver_sup, mmserver_ws_handler, servernode]},
  {registered, [servernode, mmserver_sup]},
  {applications, [
  	kernel,
	stdlib,
	cowboy
  ]},
  {mod, { mmserver_app, []}},
  {env, []}
 ]}.

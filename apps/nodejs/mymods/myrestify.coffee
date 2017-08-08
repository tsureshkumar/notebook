restify = require("restify")
server = restify.createServer()
coffee = require('coffee-script')
handlers = require('./handlers')
ecstatic = require ('ecstatic')
nstatic = require('node-static')

file = new nstatic.Server('');


exports.server = server
exports.restify = restify


respond = (req, res, next) ->
    res.send "Hello #{req.params.name}"
server.get '/hello/:name', respond

server.get /^\/.*/, (r,s,n) ->
	console.log(r.headers)
	file.serve(r, s, (err) -> 
		if (err) 
			throw err
		console.log ('next')
		n()
	)

   
server.listen 8282, ->
    console.log "%s listening at %s", server.name, server.url













#server.use(ecstatic({ root: './' , showDir: true, baseDir: "/" }));
#server.get "/hello/:name", respond
#server.get /.*/, restify.serveStatic({'directory':'./public'})
#server.get(/\/.*/, restify.serveStatic({
#  directory: '.'
#}));
#server.get "/dir.*", (r,s,n) ->
#	s.send "sss"

#handlers.register server


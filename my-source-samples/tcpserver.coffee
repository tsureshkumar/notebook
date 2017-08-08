net = require 'net'
_ = require 'underscore'
fs = require 'fs'

{EventEmitter} = require 'events'

readline = (socket) ->
        e = new EventEmitter
        partial = ''
        data = []
        socket.on 'data', (d) ->
                str = d.toString()
                if !str
                        return
                lines = str.split /\r*\n/
                if _.last(lines) != ''
                        partial += _.last(lines)
                lines = _.first(lines, lines.length - 1)
                _(lines).map (x) ->
                        e.emit 'line', x
        socket.on 'end', () ->
                if partial != ''
                        e.emit 'line', partial
        return e

process = (k,v) ->
        fs.appendFile k, v+"\n"

server = net.createServer (socket) ->
        le = readline(socket)
        key = null
        value = null
        le.on 'line', (x) ->
                value = if key then x else null
                if key && value
                        process(key, value)
                        key = value = null
                else if !value
                        key = x



server.listen 9001, 'localhost'
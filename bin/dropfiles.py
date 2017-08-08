#!/usr/local/env python

import pygtk,sys
pygtk.require('2.0')
import gtk

def motion_cb(wid, context, x, y, time):
    context.drag_status(gtk.gdk.ACTION_COPY, time)
    return True

def drop_cb(wid, context, x, y,data, info, time):
    if data and data.format == 8:
        for uri in data.data.split('\r\n')[:-1]:
            print uri[7:]
            sys.stdout.flush()
    context.finish(True, False, time)
    return True

w = gtk.Window()
w.set_size_request(200, 150)
w.drag_dest_set(gtk.DEST_DEFAULT_ALL, [('text/uri-list',0,0)], gtk.gdk.ACTION_COPY)
w.connect('drag_motion', motion_cb)
w.connect('drag_data_received', drop_cb)
w.connect('destroy', lambda w: gtk.main_quit())
l = gtk.Label()
w.add(l)
w.show_all()

gtk.main()

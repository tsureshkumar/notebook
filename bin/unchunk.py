#!/usr/bin/env python
from __future__ import print_function
import re, sys,os
from functools import reduce
import gzip, zlib

def warning(*objs):
    print("WARNING: ", *objs, file=sys.stderr)
def debug(*objs):
    print("DEBUG: ", *objs, file=sys.stderr)

infile=sys.argv[1]
outfile=None if len(sys.argv) < 3 else sys.argv[2]

f = open(infile, "rb")
b = f.read()
f.close()

lines = b.split(b"\r\n")
def splitter(x,y):
	[h,b] = x
	if len(y) == 0 or len(b) != 0:
		b.append(y)
	else: 
		h.append(y)		
	return [h,b]
[header, [dummy, *body]] = reduce(splitter, lines, [[],[]])

chunked = reduce(lambda x,y: x or re.match(b"Transfer-Encoding.*:.*chunked.*", y), header, False)
gzip = reduce(lambda x,y: x or re.match(b"Content-Encoding.*:.*gzip.*", y), header, False)
ctype = [ r.group(1) for r in list(map(lambda x: re.match(b"Content-Type\s*:\s*(.*)", x), header)) if r]
ctype = ctype[0] if ctype else None
ctype = ctype.split(b";")[0] if ctype else None
debug(ctype)
debug(body)

temp = []
if chunked:
	l = None
	for line in body:
		if not l or l == 0:
			l = 0 if line == b'' else int(line, 16)
			debug("length %d" % l)
		else:
			#sys.stdout.buffer.write(line)
			temp.append(line)
			l = l - len(line)
			if l != 0:
				temp.append(b"\r\n")
				l = l - 2
	body = temp

body = b''.join(body)
debug(body)
if gzip:
	body = zlib.decompress(body, 15 + 32)	

# writing output
m = { b"application/javascript": "js",
      b"application/json": "json",	
      b"text/javascript": "js",	
      b"text/html": "html",	
      b"text/xml": "xml",	
      b"image/gif": "gif",	
      b"image/x-icon": "ico",	
      b"image/jpeg": "gif",	
      b"image/png": "gif",	
      b"text/css": "gif"	}
if not outfile: 
	sys.stdout.buffer.write(body)
else:
	if outfile == "auto":
		filename = "%s.%s" % (infile, m.get(ctype, "unknown"))
		f = open(filename, "wb")
		f.write(body)
		f.close()

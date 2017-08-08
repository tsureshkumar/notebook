import pcap
import sys
import string
import time
import socket
import struct

protocols={socket.IPPROTO_TCP:'tcp',
           socket.IPPROTO_UDP:'udp',
           socket.IPPROTO_ICMP:'icmp'}

def decode_ip_packet(s):
  d={}
  d['version']=(ord(s[0]) & 0xf0) >> 4
  d['header_len']=ord(s[0]) & 0x0f
  d['tos']=ord(s[1])
  d['total_len']=socket.ntohs(struct.unpack('H',s[2:4])[0])
  d['id']=socket.ntohs(struct.unpack('H',s[4:6])[0])
  d['flags']=(ord(s[6]) & 0xe0) >> 5
  d['fragment_offset']=socket.ntohs(struct.unpack('H',s[6:8])[0] & 0x1f)
  d['ttl']=ord(s[8])
  d['protocol']=ord(s[9])
  d['checksum']=socket.ntohs(struct.unpack('H',s[10:12])[0])
  d['source_address']=pcap.ntoa(struct.unpack('i',s[12:16])[0])
  d['destination_address']=pcap.ntoa(struct.unpack('i',s[16:20])[0])
  if d['header_len']>5:
    d['options']=s[20:4*(d['header_len']-5)]
  else:
    d['options']=None
  d['data']=s[4*d['header_len']:]
  return d

def decode_tcp_packet(p):
    d = {}
    d['sport'] = socket.ntohs(struct.unpack('H',p[0:2])[0])
    d['dport'] = socket.ntohs(struct.unpack('H',p[2:4])[0])
    l = socket.ntohs(struct.unpack('H', p[12:14])[0])
    d['header_len'] = ((l & 0xF000) >> 12) * 4
    d['data'] = p[d['header_len']:]
    if len(d['data'])  == 0:
        d['data'] = None
    return d


def dumphex(s):
  bytes = map(lambda x: '%.2x' % x, map(ord, s))
  for i in xrange(0,len(bytes)/16):
    print '    %s' % string.join(bytes[i*16:(i+1)*16],' ')
  i = len(bytes) % 16
  print '    %s' % string.join(bytes[len(bytes)-i:],' ')

#class factory implementation from
#http://code.activestate.com/recipes/86900-factory-pattern/
#

class Factory:
    def register(self, methodName, constructor, *args, **kargs):
        """register a constructor"""
        _args = [constructor]
        _args.extend(args)
        setattr(self, methodName,apply(Functor,_args, kargs))

    def unregister(self, methodName):
        """unregister a constructor"""
        delattr(self, methodName)

class Functor:
    def __init__(self, function, *args, **kargs):
        assert callable(function), "function should be a callable obj"
        self._function = function
        self._args = args
        self._kargs = kargs

    def __call__(self, *args, **kargs):
        """call function"""
        _args = list(self._args)
        _args.extend(args)
        _kargs = self._kargs.copy()
        _kargs.update(kargs)
        return apply(self._function,_args,_kargs)

def take_byte(w):
    return (ord(w[0]), w[1:])
def take_int(w):
    return (socket.ntohs(struct.unpack('H', w[0:2])[0]), w[2:])
def take_string(w, len):
    return (struct.unpack("<%ds"%(len), w[0:len])[0], w[len+1:])
def take_lpstring(w):
    (len,w ) = take_int(w)
    if (len == 65535):
        return ("", 0, w[0:])
    return (struct.unpack("<%ds"%(len), w[0:len])[0], len, w[len+1:])
def take_boolean(w):
    (b,w) = take_byte(w)
    return (True if b == 1 else False, w)

class AJPType:
    predefined_headers = {
        0x01 : "accept",
        0x02 : "accept-charset",
        0x03 : "accept-encoding",
        0x04 : "accept-language",
        0x05 : "authorization",
        0x06 : "connection",
        0x07 : "content-type",
        0x08 : "conen-lengh",
        0x09 : "cookie",
        0x0A : "cookie2",
        0x0B : "host",
        0x0C : "pragma",
        0x0D : "referer",
        0x0E : "user-agent"
        }

    predefined_attrs = {
        0x01	: "?context",
        0x02	: "?servlet_path",
        0x03	: "?remote_user",
        0x04	: "?auth_type",
        0x05	: "?query_string",
        0x06	: "?route",
        0x07	: "?ssl_cert",
        0x08	: "?ssl_cipher",
        0x09	: "?ssl_session",
        0x0A	: "?req_attribute",
        0x0B	: "?ssl_key_size",
        0x0C	: "?secret",
        0x0D	: "?stored_method",
        0xFF	: "are_done"
        }

    methods = {
        1 : "OPTIONS",
        2 : "GET",
        3 : "HEAD",
        4 : "POST",
        5 : "PUT",
        6 : "DELETE",
        7 : "TRACE",
        8 : "PROPFIND",
        9 : "PROPPATCH",
        10 : "MKCOL",
        11 : "COPY",
        12 : "MOVE",
        13 : "LOCK",
        14 : "UNLOCK",
        15 : "ACL",
        16 : "REPORT",
        17 : "VERSION-CONTROL",
        18 : "CHECKIN",
        19 : "CHECKOUT",
        20 : "UNCHECKOUT",
        21 : "SEARCH",
        22 : "MKWORKSPACE",
        23 : "UPDATE",
        24 : "LABEL",
        25 : "MERGE",
        26 : "BASELINE_CONTROL",
        27 : "MKACTIVITY"
        }

    response_hdrs_table = {
        0x01 : "Content-Type	",
        0x02 : "Content-Language	",
        0x03 : "Content-Length	",
        0x04 : "Date	",
        0x05 : "Last-Modified	",
        0x06 : "Location	",
        0x07 : "Set-Cookie	",
        0x08 : "Set-Cookie2	",
        0x09 : "Servlet-Engine	",
        0x0A : "Status	",
        0x0B : "WWW-Authenticate	"
}

    def __init__(self, t, packet):
        self.type = t
        self.packet = packet
        self.magic = "%04x" % (socket.ntohs(struct.unpack('H', packet[0:2])[0]))
        self.length = socket.ntohs(struct.unpack('H', packet[2:4])[0])

    def parse_var_array(self, w, table):
        (cnt, w) = take_int(w)
        hdrs = []
        for i in xrange(0, cnt):
            (code_or_len, w) = take_int(w)
            if (code_or_len & 0xFF00) == 0xA000:
                name = table[code_or_len & 0x00FF]
            else:
                (name, w) = take_string(w, code_or_len)
            (val, l, w) = take_lpstring(w)
            hdrs.append((name, val))
        return (cnt, hdrs, w)

    def printme(self):
        None

class ForwardReq(AJPType):
    def __init__(self, t, p):
        AJPType.__init__(self, t,p)
        w = self.packet[2+2+1:]
        (self.method, w) = take_byte(w)
        self.method_str = "unknown" if not AJPType.methods.has_key(self.method) else AJPType.methods[self.method]
        (self.protocol_len, w) = take_int(w)
        (self.protocol, w) = take_string(w, self.protocol_len)
        (self.uri,l, w) = take_lpstring(w)
        (self.remote_addr,l, w) = take_lpstring(w)
        (self.remote_host,l, w) = take_lpstring(w)
        (self.server_name,l, w) = take_lpstring(w)
        (self.server_port, w) = take_int(w)
        (self.is_ssl, w) = take_boolean(w)
        (self.header_count, self.headers, w) = self.parse_var_array(w, AJPType.predefined_headers)

        self.attrs = []
        (attrc, w) = take_byte(w)
        while(attrc != 0xFF):
            if attrc == 0x0A:
                (attrn, l, w) = take_lpstring(w)
                (attrv, l, w) = take_lpstring(w)
            else:
                (attrn, w) = (AJPType.predefined_attrs[attrc], w)
                (attrv, l, w) = take_lpstring(w)
            self.attrs.append((attrn, attrv))
            (attrc, w) = take_byte(w)

    def printme(self):
        print '\t\tmethod:%s' % (self.method_str)
        print '\t\tprotocol:%s' % (self.protocol)
        print '\t\turi:%s' % (self.uri)
        print '\t\tremort_addr:%s' % (self.remote_addr)
        print '\t\tremote_host:%s' % (self.remote_host)
        print '\t\tserver:%s' % (self.server_name)
        print '\t\tport:%s' % (self.server_port)
        print '\t\theaders:'
        for i in self.headers:
            print '\t\t\t%s : %s' % (i[0], i[1])
        print '\t\tattrs:'
        for i in self.attrs:
            print '\t\t\t%s : %s' % (i[0], i[1])

class AJPResponseSendHeader(AJPType):
    def __init__(self, t, p):
        AJPType.__init__(self, t, p)
        w = self.packet[2+2+1:]
        self.parse(w)

    def parse(self, w):
        (self.code, w) = take_int(w)
        (self.status, l, w) = take_lpstring(w)
        (self.header_count, self.headers, w) = self.parse_var_array(w, AJPType.response_hdrs_table)

    def printme(self):
        print '\t\tstatus:%d' % (self.code)
        print '\t\tstatus:%s' % (self.status)
        print '\t\theaders:'
        for i in self.headers:
            print '\t\t\t%s : %s' % (i[0], i[1])

class AJPResponseChunk(AJPType):
    def __init__(self, t, p):
        AJPType.__init__(self, t, p)
        w = self.packet[2+2+1:]
        self.parse(w)

    def parse(self, w):
        (self.body, l, w) = take_lpstring(w)

    def printme(self):
        print self.body


class AJPData(AJPType):
    def __init__(self, t, p):
        AJPType.__init__(self, t, p)
        w = self.packet[2+2:]
        self.parse(w)

    def parse(self, w):
        (self.body, l, w) = take_lpstring(w)

    def printme(self):
        print self.body


class AJP:
    p = []
    ajpr = None
    def __init__(self, packet):
        self.ajp_req_types = {
            2 : ("CMD_AJP13_FORWARD_REQUEST 2 ", self.print_forward_request),
            3 : (" CMD_AJP13_SEND_BODY_CHUNK    ", self.print_send_body_chunk),
            4 : (" CMD_AJP13_SEND_HEADERS   4 ", self.print_send_headers),
            5 : (" CMD_AJP13_END_RESPONSE   5 ", self.print_end_response),
            6 : (" CMD_AJP13_GET_BODY_CHUNK     ", self.print_get_body_chunk),
            7 : (" CMD_AJP13_SHUTDOWN   7 ", self.print_shutdown),
            8 : (" CMD_AJP13_PING   8 ", self.print_ping),
            9 : (" CMD_AJP13_CPONG   9 ", self.print_cpong),
            10 :( " CMD_AJP13_CPING   10 ", self.print_cping),
            77 :( "CMD_AJP_REQUEST_SSL_RENEGO 77 " , self.print_ssl_renego),
            "none" : ("DATA",  self.print_data)
            }

        self.p = packet
        self.magic = "%04x" % (socket.ntohs(struct.unpack('H', packet[0:2])[0]))
        self.length = socket.ntohs(struct.unpack('H', packet[2:4])[0])
        self.type = ord(self.p[4])
        req_type = self.ajp_req_types["none"] if not self.ajp_req_types.has_key(self.type) else self.ajp_req_types[self.type]
        apply(req_type[1], [req_type[0], self.p])

    def printme(self):
        print '\tajp.magic:%s' % (self.magic)
        print '\tajp.length:%d' % (self.length)
        req_type = self.ajp_req_types["none"] if not self.ajp_req_types.has_key(self.type) else self.ajp_req_types[self.type]
        print '\tajp.type:%s' % (req_type[0])
        if self.ajpr:
            self.ajpr.printme()


    def print_forward_request(self, t, p):
        self.ajpr = ForwardReq(t,p)

    def print_send_body_chunk(self, t, p):
        self.ajpr = AJPResponseChunk(t, p)

    def print_send_headers(self, t, p):
        self.ajpr = AJPResponseSendHeader(t, p)

    def print_end_response(self, t, p):
        None

    def print_get_body_chunk(self, t, p):
        None

    def print_shutdown(self, t, p):
        None

    def print_ping(self, t, p):
        None

    def print_cpong(self, t, p):
        None

    def print_cping(self, t, p):
        None

    def print_ssl_renego(self, t, p):
        None

    def print_data(self, t, p):
        self.ajpr = AJPData(t,p)


def decode_ajp(p):
    return AJP(p)

pkt_no = 0
partial_packet = {}
def print_packet(pktlen, data, timestamp):
    global pkt_no
    global partial_packet
    pkt_no = pkt_no + 1
    if not data:
        return

    decoded=decode_ip_packet(data[14:])
    print decoded['protocol']
    #if protocols.has_key(decoded['protocol']) and protocols[decoded['protocol']] == 'tcp' :
    if 1:
        d = decode_tcp_packet(decoded['data'])
        print '#%d %s.%f %s:%d to %s:%d' % (
            pkt_no,
            time.strftime('%H:%M',
                          time.localtime(timestamp)),
            timestamp % 60,
            decoded['source_address'], d['sport'],
            decoded['destination_address'], d['dport'])


        if not d['data']:
          print "no data"
          return

        key = "%d-%d" % (d['sport'], d['dport'])
        partial = None if  not partial_packet.has_key(key) else partial_packet[key]
        if partial:
          newd = d['data']
          newd = partial[2] + newd
          newlen = partial[1] + len(newd)
          if partial[0] > newlen: #still to read partial data
            partial_packet[key] = (partial[0], newlen, newd)
            return
          d['data'] = newd
          partial_packet.remove(key)

        length = socket.ntohs(struct.unpack('H', d['data'][2:4])[0])
        if length != len(d['data']) - 4:
          partial_packet[key] = (length, len(d['data']) - 4, d['data'])
          print "len %d plen %d" % (length, len(d['data']))
          print "partial"
          return
        ajp = decode_ajp(d['data'])
        ajp.printme()
          #dumphex(decoded['data'])

if __name__=='__main__':

  if len(sys.argv) < 3:
    print 'usage: sniff.py <interface> <expr>'
    #sys.exit(0)
  p = pcap.pcapObject()
  #dev = pcap.lookupdev()
  #dev = sys.argv[1]
  #net, mask = pcap.lookupnet(dev)
  # note:  to_ms does nothing on linux
  err = None
  p.open_offline('cap.cap')
  #p.open_live(dev, 65535, 0, 100)
  #p.dump_open('cap.cap')
  p.setfilter(string.join(sys.argv[2:],' '), 0, 0)

  # try-except block to catch keyboard interrupt.  Failure to shut
  # down cleanly can result in the interface not being taken out of promisc.
  # mode
  #p.setnonblock(1)
  try:
    while 1:
      p.dispatch(1, print_packet)

    # specify 'None' to dump to dumpfile, assuming you have called
    # the dump_open method
    #  p.dispatch(0, None)

    # the loop method is another way of doing things
    #while 1:
    #  p.loop(1, print_packet)

    # as is the next() method
    # p.next() returns a (pktlen, data, timestamp) tuple
    #apply(print_packet,p.next())
  except KeyboardInterrupt:
    print '%s' % sys.exc_type
    print 'shutting down'
    print '%d packets received, %d packets dropped, %d packets dropped by interface' % p.stats()


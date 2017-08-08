import struct

from scapy.packet import *
from scapy.fields import *
from scapy.ansmachine import *
from scapy.layers.inet import UDP
from scapy.layers.inet import IP
from scapy.layers.inet import TCP
from scapy.base_classes import Net

import logging
logging.getLogger("scapy").setLevel(logging.DEBUG)

def guess_payload(p):
	AJPTypes = {
            2 : Raw, #("CMD_AJP13_FORWARD_REQUEST 2 ", self.print_forward_request),
            3 : Raw, #(" CMD_AJP13_SEND_BODY_CHUNK    ", self.print_send_body_chunk),
            4 : Raw, #(" CMD_AJP13_SEND_HEADERS   4 ", self.print_send_headers),
            5 : Raw, #(" CMD_AJP13_END_RESPONSE   5 ", self.print_end_response),
            6 : Raw, #(" CMD_AJP13_GET_BODY_CHUNK     ", self.print_get_body_chunk),
            7 : Raw, #(" CMD_AJP13_SHUTDOWN   7 ", self.print_shutdown),
            8 : Raw, #(" CMD_AJP13_PING   8 ", self.print_ping),
            9 : Raw, #(" CMD_AJP13_CPONG   9 ", self.print_cpong),
            10 :Raw, # " CMD_AJP13_CPING   10 ", self.print_cping),
            77 :Raw, # "CMD_AJP_REQUEST_SSL_RENEGO 77 " , self.print_ssl_renego),
            }
	hexdump(p)
	t = ord(p[0])
	print "suresh %0x %d" % (t, t in AJPTypes)
	if t in AJPTypes:
		return AJPTypes[t]
	return AJPData

class AJPData(Packet):
	name = "AJP Data"
	fields_desc = [ StrLenField("len", "0", fld="data", length_from = lambda p:struct.unpack("!H", p[0:2])[0]),
			]
	def guess_payload_class(self, p):
		return guess_payload(p)

class AJP(Packet):
	name = "AJP"
	fields_desc = [ XShortField("magic", 1),
			ShortField("length", 0),
			StrLenField("notdecoded", "", length_from = lambda pkt:pkt.len-4)
		      ]
	def guess_payload_class(self, p):
       		r = guess_payload(p)
		print r
		return r



bind_layers( TCP, AJP, sport=9009)
bind_layers( TCP, AJP, dport=9009)

def print_ajp(p):
	print "============================"
	print p.summary()
	print "============================"

#sniff(filter="port 9009", prn=print_ajp)
a = rdpcap("cap.cap")
print a
for p in a:
   print_ajp(p)
#def p(pkt):
#	if AJP in pkt:
#		print pkt[AJP].magic
#
#sniff(filter="port 9009", prn=p)



#!/usr/bin/python
# This uses the rawsocket.c Python module defined previously:
# http://groups.google.com/groups?selm=3D7058A9.29732F5%40alum.mit.edu
# N.B. This is shamelessly Linux-centric and likely to blow up on
# other platforms.

import sys, os, string, re, types

try:
    import rawsocket
except ImportError:
    os.system("gcc -Wall -O2 -c -I/usr/include/python2.5 rawsocket.c")
    os.system("gcc -shared -o rawsocket.so rawsocket.o")
    import rawsocket

#########################################################

def ip_address(a, b, c, d):
    """Given four bytes for a dotted quad, build a long"""
    return ((long(a) << 24) + (long(b) << 16) +
            (long(c) << 8) + long(d))

# First get my own MAC and IP addresses
ifcfg_lines = os.popen("/sbin/ifconfig eth0").readlines()

x = string.split(ifcfg_lines[0])[4]
my_macaddr = eval("0x" + re.sub(":", "", x) + "L")

x = string.split(ifcfg_lines[1])[1]
x = string.split(re.sub(".*:", "", x), ".")
my_ipaddr = apply(ip_address, map(eval, x))

# Now we'll go on a Snipe Hunt for the Etherio board, but
# we'll start out ignorant of its MAC address. The purpose of
# this exercise is to use the network to ask it for its MAC
# address.
bd_ipaddr = ip_address(192, 168, 1, 150)
bd_macaddr = 0xFFFFFFFFFFFFL

# But actually I do know the MAC address for the board, and
# I'll want it later for my ARP table
bd_macaddr_really = 0x45445450

#########################################################

class Packet:

    def __init__(self):
        self.previous = None

    def bytes(self):
        r = ""
        if self.previous:
            r = self.previous.bytes()
        return r + self.thesebytes()

    def thesebytes(self):
        r = ""
        for (field, size) in self.format:
            size = self.eval(size)
            r = r + self.bigEndian(size, getattr(self, field))
        return r

    def compute_checksum(self, str=None):
        if str == None:
            str = self.thesebytes()
        if len(str) & 1:
            str = str + "\000"
        sum = 0L
        while str:
            highbyte = ord(str[0])
            lowbyte = ord(str[1])
            str = str[2:]
            x = (highbyte << 8) + lowbyte
            sum = sum + x
        while (sum >> 16):
            sum = (sum & 0xffff) + (sum >> 16)
        return int((~sum) & 0xffff)

    def bigEndian(self, n, x):
        """Used to build packets, converts ints or longs to a
        string of characters suitable for socket sending"""
        str = ""
        shift = (n - 1) * 8
        for i in range(n):
            str = str + chr((x >> shift) & 0xFF)
            shift = shift - 8
        return str

    def extract(self, x):
        if self.previous:
            x = self.previous.extract(x)
        for (field, size) in self.format:
            size = self.eval(size)
            y, x = self.extractField(x, size)
            setattr(self, field, y)
        return x

    def extractField(self, x, len):
        """Pop a string of characters off the front of a string
        and interpret them as a big-endian long integer, used to
        parse packets."""
        sum = ""
        for c in x[:len]:
            sum = sum + ("%02X" % ord(c))
        sum = eval("0x" + sum + "L")
        try: sum = int(sum)
        except OverflowError: pass
        return sum, x[len:]

    def dottedQuad(self, x):
        """Pop four characters off the front of a string and
        interpret them as a dotted quad, return a string"""
        sum = ""
        for i in range(4):
            y = int((x & 0xff000000L) >> 24)
            sum = sum + "." + repr(y)
            x = x << 8
        return sum[1:]

    def eval(self, field):
        orig = field
        for i in range(3):
            if (type(field) == types.IntType or
                type(field) == types.LongType):
                return field
            else:
                field = getattr(self, field)
        raise AttributeError, "what the heck is " + orig

    def dump(self, indent=0):
        if self.previous:
            self.previous.dump(indent)
        print (indent * "    ") + self.__class__.__name__
        indent = indent + 1
        for (field, size) in self.format:
            x = getattr(self, field)
            print (indent * "    ") + field + " " + hex(x),
            if self.eval(size) == 4:
                print self.dottedQuad(x)
            else:
                print

 
class EthernetHeader(Packet):

    # Ethernet protocol IDs, /usr/include/net/ethernet.h
    TYPE_PUP    = 0x0200    # Xerox PUP
    TYPE_IP     = 0x0800    # IP
    TYPE_ARP    = 0x0806    # Address resolution
    TYPE_REVARP = 0x8035    # Reverse ARP

    format = [ ("dst_mac", 6),
               ("src_mac", 6),
               ("packet_type", 2) ]


class ArpPacket(Packet):

    format = [ ("arp_hw", 2),
               ("ethernet_type", 2),
               ("hw_addr_len", 1),
               ("prot_addr_len", 1),
               ("arp_opcode", 2),
               # these sizes are variable
               ("src_hw_addr", "hw_addr_len"),
               ("src_prot_addr", "prot_addr_len"),
               ("dst_hw_addr", "hw_addr_len"),
               ("dst_prot_addr", "prot_addr_len") ]

    # Constants found in /usr/include/net/if_arp.h
    # ARP hardware IDs
    HW_NETROM = 0     # From KA9Q: NET/ROM pseudo.
    HW_ETHER = 1      # Ethernet 10/100Mbps. 
    HW_EETHER = 2     # Experimental Ethernet. 
    HW_AX25 = 3       # AX.25 Level 2. 
    HW_PRONET = 4     # PROnet token ring. 
    HW_CHAOS = 5      # Chaosnet. 
    HW_IEEE802 = 6    # IEEE 802.2 Ethernet/TR/TB. 
    HW_ARCNET = 7     # ARCnet. 
    HW_APPLETLK = 8   # APPLEtalk. 
    HW_DLCI = 15      # Frame Relay DLCI. 
    HW_ATM = 19       # ATM. 
    HW_METRICOM = 23  # Metricom STRIP (new IANA id). 

    # ARP opcodes
    OPCODE_REQUEST = 1   # ARP request.
    OPCODE_REPLY = 2     # ARP reply.
    OPCODE_RREQUEST = 3  # RARP request.
    OPCODE_RREPLY = 4    # RARP reply.
    OPCODE_InREQUEST = 8 # InARP request.
    OPCODE_InREPLY = 9   # InARP reply.
    OPCODE_NAK = 10      # (ATM)ARP NAK.

    def __init__(self):
        # For now, assume hardware address are 6-byte MAC addresses,
        # and protocol addresses are 4-byte IP addresses, but this
        # might be different for 802.11b or other protocols.
        self.hw_addr_len = 6
        self.prot_addr_len = 4
        self.previous = EthernetHeader()


arptable = {
    my_ipaddr: my_macaddr,
    # bd_ipaddr: bd_macaddr_really
    }


class IpHeader(Packet):

    # Constants taken from /usr/include/netinet/in.h
    PROT_IP = 0        # Dummy protocol for TCP.
    PROT_HOPOPTS = 0   # IPv6 Hop-by-Hop options.
    PROT_ICMP = 1      # Internet Control Message Protocol.
    PROT_IGMP = 2      # Internet Group Management Protocol
    PROT_IPIP = 4      # IPIP tunnels (older KA9Q tunnels use 94).
    PROT_TCP = 6       # Transmission Control Protocol.
    PROT_EGP = 8       # Exterior Gateway Protocol.
    PROT_PUP = 12      # PUP protocol.
    PROT_UDP = 17      # User Datagram Protocol.
    PROT_IDP = 22      # XNS IDP protocol.
    PROT_TP = 29       # SO Transport Protocol Class 4.
    PROT_IPV6 = 41     # IPv6 header.
    PROT_ROUTING = 43  # IPv6 routing header.
    PROT_FRAGMENT = 44 # IPv6 fragmentation header.
    PROT_RSVP = 46     # Reservation Protocol.
    PROT_GRE = 47      # General Routing Encapsulation.
    PROT_ESP = 50      # encapsulating security payload.
    PROT_AH = 51       # authentication header.
    PROT_ICMPV6 = 58   # ICMPv6.
    PROT_NONE = 59     # IPv6 no next header.
    PROT_DSTOPTS = 60  # IPv6 destination options.
    PROT_MTP = 92      # Multicast Transport Protocol.
    PROT_ENCAP = 98    # Encapsulation Header.
    PROT_PIM = 103     # Protocol Independent Multicast.
    PROT_COMP = 108    # Compression Header Protocol.
    PROT_RAW = 255     # Raw IP packets.

    format = [ ("version_hdr_len", 1),
               ("diff_services_field", 1),
               ("total_length", 2),
               ("id", 2),
               ("flags_fragment_offset", 2),
               ("ttl", 1),
               ("protocol", 1),
               ("hdr_chksum", 2),
               ("src_ipaddr", 4),
               ("dst_ipaddr", 4),
               ]

    def __init__(self, srcip, dstip, datalength, protocol):

        self.previous = eth = EthernetHeader()
        eth.dst_mac = arptable[dstip]
        eth.src_mac = arptable[srcip]
        eth.packet_type = EthernetHeader.TYPE_IP

        self.version_hdr_len = 0x45
        self.diff_services_field = 0
        self.total_length = datalength + 20
        self.id = 0x67cb
        self.flags_fragment_offset = 0x4000
        self.ttl = 64
        self.protocol = protocol
        self.hdr_chksum = 0
        self.src_ipaddr = srcip
        self.dst_ipaddr = dstip

        self.hdr_chksum = self.compute_checksum()


class UdpPacket(Packet):

    format = [ ("src_port", 2),
               ("dst_port", 2),
               ("length", 2),
               ("chksum", 2),
               ]

    def __init__(self, srcip, srcport, dstip, dstport, data):
        n = len(data) + 8
        self.length = n
        self.src_port = srcport
        self.dst_port = dstport
        self.data = data
        self.chksum = 0

        self.chksum = self.compute_checksum()

        self.previous = IpHeader(srcip, dstip,
                                 n, IpHeader.PROT_UDP)

    def thesebytes(self):
        return Packet.thesebytes(self) + self.data

    def extract(self, x):
        x = Packet.extract(self, x)
        n = self.length - 8
        self.data = x[:n]
        return x[n:]

    def dump(self):
        Packet.dump(self)
        print "UDP data:",
        print string.join(map(lambda x: hex(ord(x)),
                              list(self.data)))


########################################################################
#
# Create a raw socket. Set up an outgoing ARP packet, asking for the
# board's MAC address. An ARP reply will come back. Print dumps of
# both outgoing and incoming packets.
#


def test_arp():

    try:
        sock = rawsocket.socket(EthernetHeader.TYPE_ARP)
    except IOError:
        print "Login as root, or you won't be able to make a raw socket"
        raise IOError

    a = ArpPacket()

    eth = a.previous
    eth.dst_mac = bd_macaddr
    eth.src_mac = my_macaddr
    eth.packet_type = EthernetHeader.TYPE_ARP

    a.arp_hw = ArpPacket.HW_ETHER
    a.ethernet_type = EthernetHeader.TYPE_IP
    a.arp_opcode = ArpPacket.OPCODE_REQUEST
    a.src_hw_addr = my_macaddr
    a.src_prot_addr = my_ipaddr
    a.dst_hw_addr = bd_macaddr
    a.dst_prot_addr = bd_ipaddr

    print "ARP REQUEST"
    a.dump()

    # Send ARP request, receive ARP reply
    rawsocket.sendto(sock, a.bytes())
    x = rawsocket.recvfrom(sock)

    print "\nARP REPLY"
    x = a.extract(x)
    a.dump()

    if x:
        print len(x), "bytes left over"

def standard_arp_request(dst_ipaddr):
    try:
        sock = rawsocket.socket(EthernetHeader.TYPE_ARP)
    except IOError:
        print "Login as root, or you won't be able to make a raw socket"
        raise IOError

    a = ArpPacket()

    eth = a.previous
    eth.dst_mac = 0xFFFFFFFFFFFFL
    eth.src_mac = my_macaddr
    eth.packet_type = EthernetHeader.TYPE_ARP

    a.arp_hw = ArpPacket.HW_ETHER
    a.ethernet_type = EthernetHeader.TYPE_IP
    a.arp_opcode = ArpPacket.OPCODE_REQUEST
    a.src_hw_addr = my_macaddr
    a.src_prot_addr = my_ipaddr
    a.dst_hw_addr = 0xFFFFFFFFFFFFL
    a.dst_prot_addr = dst_ipaddr
    rawsocket.sendto(sock, a.bytes())
    x = rawsocket.recvfrom(sock)
    x = a.extract(x)
    arptable[dst_ipaddr] = a.src_hw_addr


def test_udp(byte):

    if not arptable.has_key(bd_ipaddr):
        standard_arp_request(bd_ipaddr)

    try:
        sock = rawsocket.socket(EthernetHeader.TYPE_IP)
    except IOError:
        print "Login as root, or you won't be able to make a raw socket"
        raise IOError

    cmd = "\xff" + ((byte and "\x01") or "\x00")

    u = UdpPacket(my_ipaddr, 43700,
                  bd_ipaddr, 5000, cmd)
    print "IP COMMAND TO BOARD"
    u.dump()
    rawsocket.sendto(sock, u.bytes())
    x = rawsocket.recvfrom(sock)
    oldtype = u.previous.previous.packet_type
    x = u.extract(x)
    assert u.previous.previous.packet_type == oldtype
    print "\nIP RESPONSE FROM BOARD"
    u.dump()
    if x:
        print len(x), "bytes left over"


if len(sys.argv) > 1 and sys.argv[1] == "arp":
    test_arp()
else:
    byte = 0
    if len(sys.argv) > 2:
        byte = eval(sys.argv[2])
    test_udp(byte)

# When I run "python packetfun.py udp 1", here is what happens.
# Because I've removed the board's MAC address from arptable, an ARP
# request is issued to the board, and the board sends back an ARP
# response whose only wierdness is that it has trailing bytes because
# I'm still a little ignorant about the CS8900A chip. Then a UDP
# command goes out, and the board sends back the expected UDP
# response. So far so good.

# Then a funny thing happens. The UDP response gets picked up by the
# Linux kernel, who looks at it and says, "hey, I never issued this
# port number, what's going on?" and the Linux kernel sends out an
# ICMP message saying, "there is no process listening on the bogus
# port number you're trying to reach". Technically the message is
# a "Destination unreachable" message. The board thinks it's being
# pinged, and sends back a ping response.

# Here's info on port allocation:
# http://www.cee.hw.ac.uk/courses/3ne3/5/9.htm
# http://ou800doc.caldera.com/SDK_netapi/sockC.addr_binding.html

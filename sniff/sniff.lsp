#!/usr/bin/env newlisp

; sniff  v.1.23 - net work packet snifffer
; 2010-March-26 added out-commented callback method
; 2010-April-10 documentation
; 2010-April-27 added 'packet' output format for whole packet
;               added output of unknown packets
; 2010-April-29 help text improvements
; 2010-May-29 added library path for OpenBSD 4.6 
; 2013-Sep-22 change stats to pstats (stats is function since 10.4.2)
;
; See also http://www.tcpdump.org/pcap.htm
; tested on Mac OS X 10.6 and UBUNTU 9.4 and Win32
; for a Win32 library goto http://www.winpcap.org/
; first version L.M.in March 2010
;
; This script must be run with super user prigileges.

(unless (main-args 2) 
	(println [text]
- sniff - network packet sniffer, version 1.21, April 2010

USAGE UNIX: 
    sudo sniff <device> [<filter> [<count> [hex | ascii | packet]]] 

USAGE Win32:
    newlisp sniff <device> [<filter> [<count> [hex | ascii | packet]]]   

The program must run in super user or administrator mode. On Unix the 
sudo utility can be used, and the OS will ask for the root password.

EXAMPLES: 
    sniff eth01 'tcp port 80' 100 hex
    sniff en1 ip 100 packet           <-- best all purpose format
    sniff eth0 'ip host 10.1.2.3'
    sniff en0
    sniff ""
    
Use an empty string "" or '' for <device> to let sniff find out a device.
This works well on Windows but frequently does not work on UNIX, where
it is better to use the ifconfig utility to find a connected device. 

The 'hex' option dumps data in HEX and ASCII, the 'ascii' option only in 
ASCII format. Both options only dump the data part of the packet after the 
TCP, UDP or frst 8 bytees of the ICMP header. The 'packet' option dumps
the whole packet starting with the IP header and in HEX and ASCII format. 

Default <filter> expression is 'ip' for all protocols. The protocol ids:
tcp, udp and icmp are supported. Default packet <count> is 10 and 
default output mode is no output.
[/text])
	(exit)
)

; import pcap library

(set 'bit-64 (= 256 (& (sys-info -1) 256)))

(set 'files '(
    "/usr/local/lib/libpcap.so"; CentOS 6.7
	"/usr/lib/libpcap.dylib"   ; Mac OS X 10.6
	"/usr/lib/libpcap.so.0.8"  ; UBUNTU 9.04
	"/usr/lib/libpcap.so.1.0.0"; UBUNTU 9.04
	"/usr/lib/libpcap.so.6.0"  ; OpenBSD 4.8
	"c:/windows/system32/wpcap.dll" ; Win32 XP
))

(set 'libpcap (files (or
			(find true (map file? files))
			(throw-error "cannot find  pcap packet capture library"))))

; import library functions
(import libpcap "pcap_lookupdev")
(import libpcap "pcap_lookupnet")
(import libpcap "pcap_open_live")
(import libpcap "pcap_compile")
(import libpcap "pcap_setfilter")
(import libpcap "pcap_next")
(import libpcap "pcap_loop")
(import libpcap "pcap_stats")

; allocate ptr varables used in library API
(set 'errbuf (dup "\000" 256)) ; for error message
(set 'fp (dup "\000" 8)) ; sizeof pointer (32 and 64 bit)
(set 'net "\000\000\000\000") ; IP number of sniffing device
(set 'mask "\000\000\000\000") ; net mask of sniffing device
(set 'header (dup "\000" 24)) ; space for packet header (32 and 64 bit)
(set 'protocols '((6 TCP) (17 UDP) (1 ICMP) (0 IP)))

; constants
(constant 'PCAP_BUFFSIZE 1518)
(constant 'PROMISCUOUS 1)
(constant 'SIZE_ETHER 14) 

; get device
(set 'dev (main-args 2))
(when (= dev "")
	(set 'dev (pcap_lookupdev errbuf)))

(when (zero? dev)
	(println errbuf)
	(println "must exit")
	(exit 1))

(when (= (pcap_lookupnet dev net mask errbuf) -1)
	(println (get-string errbuf))
	(println "must exit")
	(exit 1))

; get filter expression
(if (main-args 3)
	(set 'filter-exp (lower-case (main-args 3)))
	(set 'filter-exp "ip")) ; default is all packets

; get packet count
(set 'packet-count (int (main-args 4) 10)) ; default 10

; get output options
(set 'out-opt (or (main-args 5) "none"))	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set 'handle (pcap_open_live dev PCAP_BUFFSIZE PROMISCUOUS 1000 errbuf))
(when (= handle 0)
	(println (get-string errbuf))
	(println "must exit")
	(exit 2))

(when (= (pcap_compile handle fp filter-exp 0 net) -1)
	(println "could not compile filter:" filter-exp)
	(println "must exit")
	(exit 3))

(when (= (pcap_setfilter handle fp) -1)
	(println "could not install filter:" filter-exp)
	(println "must exit")
	(exit 4))

; transform ip number to n.n.n.n
(define (ip-to-ascii ip)
	(join (map string (unpack "bbbb" (pack ">lu" ip))) "."))
	
; tansform mac address to n:n::n:n:n:n
(define (eth-to-ascii host)
	(join (map (fn (x) (format "%02x" x)) (unpack "bbbbbb" host) ) ":"))

; packet handler, called when packet arrives
(define (report-packet header packet)

	(if bit-64
		(map set '(tm usec dummy caplen len)  (unpack "Lu lu lu lu lu" header))
		(map set '(tm usec caplen len)  (unpack "lu lu lu lu" header)))
	(println "time: " (date tm 0 "%H:%M:%S.") usec " captured:" caplen " wire:" len)

	; get pcap header
	(map set '(dhost shost type)  (unpack "s6 s6 d" packet))
	(println "from ether addr: " (eth-to-ascii shost) 
			" to: " (eth-to-ascii dhost))

	; IP header at p + SIZE_ETHER: sniff_ip
	(map set '(vhl tos ip-len id off ttl proto sum src dest) 
		(unpack "> b b u u u b b u lu lu" (+ packet SIZE_ETHER)))
	(set 'IP_HL (& vhl 0x0f))
	(set 'IP_V (>> vhl 4))

	(when (or (< IP_HL 5) (< ip-len (* IP_HL 4)))
		(println "ERROR malformed IP datagram")
		(throw true))

	(set 'protocol (lookup proto protocols))

	(if ; with multiple <condition> <body> terms
	; TCP header: sniff_tcp
	(= protocol 'TCP) ; TCP 6
	(begin
		(map set '(sport dport seq ack offx2) 
			(unpack "> u u lu lu b" (+ packet SIZE_ETHER (* IP_HL 4))))
		(set 'TH_OFF  (>> (& offx2 0xf0) 4)) ; * 4 -> offset
		(when (< TH_OFF 5)
			(println "ERROR malformed TCP segment")
			(throw true))

		(println protocol " from ip addr: " (ip-to-ascii src) " port:" sport
                 " to: " (ip-to-ascii dest) " port:" dport)
	)

	(= protocol 'UDP) ; UDP 17
	(begin
		(map set '(sport dport ulen sum) 
			(unpack "> u u u u" (+ packet SIZE_ETHER (* IP_HL 4))))
		(set 'TH_OFF 2) ; 2*4=8 bytes of UDP header length

		(println protocol " from " (ip-to-ascii src) " port:" sport
                 " to " (ip-to-ascii dest) " port:" dport)
	)

	(= protocol 'ICMP) ; ICMP 1
	(begin
		(map set '(type code cksum other) 
			(unpack "> b b u lu" (+ packet SIZE_ETHER (* IP_HL 4))))
		(set 'TH_OFF 2) ; 2*4=8 bytes of ICMP header length
		(println protocol 
			" from: " (ip-to-ascii src) " to: " (ip-to-ascii dest)) 
		(println "type: " type " code: " code  " checksum: " cksum)
	)

	true ; catch all other
	(begin
		(set 'TH_OFF 0)
		(print "Protocol: " proto
			" from: " (ip-to-ascii src) " to: " (ip-to-ascii dest)) )
	) ; end if

	; dump payload
	;(set 'ip-len (min len (- caplen SIZE_ETHER)))
	(set 'payload-len (- ip-len (* IP_HL 4) (* TH_OFF 4)))
	(print "packet total len w/o ether-header:" ip-len)
	(println ", payload length:" payload-len)

	(when (and (zero? payload-len) (!= out-opt "packet"))
		(throw true))

	(if (= out-opt "packet")
		(set 'data (unpack (dup "b" ip-len) 
			(+ packet SIZE_ETHER )))
		(set 'data (unpack (dup "b" payload-len) 
			(+ packet SIZE_ETHER (* IP_HL 4) (* TH_OFF 4))))
	)

	; dump data as hex and ascii
	(when (or (= out-opt "hex") (= out-opt "packet"))
		(set 'addr 0)
		(dolist (line (explode data 16) )
			(print (format "%05d " addr))
			(println (format "%-48s" (join (map (fn (b) (format "%02x " b)) line))) " "
				(join (map (fn (c) (if (or (< c 32) (> c 126)) "." (char c))) line)))
		(inc addr 16))
	)

	; dump ascii only
	(when (= out-opt "ascii")
		(dolist (line (explode data 64) )
			(println (join (map (fn (c) (if (or (< c 32) (> c 126)) "." (char c))) line)))
		)
	)

	true
)

(set 'counter 0)
(println "device: " (get-string dev))

; Method using pcap_next and newLISP controlling
; this method leves more time in newLISP to do 
; stuff in the while loop.
(while (< counter packet-count)
	(set 'capture '())
	(set 'packet (pcap_next handle header))
	(unless (= packet 0)
		(print "\n--- " (inc counter) "---  ")
		(catch (report-packet header packet ))
))

; Alternative method using callback with pcap_loop
; and pcap library controlling. This leaves less
; time for newLISP as the waiting for new packets
; happens inside libpcap
;(define (callback-handler params header packet)
;	(unless (= packet 0)
;		(print "\n--- " (inc counter) "---  ")
;		(catch (report-packet header packet ))
;))
;(pcap_loop handle 20 (callback 0 'callback-handler) 0)


; report pcap status
(set 'pstats (dup "\000" 8))
(pcap_stats handle pstats)
(map set '(received dropped) (unpack "lu lu" pstats))
(println "\npackages dropped: " dropped)

(exit)

; eof

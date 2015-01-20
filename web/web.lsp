(module "crypto.lsp")

(setq files (list
    "/usr/lib64/libevent-2.0.so.5" ; CentOS 7
    "/usr/lib/i386-linux-gnu/libevent-2.0.so.5" ; Ubuntu 14.04 LTS (32 bit)
    "/usr/local/lib/libevent.so" ; https://github.com/downloads/libevent/libevent/libevent-2.0.21-stable.tar.gz
    "/usr/lib/libevent-2.0.so.5" ; Ubuntu 12.04.3 LTS
))

(setq lib (files (or (find true (map file? files)) (throw-error "cannot find libevent library"))))

(import lib "event_init")
(import lib "event_base_dispatch")
(import lib "evbuffer_add_printf")
(import lib "evbuffer_free")
(import lib "evbuffer_get_length")
(import lib "evbuffer_new")
(import lib "evbuffer_write")
(import lib "evhttp_bind_socket")
(import lib "evhttp_new")
(import lib "evhttp_request_get_uri")
(import lib "evhttp_request_get_input_buffer")
(import lib "evhttp_send_reply")
(import lib "evhttp_send_reply_end")
(import lib "evhttp_set_gencb")

(define (url-encode str)
    (join (map (fn (c) (format "%%%02x" c)) (unpack (dup "b" (length str)) str))))

(define (url-decode url)
    (replace "%([0-9a-f][0-9a-f])" (replace "+" url " ") (pack "b" (int $1 0 16)) 1))

(define (get-data data datalen, fd)
    (let ((databin nil))
        (if (> datalen 0)
            (begin
                (setq fd (pipe))
                (evbuffer_write data (last fd))
                (read (first fd) databin datalen)
                (dolist (f fd) (close f))))
        databin))
    
(define (process_request req arg, query)
    (letn ((buf (evbuffer_new)) 
           (uri (evhttp_request_get_uri req))
           (data (evhttp_request_get_input_buffer req))
           (datalen (evbuffer_get_length data))
           (params (catch (map (lambda (x) (parse x "=")) (parse ((parse (get-string uri) "?") 1) "&")) 'query)))
        (evbuffer_add_printf buf 
            "{ 'uri':'%s', 'datalen':'%d', 'data-md5':'%s', 'query':'%s' }" 
            uri 
            datalen
            (crypto:md5 (get-data data datalen))
            (if (nil? params) "" (string query)))
        (evhttp_send_reply req 200 "OK" buf)
        (evhttp_send_reply_end req)
        (evbuffer_free buf)))

(define (main)
    (println "newLISP(c) WEB 1.0.0")
    (println "using " lib)

    (setf host "0.0.0.0")
    (setf port 1972)

    (setf base (event_init))
    (setf httpd (evhttp_new base))

    (if (evhttp_bind_socket httpd host port)
        (println (format "listening %s:%d" host port)))

    (evhttp_set_gencb httpd (callback 1 'process_request) nil)
    (event_base_dispatch base)

    (exit 0))

(main)

; curl -v --request POST --data-binary "@/home/irocha/Pictures/Ivan.jpg" http://localhost:1972/?name=Ivan.jpg && echo;md5sum /home/irocha/Pictures/Ivan.jpg;echo
; curl -v -d "this is a test" "http://localhost:1972/?name=ale&dog=luma";echo


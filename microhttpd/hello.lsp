; CentOS
; sudo yum install libmicrohttpd-devel libmicrohttpd-doc libmicrohttpd

(load "util.lsp")
(load "json.lsp")

(setf lib "libmicrohttpd.so.10")

(import lib "MHD_start_daemon")
(import lib "MHD_stop_daemon")
(import lib "MHD_create_response_from_data")
(import lib "MHD_queue_response")
(import lib "MHD_destroy_response")
(import lib "MHD_add_response_header")

(setf MHD_USE_SELECT_INTERNALLY 8)
(setf MHD_OPTION_END 0)
(setf MHD_YES 1)
(setf MHD_NO 0)
(setf MHD_HTTP_OK 200)

(setf PORT 8081)

(define (process_request cls connection url method version upload_data upload_data_size con_cls, page response ret)
    (setq page (Json:Lisp->Json '(("message" "Hello, newLISP!"))))
    (setq response (MHD_create_response_from_data (length page) page MHD_YES MHD_YES))
    (MHD_add_response_header response "Content-type" "application/json")
    (setq ret (MHD_queue_response connection MHD_HTTP_OK response))
    (MHD_destroy_response response)
    ret)

(setf cb (callback 1 'process_request))

(setf daemon (MHD_start_daemon MHD_USE_SELECT_INTERNALLY PORT 0 0 
              cb 0 MHD_OPTION_END))

(read-key)

(MHD_stop_daemon daemon)

(exit)

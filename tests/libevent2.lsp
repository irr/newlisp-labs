;; @module libevent
;; @description Low-level newlisp bindings for libevent2.
;; @version 0.1
;; @author Jeff Ober <jeffober@gmail.com>
;;
;; The libevent module provides a wrapper on top of the
;; @link http://libevent.org/ libevent2 library.
;;
;; TODO
;; <ul>
;;   <li>signals</li>
;; </ul>
;;
;; @example
;; ; ------------------------------------------------------------------------------
;; ; Timers
;; ; ------------------------------------------------------------------------------
;; (libevent:init)
;;
;; (libevent:set-interval 10
;;   (fn () (println "Another 10ms have passed!")))
;;
;; (libevent:run)
;;
;;
;; ; ------------------------------------------------------------------------------
;; ; IO
;; ; ------------------------------------------------------------------------------
;; (libevent:init)
;; (setf socket (net-connect "www.google.com" 80))
;; (setf buffer "")
;;
;; ; Wait until socket is write-ready
;; (libevent:watch-once socket libevent:WRITE
;;   (fn (fd e id)
;;     ; send HTTP request
;;     (write socket "GET / HTTP/1.0\r\n\r\n")
;;
;;     ; wait for response
;;     (libevent:watch socket libevent:READ
;;       (fn (fd e id , buf bytes)
;;         ; read to local buffer
;;         (setf bytes (read fd buf 4096))
;;         (if bytes
;;           ; write to global buffer
;;           (write buffer buf)
;;           ; kill watcher and stop loop
;;           (begin
;;             (libevent:unwatch id)
;;             (libevent:stop)))))))
;;
;; (libevent:run)
;; (println buffer)
;;
;;
;; ; ------------------------------------------------------------------------------
;; ; Using buffers
;; ; ------------------------------------------------------------------------------
;; (libevent:init)
;; 
;; (setf html "")
;; 
;; (define (on-read data)
;;   (write html data))
;; 
;; (define (on-event ev data)
;;   (cond
;;     ((libevent:masks? ev libevent:BUFFER_EOF)
;;      (write html data)
;;      (println "Disconnected")
;;      (libevent:stop))
;;     ((libevent:masks? ev libevent:BUFFER_ERROR)
;;      (println "An error occurred")
;;      (libevent:stop))
;;     ((libevent:masks? ev libevent:BUFFER_TIMEOUT)
;;      (println "Timed out")
;;      (libevent:stop))))
;; 
;; (or (setf socket (net-connect "www.google.com" 80))
;;     (throw-error "Unable to connect"))
;; 
;; (setf buffer (libevent:make-buffer socket (regex-comp "[\r\n]+" 4) on-read on-event))
;; (libevent:buffer-send buffer "GET / HTTP/1.0\r\n\r\n")
;; (libevent:run)
;; 
;; (println html)

;-------------------------------------------------------------------------------
;Data storage
;-------------------------------------------------------------------------------
(define EventID:EventID)
(define EventCB:EventCB)

(define BufferID:BufferID)
(define BufferCB:BufferCB)
(define BufferEv:BufferEv)
(define BufferData:BufferData)

(context 'libevent)

(struct 'TIMEVAL "int" "long")

;-------------------------------------------------------------------------------
; Constants (from event.h)
;-------------------------------------------------------------------------------
;; <h3>Event constants</h3>
;; @const READ
;; @const WRITE
;; @const TIMEOUT
;; @const SIGNAL
(constant 'TIMEOUT   0x01)
(constant 'READ      0x02)
(constant 'WRITE     0x04)
(constant 'SIGNAL    0x08)
(constant 'PERSIST   0x10)

; Buffer events
;; <h3>Buffer constants</h3>
;; @const BUFFER_READING
;; @const BUFFER_WRITING
;; @const BUFFER_EOF
;; @const BUFFER_ERROR
;; @const BUFFER_TIMEOUT
;; @const BUFFER_CONNECTED
(constant 'BUFFER_READING   0x01)
(constant 'BUFFER_WRITING   0x02)
(constant 'BUFFER_EOF       0x10)
(constant 'BUFFER_ERROR     0x20)
(constant 'BUFFER_TIMEOUT   0x40)
(constant 'BUFFER_CONNECTED 0x80)

; Buffer options
(constant 'BUFFER_OPT_DEFER_CALLBACKS (<< 1 2))

; Defaults
(constant 'DEFAULT_CHUNK_SIZE 1024)

;-------------------------------------------------------------------------------
; Locate libevent library
;-------------------------------------------------------------------------------
(constant 'LIB
  (cond
    ((= ostype "Win32") "libevent.dll")
    ((= ostype "OSX")   "libevent.dylib")
    (true               "/usr/lib64/libevent-2.0.so.5")))

(unless (import LIB)
  (throw-error "libevent not found"))

;-------------------------------------------------------------------------------
; Import libevent routines
;-------------------------------------------------------------------------------
(import LIB "event_enable_debug_mode")
(import LIB "event_base_new" "void*")
(import LIB "event_base_free" "void" "void*")
(import LIB "event_base_dispatch" "int" "void*")
(import LIB "event_base_loopbreak" "int" "void*")

(import LIB "event_new" "void*" "void*" "int" "short int" "void*" "void*")
(import LIB "event_free" "void" "void*")
(import LIB "event_add" "int" "void*" "void*")
(import LIB "event_del" "int" "void*")

(import LIB "bufferevent_socket_new" "void*" "void*" "int" "int")
(import LIB "bufferevent_free" "void" "void*")
(import LIB "bufferevent_enable" "int" "void*" "short int")
(import LIB "bufferevent_disable" "int" "void*" "short int")
(import LIB "bufferevent_read" "int" "void*" "void*" "int")
(import LIB "bufferevent_write" "int" "void*" "void*" "int")
(import LIB "bufferevent_setcb" "void" "void*" "void*" "void*" "void*" "void*")

(when MAIN:LIBEVENT2_DEBUG
  (event_enable_debug_mode))

;-------------------------------------------------------------------------------
;Utilities
;-------------------------------------------------------------------------------
(define (masks? a b)
  (not (zero? (& a b))))

;-------------------------------------------------------------------------------
; Loop control
;-------------------------------------------------------------------------------
(setf BASE nil)
(setf RUNNING nil)

;; @syntax (init)
;; Initializes the event loop. Will not re-init a previously initialized
;; loop unless <cleanup> is called first.
(define (init)
  (or BASE
      (not (zero? (setf BASE (event_base_new))))
      (throw-error "Error initializing event loop")))

(define (initialized?)
  "Returns true if libevent has been initialized."
  (true? BASE))

(define (assert-initialized)
  "Convenience routine to throw an error if the library has not yet been
  initialized."
  (unless (initialized?)
    (throw-error "Event loop is not initialized")))

(define (cleanup)
  "Cleans up memory used by the event loop."
  (when RUNNING (stop))
  (when BASE
    (event_base_free BASE)
    (setf BASE nil)))

;; @syntax (run)
;; Starts the event loop. Does not return until the loop is stopped.
(define (run)
  (setf RUNNING true)
  (case (event_base_dispatch BASE)
    (0  true)
    (1  (throw-error "No more events registered."))
    (-1 (throw-error "Unable to start loop."))))

;; @syntax (stop)
;; Halts the event loop after the next iteration.
(define (stop)
  (unless (zero? (event_base_loopbreak BASE))
    (throw-error "Unable to halt event loop."))
  (setf RUNNING nil)
  (cleanup))

;-------------------------------------------------------------------------------
; Event callback triggering
;-------------------------------------------------------------------------------
(define (event-id , id)
  "Generates an id for the event, anchored in memory using a tree, that is used
  to locate the event object from the callback."
  (setf id (string (inc _event_id)))
  (EventID id id) ; anchor in memory
  (list (EventID id) (address (EventID id))))

(define (trigger fd ev arg , id event cb)
  "Helper function that is called by libevent and calls the user-supplied
  callback."
  (setf id (get-string arg))
  (map set '(event cb) (EventCB id))
  (cb fd ev id)
  0)

; Create callback for libevent
(setf _event_cb (callback 'trigger "void" "int" "short int" "void*"))

(define (make-event fd ev cb once timeval, id event id-address)
  "Wrapper for event_new and event_add."
  (assert-initialized)

  (unless once (setf ev (| ev PERSIST)))

  (map set '(id id-address) (event-id))
  (setf event (event_new BASE fd ev _event_cb id-address))
  (EventCB id (list event cb))

  (setf timeval
    (if timeval
      (pack TIMEVAL 0 (* 1000 timeval)) ; convert usec to msec
      0))

  (unless (zero? (event_add event (address timeval)))
    (throw-error "Error adding event"))

  id)

;-------------------------------------------------------------------------------
; Event registration
;-------------------------------------------------------------------------------
;; @syntax (watch <fd> <ev> <cb> <once>)
;; @param <int>  'fd'   An open file descriptor
;; @param <int>  'ev'   A bitmask of event constants
;; @param <fn>   'cb'   A callback function
;; @param <bool> 'once' When true (default false) callback is triggered only once
;; @return <string> id used to manage the event watcher
;; Registers callback function <cb> to be called whenever an event masked in
;; <ev> is triggered for <fd>. <cb> is called with the file descriptor,
;; event, and id as its arguments.
;;
;; @example
;; (watch socket (| READ WRITE)
;;   (fn (fd e)
;;     (cond
;;       (== e READ) (...)
;;       (== e WRITE) (...))))
(define (watch fd ev cb once , id event id-address)
  (assert-initialized)
  (make-event fd ev cb once))

;; @syntax (unwatch <id>)
;; @param <string> 'id' ID returned by <watch>
;; Unregisters an event watcher. Once unwatched, the watcher id is invalid
;; and may no longer be used.
;;
;; @example
;; (watch socket WRITE
;;   (lambda (fd e id)
;;     (unwatch id)
;;     (write fd "Hello world")))
(define (unwatch id , event cb)
  (assert-initialized)
  (map set '(event cb) (EventCB id))
  (event_del event)
  (event_free event))

;; @syntax (watch-once <fd> <ev> <cb>)
;; @param <int> 'fd' An open file descriptor
;; @param <int> 'ev' A bitmask of event constants
;; @param <fn>  'cb' A callback function
;; Registers a callback <cb> for events <ev> on descriptor <fd>. After the
;; callback is triggered, it is automatically unregistered for events <ev>.
;; For example, the example code from <unwatch> could be rewritten as:
;;
;; @example
;; (watch-once socket WRITE
;;   (lambda (fd e)
;;     (write fd "Hello world")))
(define (watch-once fd ev cb)
  (watch fd ev cb true))

;-------------------------------------------------------------------------------
; Timers
;-------------------------------------------------------------------------------
;; @syntax (set-interval <msec> <cb>)
;; @param <int> 'msec' Millisecond interval
;; @param <fn>  'cb'   A callback function
;; @return <string> Returns the timer id
;; Registers a callback <cb> to be executed every <msec> milliseconds. Note
;; that the timing is not guaranteed; <cb> will be called on the first
;; iteration of the event loop after <msec> milliseconds have passed since its
;; last execution. Returns an event ID that may be used to clear the interval
;; event using <clear-interval>.
;;
;; @example
;; (set-interval 500 (fn () (println "Another 500ms have passed")))
(define (set-interval msec cb)
  (assert-initialized)
  (make-event -1 (| 0 PERSIST) cb nil msec))

;; @syntax (clear-interval <id>)
;; @param <string> 'id' id of a timer event
;; Clears an interval id.
;;
;; @example
;; (setf n 10)
;; (set-interval 500
;;   (fn (fd e id) ; fd is nil and e is TIMEOUT
;;     (when (zero? (dec n))
;;       (clear-interval id))))
(define (clear-interval id)
  (unwatch id))

;; @syntax (set-timer <msec> <cb>)
;; @param <int> 'msec' Millisecond interval
;; @param <fn>  'cb'   A callback function
;; @return <string> Returns the timer id
;; Registers a callback <cb> to be executed one time after <msec> milliseconds.
;;
;; @example
;; (set-timer 500 (fn () (println "500ms have elapsed.")))
(define (set-timer msec cb)
  (assert-initialized)
  (make-event -1 0 cb nil msec))

;-------------------------------------------------------------------------------
; Buffered IO
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;Wrapper functions
;-------------------------------------------------------------------------------
(define (buffer-create socket , buffer)
  (assert-initialized)
  (setf buffer (bufferevent_socket_new BASE socket BUFFER_OPT_DEFER_CALLBACKS))
  (and (not (zero? buffer)) buffer))

(define (buffer-free buffer)
  (bufferevent_free buffer))

(define (buffer-enable buffer ev)
  (assert-initialized)
  (zero? (bufferevent_enable buffer ev)))

(define (buffer-disable buffer ev)
  (assert-initialized)
  (zero? (bufferevent_disable buffer ev)))

(define (buffer-read buffer (chunk-size DEFAULT_CHUNK_SIZE) , buf bytes)
  (assert-initialized)
  (setf buf (dup "\000" (+ 10 chunk-size)))
  (setf bytes (bufferevent_read buffer buf chunk-size))
  (list bytes (get-string buf)))

(define (buffer-write buffer data)
  (assert-initialized)
  (zero? (bufferevent_write buffer data (length data))))

;-------------------------------------------------------------------------------
;Buffered IO - callbacks
;-------------------------------------------------------------------------------
(define (_buffer_read buffer ctx , (bytes 1) buf id trigger?)
  (setf id (get-string ctx))

  (while (> bytes 0)
    (map set '(bytes buf) (buffer-read buffer))
    (write (BufferData id) buf)
    (setf trigger? true))

  (when trigger?
    (trigger-buffer-read id))

  0)

(define (_buffer_write buffer ctx)
  (buffer-disable buffer WRITE)
  0)

(define (_buffer_event buffer ev ctx , id)
  (setf id (get-string ctx))

  ; Connection terminated
  (when (masks? ev BUFFER_EOF)
    (trigger-buffer-read id))

  (unless (masks? ev BUFFER_CONNECTED)
    (trigger-buffer-error id ev))
  0)

(setf _buffer_read_cb  (callback '_buffer_read "void" "void*" "void*"))
(setf _buffer_write_cb (callback '_buffer_write "void" "void*" "void*"))
(setf _buffer_event_cb (callback '_buffer_event "void" "void*" "short int" "void*"))

(define (buffer-setcb buffer ctx)
  (assert-initialized)
  (bufferevent_setcb buffer _buffer_read_cb _buffer_write_cb _buffer_event_cb ctx))

(define (trigger-buffer-read id , marker on-success _ idx len)
  (when (BufferCB id)
    (map set '(marker on-success _) (BufferCB id))

    ; if marker is set, find it in the data
    (if marker
      (let ((found (regex marker (BufferData id) 0x10000)))
        (when found
          (map set '(_ idx len) found)))
      (setf idx 0 len 0))

    ; if the marker was found (or was set to nil), call the on-success
    ; callback with that slice of the data, removing it from the buffer.
    (when idx
      (on-success (0 (+ idx len) (BufferData id)))
      (setf (BufferData id) ((+ idx len) (BufferData id))))))

(define (trigger-buffer-error id ev , buffer data marker _ on-event)
  (map set '(marker _ on-event) (BufferCB id))
  (setf data (BufferData id))
  (setf buffer (BufferEv id))

  ; Clean up
  (buffer-disable buffer (| READ WRITE))
  (free-buffer id)

  ; Callback
  (on-event ev data))

;-------------------------------------------------------------------------------
;Buffered IO - API
;-------------------------------------------------------------------------------
(define (buffer-id, id)
  (setf id (string (inc _buffer_id)))
  (BufferID id id) ; anchor in memory
  (list (BufferID id) (address (BufferID id))))

(define (get-buffer id)
  (BufferEv id))

(define (assert-buffer id)
  (unless (get-buffer id) (throw-error "Invalid buffer id")))

;; @syntax (make-buffer <socket> <read-marker> <on-read> <on-event>)
;; @param <int>    'socket' an open socket; must not be a pipe
;; @param <regex>  'read-marker' a compiled regex
;; @param <fn>     'on-read'
;; @param <fn>     'on-event'
;; @return <string> an id used to identify the buffer
;; Creates a new buffer object. Configures buffer to call <on-read> whenever
;; the buffer is able to match its contents against pre-compiled regex
;; <read-marker>. <on-event> is triggered in the event of a disconnected
;; socket, error, etc.
(define (make-buffer socket read-marker on-read on-event, id id-address buffer)
  (assert-initialized)
  (map set '(id id-address) (buffer-id))

  ; create buffer
  (setf buffer (buffer-create socket))

  ; configure buffer
  (bufferevent_setcb buffer _buffer_read_cb _buffer_write_cb _buffer_event_cb id-address)

  ; store buffer
  (BufferData id "")   ; prepare input storage
  (BufferEv id buffer) ; store buffer

  ; configure buffer
  (BufferCB id (list read-marker on-read on-event))
  (when on-read
    (buffer-enable (get-buffer id) READ))

  id)

;; @syntax (free-buffer <id>)
;; @param <string> 'id' buffer id
;; Cleans up after a buffer. The buffer is not usable after calling this
;; routine.
(define (free-buffer id)
  (assert-buffer id)
  (buffer-free buffer)
  (BufferData id nil)
  (BufferCB id nil)
  (BufferID id nil)
  (BufferEv id nil))

;; @syntax (buffer-send <id> <data>)
;; @param <string> 'id' buffer id
;; @param <string> 'data' data to send
;; Queues <data> to be sent along the socket transport of buffer <buffer-id>.
(define (buffer-send id data)
  (assert-initialized)
  (assert-buffer id)
  (buffer-write (get-buffer id) data)
  (buffer-enable (get-buffer id) WRITE))

(context 'MAIN)


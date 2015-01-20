;; @module memcached
;; @author Jeff Ober <jeffober@gmail.com>
;; @version 0.3
;; @location http://static.artfulcode.net/newlisp/memcached.lsp
;; @package http://static.artfulcode.net/newlisp/memcached.qwerty
;; @description Interface to libmemcached (http://tangent.org/552/libmemcached.html) (updated for newlisp 10)
;; <p>This module is a work-in-progress. Currently implemented functions work (or at least they
;; appear to). The full range of functionality is not nearly implemented, but it works well enough
;; to allocate, connect, get/set keys, and disconnect/deallocate.</p>
;; <h4>External libraries</h4>
;; &bull;
;; @link http://tangent.org/552/libmemcached.html libmemcached<br>
;; &bull;
;; @link http://www.danga.com/memcached/download.bml memcached<br>
;; &bull;
;; @link http://monkey.org/~provos/libevent/ libevent&nbsp;(required&nbsp;by&nbsp;memcached)
;; 
;; <h4>Version history</h4>
;; <b>0.3</b>
;; &bull; updated for newlisp 10
;; 
;; <b>0.2</b>
;; &bull; cleaned up some functions
;; &bull; added 'get-keys'
;; 
;; <b>0.1</b>
;; &bull; development release
;; 
;; @example
;; (memcached:init)
;; (memcached:add-server "localhost" 11211)
;; (memcached:set-key "foo" "bar" 30)
;; (memcached:get-key "foo") ; within 30 seconds
;; => "bar"
;; (sleep 30000) ; wait 30 seconds
;; (memcached:get-key "foo") ; after 30+ seconds
;; => nil
;; (memcached:disconnect)

(context 'memcached)

;;; note: technique robbed from mysql5.lsp

(setq files '("/usr/lib64/libmemcached.so.11" "/usr/lib64/libmemcached.so" "/usr/lib/i386-linux-gnu/libmemcached.so.10" "/usr/lib/x86_64-linux-gnu/libmemcached.so" "/usr/lib/libmemcached.so" "/usr/local/lib/libmemcached.so" "/usr/local/lib/libmemcached.dylib"))

(setq libmemcached
  (files (or (find true (map file? files)) 
				     (begin (println "cannot find libmemcached library")
	                  (exit)))))

(import libmemcached "memcached_create")
(import libmemcached "memcached_free")
(import libmemcached "memcached_server_add")
(import libmemcached "memcached_strerror")
(import libmemcached "memcached_quit")
(import libmemcached "memcached_set")
(import libmemcached "memcached_get")
(import libmemcached "memcached_mget")
(import libmemcached "memcached_fetch")
(import libmemcached "memcached_result_create")

(setq MEMCACHED nil)
(setq MEMCACHED_RETURN nil)
(setq ERROR nil)

;; @syntax (memcached:init)
;; <p>Initializes the 'memcached' module.</p>
(define (init)
  (if MEMCACHED (memcached_free MEMCACHED))
  (setq MEMCACHED (memcached_create 0))
  (if (zero? MEMCACHED) (setq MEMCACHED nil))
  (not (nil? MEMCACHED)))

;; @syntax (memcached:disconnect)
;; <p>Disconnects from all servers and deallocates libmemcached structures.</p>
(define (disconnect)
  (when MEMCACHED
    (memcached_quit MEMCACHED)
    (memcached_free MEMCACHED)
    true))

;; @syntax (memcached:add-server <str-host> <int-port>)
;; @param <str-host> the hostname; required
;; @param <int-port> the host port; required
;; <p>Adds a server to be used as a source of cached data. Returns true or nil,
;; depending on whether the server was successfully added or not.</p>
;; @example
;; (memcached:add-server "localhost" 8000)
;; => true
(define (add-server host port)
  (when MEMCACHED
    (setq MEMCACHED_RETURN (memcached_server_add MEMCACHED host port))
    (= "SUCCESS" (result))))

;; @syntax (memcached:result)
;; <p>Returns the result or error from the last operation.</p>
(define (result)
  (if (and MEMCACHED MEMCACHED_RETURN)
      (get-string (memcached_strerror MEMCACHED MEMCACHED_RETURN))))

;; @syntax (memcached:set-key <str-key> <expr-value> [<int-expiration>])
;; @param <str-key> unique key to store <expr-value> under; required
;; @param <str-expr> value to store under <str-key>; required
;; @param <int-expiration> seconds until <str-key> will expire; optional
;; <p>Sets <str-key> to <str-expr> on the memcached server. <str-expr> will be serialized
;; using 'string'. Keys that already exist are overwritten. Returns true for success,
;; nil for failure.</p>
;; @example
;; (memcached:set-key "foo" "bar" 30) ; sets "foo" to "bar" for 30 seconds
;; => true
(define (_set-key key value expiration)
  (when MEMCACHED
    (setq key (string key))
    (setq value (string value))
    (setq MEMCACHED_RETURN (memcached_set MEMCACHED key (length key)
                                                    value (length value)
                                                    expiration nil))
    (= "SUCCESS" (result))))

(define (set-key key value expiration)
  (if (_set-key key value expiration)
      value))

;; @syntax (memcached:get-key <str-key>)
;; @param <str-key> the key to retrieve; required
;; <p>Retrieves the value associated with <str-key> from the memcached server. If the
;; key does not exist or has expired, evaluates to nil. Otherwise, the string value
;; is returned.</p>
;; @example
;; (memcached:set-key "foo" '("bar" "baz" "bat") (* 60 60))
;; => true
;; 
;; (memcached:get-key "foo")
;; => "(\"bar\" \"baz\" \"bat\")"
;; 
;; (let ((res (memcached:get-key "foo")))
;;   (if res (eval-string (string "'" res)))) ; evaluate quoted
;; => ("bar" "baz" "bat")
(define (get-key key , res (value-length 0) (flags 0))
  (when MEMCACHED
    (setq res (memcached_get MEMCACHED
                             key (length key)
                             (address value-length)
                             (address flags)
                             (address MEMCACHED_RETURN)))
    (unless (zero? res) (get-string res))))

(define (_fetch key , klen vlen flags res)
  (setq klen (address (length key))
        vlen (address 0)
        flags (address 0))
  (setq res (memcached_fetch MEMCACHED key klen vlen flags (address MEMCACHED_RETURN)))
  (if (= 0 res) nil (get-string res)))

;; @syntax (memcached:get-keys <list-keys>)
;; @param <list-keys> a list of strings
;; <p>Fetches an association list of '(key value) pairs from the
;; server.  Invalid or expired values return nil.</p>
;; @example
;; (memcached:set-key "foo" "bar" 300)
;; => "bar"
;; (memcached:set-key "baz" "bat" 300)
;; => "bat"
;; (memcached:set-key "asdf" "qwerty" 300)
;; => "qwerty"
;; 
;; (memcached:get-keys '("foo" "baz" "asdf")
;; => (("foo" "bar") ("baz" "bat") ("asdf" "qwerty"))
;; 
;; (memcached:get-keys '("foo" "invalid" "expired"))
;; => (("foo" "bar") ("invalid" nil) ("expired" nil))
(define (get-keys list-keys , res num-keys s-keys keys len-s-keys lengths)
  (setq MEMCACHED_RETURN nil)
  (when MEMCACHED
    (setq num-keys (length list-keys)
          s-keys (map string list-keys)
          keys (pack (dup "lu" num-keys) s-keys)
          len-s-keys (map length s-keys)
          lengths (pack (dup "lu" num-keys) len-s-keys)
          MEMCACHED_RETURN (memcached_mget MEMCACHED keys lengths num-keys))
    (when (= (result) "SUCCESS")
      (setq res '())
      (dolist (key list-keys)
        (push (list key (_fetch key)) res -1))))
  res)

(context 'MAIN)


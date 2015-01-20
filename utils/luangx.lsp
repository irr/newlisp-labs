#!/usr/bin/env newlisp

(define nginx [text]
    worker_processes  1;

    error_log  logs/error.log notice;
    pid        logs/nginx.pid;

    events {
        worker_connections 1024;
    }

    http {
        default_type       application/octet-stream;
        access_log         logs/access.log combined;
        sendfile           on;
        keepalive_timeout  10;

        lua_package_path   '$lpath;/usr/local/openresty/lualib/?.lua;;';
        lua_package_cpath  '$cpath;;';

        server {
            listen         $port;
            server_name    localhost;

            location / {
                content_by_lua_file "$file";
            }
        }
    }
[/text])

(define (fix path)
    (if (= path nil) "" (string path)))

(define (template str pairs)
    (dolist (pair pairs)
        (set 'str (replace (first pair) str (last pair)))) str)

(define (get-port, port)
    (set 'port (+ (rand 5000) 60000))
    (do-while (= port 0) 
        (if (= 0 (exec (format "nc -z localhost %d" port))) (set 'port 0)))
    port)

(define (luangx file lpath cpath)
    (letn ((temp  (first (exec "mktemp -d")))
           (logs  (format "%s/logs" temp))
           (conf  (format "%s/conf" temp))
           (ngxf  (format "%s/nginx.conf" conf))
           (pidf  (format "%s/nginx.pid" logs))
           (fname (first (exec (format "readlink -f %s" file))))
           (dname (first (exec (format "dirname %s" fname))))
           (port  (string (get-port))))
        (dolist (s (list temp logs conf)) (make-dir s 0755))
        (write-file ngxf (template nginx (list (cons "$file" file) 
                                               (cons "$port" port)
                                               (cons "$lpath" lpath)
                                               (cons "$cpath" cpath))))
        (exec (format "cp -r %s/. %s/" dname temp))
        (exec (format "nginx -c %s -p %s" ngxf temp))
        (dolist (l (exec (format "curl http://localhost:%s/ 2>/dev/null" port))) (println l))
        (exec (format "kill %s" (read-file pidf)))
        (exec (format "rm -rf %s" temp))))

(if (= (main-args 2) nil)
    (begin (print "usage: luangx <main-file> [lpath] [cpath]\n") (exit 1))
    (let ((file (string (main-args 2)))
          (lpath (fix (main-args 3)))
          (cpath (fix (main-args 4))))
        (seed (time-of-day))
        (luangx file lpath cpath)))

(exit 0)

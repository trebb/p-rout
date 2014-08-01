#!/usr/bin/guile -s
!#

;;;; Copyright (c) 2014 Bert Burgemeister  trebbu@googlemail.com
;;;; 
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;; 
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.


;;;; We handle POST requests whose URI ends with ".json".  Suppose we
;;;; are receiving a request with a URI path of /some/path/foo.json
;;;; and a body like this:
;;;;
;;;;  { "table_one" :
;;;;    [ { "column1" : 5001,
;;;;        "column2" : 1
;;;;      }, 
;;;;      { "column1" : 9,
;;;;        "column3" : 1,
;;;;        "column4" : 342
;;;;      }
;;;;    ],
;;;;    "table_two" : 
;;;;    { "column1" : 3,
;;;;      "column2" : "2014-04-20T22:09:15+01:00"
;;;;    },
;;;;    "third_table" : "more stuff"
;;;;  }
;;;;
;;;; The following will happen:
;;;;
;;;;   - Store two rows in foo.table_one.
;;;;       (1) set column1 = "5001", column2 = "1"
;;;;       (2) set column1 = "3", column3 = "1", column4 = "342"
;;;;   - Store one row in foo.table_two.
;;;;       (1) set column1 = "3", column2 = "2014-04-20T22:09:15+01:00"
;;;;   - Store one row in foo.third_table.
;;;;       (1) set column data = "more stuff"
;;;;   - Every table has an additional column whose name is in
;;;;     +record-id-column+ which is unique for each record.
;;;;   - Database schemas ("foo" in this example) and tables will be
;;;;     created and columns will be added as necessary.

;; On Debian, we may need to
;; wget http://download.savannah.gnu.org/releases/guile-json/guile-json-0.3.1.tar.gz,
;; wget http://download.gna.org/guile-dbi/guile-dbi-2.1.5.tar.gz,
;; wget http://download.gna.org/guile-dbi/guile-dbd-postgresql-2.1.4.tar.gz
;; ./configure; make; make install, respectively,
;; ldconfig,
;; and to
;; (add-to-load-path "/usr/local/share/guile/site/")

(use-modules (web server)
	     (web request)
	     (web response)
	     (web uri)
	     (rnrs bytevectors)
	     (json)
	     (dbi dbi)
	     (srfi srfi-1)
	     (srfi srfi-19)
	     (ice-9 getopt-long)
	     (ice-9 pretty-print)
	     (ice-9 match)
	     (ice-9 rdelim))

;;; Return first line of file at path, or return #f
(define (read-line-from-file path)
  (catch
    #t
    (lambda () (call-with-input-file path (lambda (in) (read-line in))))
    (lambda (k . args) #f)))

(define option-spec
  '((help (single-char #\h))
    (version (single-char #\V))
    (verbose (single-char #\v))
    (addr (single-char #\a) (value #t))
    (port (single-char #\p) (value #t))
    (db-connection (value #t))
    (log-dir (value #t))
    (no-daemon)
    (pid-file (value #t))))

(define options (getopt-long (command-line) option-spec))

(when (option-ref options 'help #f)
  (display (car (command-line)))
  (display "\
 [options]
  -h, --help      Display this help
  -V, --version   Display version number
  -v, --verbose   Display debugging output
  -a, --addr      Address to listen on
  -p, --port      Port to listen on
  --db-connection <user>:<pass>:<db>:<path/ip>[:port] (default:
                    p-rout:p-rout:p_rout:/run/postgres:localhost)
  --log-dir       Log directory (default: \"log\")
  --no-daemon     Remain in foreground
  --pid-file      Store daemon's PID here (default:
                    /var/run/p-rout/p-rout-collect.pid)
")
  (exit))

(when (option-ref options 'version #f)
  (display (or (read-line-from-file "./VERSION")
	       (read-line-from-file "/usr/share/p-rout/VERSION")
	       "unknown"))
  (newline)
  (exit))

(define +record-id-column+ "p_rout_id")
(define +verbose+ (option-ref options 'verbose #f))
(define +db-connection+
  (option-ref options
	      'db-connection
	      "p-rout:p-rout:p_rout:/run/postgres:localhost"))
(define +log-dir+ (option-ref options 'log-dir "log"))
(define +addr+ (option-ref options 'addr "10.11.0.3"))
(define +port+ (string->number (option-ref options 'port "80")))
(define +no-daemon+ (option-ref options 'no-daemon #f))
(define +pid-file+ (option-ref options 'pid-file "/var/run/p-rout/p-rout-collect.pid"))
(define *db* #f)

;;; The main HTTP handler
(define (p-rout-collector request body)
  (cond ((eq? (request-method request) 'POST)
	 (json-handler request body))
	(else (not-found-handler request body))))

;;; Unanticipated access
(define (not-found-handler request body)
  (file-log #f "Unexpected request:" request body)
  (values (build-response #:code 404)
	  (string-append "Not found: "
			 (uri->string (request-uri request)))))

;;; The purpose of this HTTP server
(define (json-handler request body)
  (when +verbose+
    (file-log
     "json-handler" "Handling request " request (utf8->string body)))
  (store-record (uri-path (request-uri request))
		(json-string->scm (utf8->string body)))
  (values (build-response #:code 201
			  #:reason-phrase "Created"
			  #:headers `((content-type . (application/json)) 
				      (charset . "utf-8")
				      (server . "nginx")
				      (cache-control . (no-cache 
							private 
							(max-age . 0)
							must-revalidate))
				      (date . ,(current-date))
				      (connection . (keep-alive))))
	  (scm->json-string (json (object ("status" "ok")
					  ("next-log-level" 2))))))

;;; Convert JSON hashtable into a list structure
(define (ht->l x)
  (cond
   ((hash-table? x)
    (hash-map->list (lambda (a b) (cons a (ht->l b))) x))
   ((list? x)
    (map (lambda (item) (ht->l item)) x))
   (else x)))

;;; Extract a list of toplevel names from JSON hashtable
(define (ht->tablenames json)
  (map car (ht->l json)))

;;; Extract from JSON hashtable data for one table
(define (ht->table-content json tablename)
  (cdar (filter (lambda (x) (equal? tablename (car x)))
		(ht->l json))))

;;; Put a log entry into file +log-dir+/<basename>.log
(define (file-log basename . message-parts)
  (system* "mkdir" "-p" +log-dir+)
  (let ((logfile
	 (string-append +log-dir+ "/" (or basename "unexpected") ".log"))
	(out #f))
    (dynamic-wind
      (lambda () (set! out (open-file logfile "a")))
      (lambda ()
	(display (now) out)
	(when +verbose+
	  (display (now)))
	(for-each
	 (lambda (part)
	   (display " " out)
	   (display part out)
	   (when +verbose+
	     (display " ")
	     (display part)))
	 message-parts)
	(newline out)
	(when +verbose+
	  (newline)))
      (lambda () (close out)))))

(define (now) (date->string (current-date) "~4"))

(define (dot-append . strings) (string-join strings "."))

;;; Send SQL query to database
;;; ignore-codes are expected database error codes that don't cause
;;; log entries
(define (logged-query logfile query . ignore-codes)
  (when +verbose+
    (file-log logfile query))
  (dbi-query *db* query)
  (match (dbi-get_status *db*)
    ((code . message)
     (if (member code `(0 ,@ignore-codes))
	 #f
	 (begin
	   (file-log logfile message)
	   (cons code message))))
    (unexpected
     (file-log logfile "weird status message: " unexpected)
     unexpected)))

;;; guile-dbd-postgresql v2.1.4 doesn't do anything until we've read
;;; any previous results
(define (flush-query)
  (while (dbi-get_row *db*)))

;;; Make a bunch of SQL tables if necessary
(define (create-tables schema tablenames)
  (logged-query "db" (string-append "CREATE SCHEMA IF NOT EXISTS " schema))
  (flush-query)
  (for-each
   (lambda (tablename)
     (logged-query (dot-append schema tablename)
		   (string-join `("CREATE TABLE IF NOT EXISTS"
				  ,(dot-append schema tablename)
				  "(" ,+record-id-column+ "integer)")))
     (flush-query))
   tablenames))

(define (column-names schema table)
  (logged-query (dot-append schema table)
		(string-join
		 `("SELECT * FROM" ,(dot-append schema table) "LIMIT 1")))
  (let ((result (dbi-get_row *db*)))
    (flush-query)
    (if result
	(map car result)
	'())))

;;; Add a bunch of columns to an SQL table
(define (add-columns schema table columnnames)
  (let* ((present-columns (column-names schema table))
	 (missing-columns (lset-difference
			   string= columnnames present-columns)))
    (for-each
     (lambda (columnname)
       (logged-query (dot-append schema table)
		     (string-join `("ALTER TABLE"
				    ,(dot-append schema table)
				    "ADD COLUMN" ,columnname "text")))
       (flush-query))
     missing-columns)))

;;; Extract from table-content the set of column names
(define (columnnames table-content)
  (let* ((result '())
	 (collect-columnnames
	  (lambda (name-value-pair)
	    (set! result
		  (cons (car name-value-pair) result)))))
    (map-rows collect-columnnames table-content)
    (sort (delete-duplicates result) string<?)))

;;; Apply function of (columnname . value) to table-content
(define (map-rows function table-content)
  (match table-content
    ((((string . _) ...) ...)
     (map
      (lambda (array-element) 
	(map function array-element))
      table-content))
    (((string . _) ...)
     (map function table-content))
    (something-else
     (list (function (cons "data" something-else))))))

;;; Add a row to SQL table
(define (add-row schema table columnnames values record-id)
  (let ((names (string-join (cons +record-id-column+ columnnames) ", "))
	(vals (string-join
	       (map (lambda (val)
		      (if (number? val)
			  (number->string val)
			  (string-join `("'" ,val "'") "")))
		    (cons record-id values))
	       ", ")))
    (logged-query (dot-append schema table)
		  (string-join `("INSERT INTO"
				 ,(dot-append schema table)
				 "(" ,names ") VALUES (" ,vals ")")))
    (flush-query)))

;;; "/some/path/foo.json" -> "foo"
(define (uri-path->basename uri-path)
  (let ((uri-extension ".json")
	(filename (last (split-and-decode-uri-path uri-path))))
    (if (string-suffix? uri-extension filename)
	(string-drop-right filename (string-length uri-extension))
	#f)))
  
;;; Put data from JSON payload into SQL database whose name is derived
;;; from uri-path
(define (store-record uri-path payload)
  (let ((schema (uri-path->basename uri-path)))
    (if schema
	(add-record schema payload)
	(file-log #f "Unexpected POST request:" uri-path))))

;;; Store one record into SQL database
(define (add-record schema json)
  (let ((tablenames (ht->tablenames json)))
    (create-tables schema tablenames)
    (logged-query "db" "BEGIN TRANSACTION ISOLATION LEVEL SERIALIZABLE")
    (flush-query)
    (let ((next-id
	   (apply
	    max 
	    (map
	     (lambda (table)
	       (logged-query (dot-append schema table)
			     (string-join `("SELECT max("
					    ,+record-id-column+
					    ") AS last_id FROM"
					    ,(dot-append schema table))))
	       (let ((last-id (dbi-get_row *db*)))
		 (flush-query)
		 (match last-id
		   ((("last_id" . latest-row))
		    (if (number? latest-row)
			(+ 1 latest-row)
			0))	  
		   (_ 0))))
	     tablenames))))
      (for-each
       (lambda (table)
	 (let ((table-content (ht->table-content json table)))
	   (add-columns schema
	   		table
	   		(columnnames table-content))
	   (let ((names (map-rows car table-content))
		 (values (map-rows cdr table-content)))
	     (match names
	       (((_ ...) ...)
		(for-each
		 (lambda (names values)
		   (add-row schema table names values next-id))
		 names values))
	       ((_ ...)
		(add-row schema table names values next-id))))))
       tablenames))
    (logged-query "db" "COMMIT TRANSACTION")
    (flush-query)))

(unless +no-daemon+
  (let ((pid (primitive-fork)))
    (cond ((> pid 0)
	   (with-output-to-file +pid-file+
	     (lambda () (display pid)))
	   (primitive-exit 0))
	  ((< pid 0)
	   (primitive-exit 1))
	  (else
	   (umask 0)
	   (setsid)))))

(dynamic-wind
  (lambda () (set! *db* (dbi-open "postgresql" +db-connection+)))
  (lambda ()
    (logged-query "db" "SET client_min_messages = WARNING;")
    (run-server p-rout-collector
		'http
		`(#:port ,+port+ #:addr ,(inet-pton AF_INET +addr+))))
  (lambda () (dbi-close *db*)))


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
;;;;   - Open sqlite3 database foo.
;;;;   - Store two rows in table_one.
;;;;       (1) set column1 = 5001, column2 = 1
;;;;       (2) set column1 = 3, column3 = 1, column4 = 342
;;;;   - Store one row in table_two.
;;;;       (1) set column1 = 3, column2 = "2014-04-20T22:09:15+01:00"
;;;;   - Store one row in third_table.
;;;;       (1) set column data = "more stuff"
;;;;   - Every table has an additional column whose name is in
;;;;     +record-id-column+ which is unique for each record.
;;;;   - Databases and tables will be created and columns will be
;;;;     added as necessary.

;; On Debian, we may need to
;; wget http://download.savannah.gnu.org/releases/guile-json/guile-json-0.3.1.tar.gz,
;; wget http://download.gna.org/guile-dbi/guile-dbi-2.1.5.tar.gz,
;; wget http://download.gna.org/guile-dbi/guile-dbd-sqlite3-2.1.4.tar.gz,
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
	     (ice-9 threads))

(define option-spec
  '((help (single-char #\h))
    (verbose (single-char #\v))
    (addr (single-char #\a) (value #t))
    (port (single-char #\p) (value #t))
    (db-dir (value #t))
    (log-dir (value #t))
    (no-daemon)))

(define options (getopt-long (command-line) option-spec))

(when (option-ref options 'help #f)
  (display (car (command-line)))
  (display "\
 [options]
  -h, --help     Display this help
  -v, --verbose  Display debugging output
  -a, --addr     Address to listen on
  -p, --port     Port to listen on
  --db-dir       Database directory (default: \"log-db\")
  --log-dir      Log directory (default: \"log-db\")
  --no-daemon    Remain in foreground
")
  (exit))

(define +record-id-column+ "pur_r_id")
(define +verbose+ (option-ref options 'verbose #f))
(define +db-dir+ (option-ref options 'db-dir "log-db"))
(define +log-dir+ (option-ref options 'log-dir "log-db"))
(define +addr+ (option-ref options 'addr "10.11.0.3"))
(define +port+ (option-ref options 'port 80))
(define +no-daemon+ (option-ref options 'no-daemon #f))

;;; The main HTTP handler
(define (p-rout-collector request body)
  (cond ((eq? (request-method request) 'POST)
	 (json-handler request body))
	(else (not-found-handler request body))))

;;; Unanticipated access
(define (not-found-handler request)
  (file-log #f "Unexpected request:" request body)
  (values (build-response #:code 404)
	  (string-append "Not found: "
			 (uri->string (request-uri request)))))

;;; The purpose of this HTTP server
(define (json-handler request body)
  (when +verbose+
    (file-log "json-handler" "Handling request " request (utf8->string body)))
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
  (system (string-append "mkdir -p " +log-dir+))
  (let ((logfile (string-append +log-dir+ "/" (or basename "unexpected") ".log"))
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

;;; Send SQL query to database
;;; ignore-codes are expected database error codes that don't cause
;;; log entries
(define (logged-query db logfile query . ignore-codes)
  (when +verbose+
    (file-log logfile query "  sent to  " db))
  (dbi-query db query)
  (match (dbi-get_status db)
    ((code . message)
     (if (member code `(0 ,@ignore-codes))
	 #f
	 (begin
	   (file-log logfile message)
	   (cons code message))))
    (unexpected
     (file-log logfile "weird status message: " unexpected)
     unexpected)))

;;; Make a bunch of SQL tables if necessary
(define (create-tables db db-name tablenames)
  (for-each
   (lambda (tablename)
     (logged-query db db-name
		   (string-join `("CREATE TABLE IF NOT EXISTS"
				  ,tablename "(pur_r_id INTEGER)"))))
   tablenames))

;;; Add a bunch of columns to an SQL table
(define (add-columns db table logfile-basename columnnames)
  (for-each
   (lambda (columnname)
     (logged-query db logfile-basename
		   (string-join `("ALTER TABLE" ,table
				  "ADD COLUMN" ,columnname))
      1))  ; ignore duplicate column name error
   columnnames))

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
(define (add-row db db-name table columnnames values record-id)
  (let ((names (string-join (cons +record-id-column+ columnnames) ", "))
	(vals (string-join
	       (map (lambda (val)
		      (if (number? val)
			  (number->string val)
			  (string-join `("'" ,val "'") "")))
		    (cons record-id values))
	       ", ")))
    (logged-query db (string-append db-name "-" table)
		  (string-join `("INSERT INTO" ,table
				 "(" ,names ") VALUES (" ,vals ")")))))

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
  (let ((basename (uri-path->basename uri-path)))
    (if basename
	(begin
	  (system (string-append "mkdir -p " +db-dir+))
	  (let ((dbfile (string-append +db-dir+ "/" basename ".sqlite3"))
		(db #f))
	    (dynamic-wind
	      (lambda () (set! db (dbi-open "sqlite3" dbfile)))
	      (lambda () (add-record db basename payload))
	      (lambda () (dbi-close db)))))
	(file-log #f "Unexpected POST request:" uri-path))))

;;; Store one record into SQL database
(define (add-record db db-name json)
  (let ((tablenames (ht->tablenames json)))
    (create-tables db db-name tablenames)
    (logged-query db "db" "BEGIN IMMEDIATE TRANSACTION")
    (let ((next-id
	   (apply
	    max 
	    (map
	     (lambda (table)
	       (logged-query db (string-append db-name "-" table)
			     (string-join `("SELECT max(" ,+record-id-column+
					    ") AS last_id FROM" ,table)))
	       (match (dbi-get_row db)
		 ((("last_id" . latest-row))
		  (if (number? latest-row)
		      (+ 1 latest-row)
		      0))	  
		 (_ 0)))
	     tablenames))))
      (for-each
       (lambda (table)
	 (let ((table-content (ht->table-content json table)))
	   (add-columns db table (string-append db-name "-" table)
			(columnnames table-content))
	   (let ((names (map-rows car table-content))
		 (values (map-rows cdr table-content)))
	     (match names
	       (((_ ...) ...)
		(for-each
		 (lambda (names values)
		   (add-row db db-name table names values next-id))
		 names values))
	       ((_ ...)
		(add-row db db-name table names values next-id))))))
       tablenames))
    (logged-query db "db" "COMMIT TRANSACTION")))

(unless +no-daemon+
  (let ((pid (primitive-fork)))
    (cond ((> pid 0)
	   (display pid) (newline)
	   (primitive-exit 0))
	  ((< pid 0)
	   (primitive-exit 1))
	  (else
	   (umask 0)
	   (setsid)))))

(run-server p-rout-collector
	    'http
	    `(#:port ,+port+ #:addr ,(inet-pton AF_INET +addr+)))

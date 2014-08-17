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

(use-modules (rnrs bytevectors)
	     (dbi dbi)
	     (srfi srfi-1)
	     (srfi srfi-19)
	     (ice-9 getopt-long)
	     (ice-9 pretty-print)
	     (ice-9 match)
	     (ice-9 rdelim)
	     (ice-9 popen))

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
    (db-connection (value #t))
    (log-dir (value #t))))

(define options (getopt-long (command-line) option-spec))

(when (option-ref options 'help #f)
  (display (car (command-line)))
  (display "\
 [options] <dump-file>.sql.gz
  -h, --help      Display this help
  -V, --version   Display version number
  -v, --verbose   Display debugging output
  --db-connection <user>:<pass>:<db>:<path/ip>[:port] (default:
                    p-rout:p-rout:p_rout:/run/postgres:localhost)
  --log-dir       Log directory (default: \"log-r\")
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
(define +log-dir+ (option-ref options 'log-dir "log-r"))
(define +dump-file+ (let ((args (option-ref options '() #f))) ;default broken
		      (if (null? args) #f (car args))))
(define *db* #f)
(define *temp-db* #f)
(define +temp-db-name-base+ "p_rout_temp")
(define *temp-db-name* #f)

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

;;; Tell whether program is currently running
(define (running-process? program)
  (let ((ps-port #f))
    (dynamic-wind
      (lambda ()
	(set! ps-port
	      (open-input-pipe (string-append "ps -C " program " -o pid="))))
      (lambda ()
	(not (eof-object? (read-line ps-port))))
      (lambda ()
	(close-pipe ps-port)))))

;;; Exit if program is currently running
(define (exit-if-running-process program)
  (when (running-process? program)
    (display
     (string-append "\
There seems to be a running process by the name of " program " \
which may attempt to write into the database during the restore operation.
This could mess up the database; therefore, I'm giving up now.
"))
    (exit)))

;;; Send SQL query to database
;;; ignore-codes are expected database error codes that don't cause
;;; log entries
(define (logged-query dbi-db logfile query . ignore-codes)
  ;; guile-dbd-postgresql v2.1.4 refuses to do anything until we've read
  ;; any previous results
  (while (dbi-get_row dbi-db))
  (when +verbose+
    (file-log logfile query))
  (dbi-query dbi-db query)
  (match (dbi-get_status dbi-db)
    ((code . message)
     (if (member code `(0 ,@ignore-codes))
	 #f
	 (begin
	   (file-log logfile message)
	   (cons code message))))
    (unexpected
     (file-log logfile "weird status message: " unexpected)
     unexpected)))

;;; List of all databases in current DB cluster
(define (databases)
  (logged-query *db*
		"restore"
		"SELECT datname FROM pg_database WHERE datistemplate = false")
  (do ((database (dbi-get_row *db*) (dbi-get_row *db*))
       (databases '()))
      ((not database) databases)
    (set! databases (cons (cdar database) databases))))

;;; Return colon-separated string db-connection as a list
(define (db-connection-list db-connection)
  (string-split +db-connection+ #\:))

(define (db-user)
  (first (db-connection-list +db-connection+)))
(define (db-password)
  (second (db-connection-list +db-connection+)))
(define (db-name)
  (third (db-connection-list +db-connection+)))
(define (db-path-or-ip)
  (fourth (db-connection-list +db-connection+)))
(define (db-host)
  (fifth (db-connection-list +db-connection+)))
(define (db-port)
  (if (= 6 (length (db-connection-list +db-connection+)))
      (sixth (db-connection-list +db-connection+))
      "5432"))

;;; Return database object for a freshly created DB; store DB name
;;; in *temp-db-name*
(define (temp-db)
  (do ((i 0 (1+ i))
       (tempname +temp-db-name-base+))
      ((not (member tempname (databases)))
       (set! *temp-db-name* tempname)
       (logged-query
	*db* "restore" (string-append "CREATE DATABASE " *temp-db-name*))
       (dbi-get_row *db*)		;waiting for the new DB
       (dbi-open "postgresql" (string-join (list (db-user)
						 (db-password)
						 *temp-db-name*
						 (db-path-or-ip)
						 (db-host)
						 (db-port))
					   ":")))
    (set! tempname (string-append +temp-db-name-base+ (number->string i)))))

;;; Pipe gzipped sql-file via psql into db-name
(define (psql-apply-dump db-name sql-file)
  (system (string-append
	   "zcat " sql-file
	   " | psql --username=" (db-user)
	   " --no-password --host=" (db-host)
	   " --port=" (db-port) " "
	   db-name)))

;;; Dump source-db and pipe that via psql into destination-db
(define (dump-into destination-db-name source-db-name)
  (system (string-append
	   "pg_dump --username=" (db-user)
	   " --no-password --host=" (db-host)
	   " --port=" (db-port) " " source-db-name
	   " | psql --username=" (db-user)
	   " --no-password --host=" (db-host)
	   " --port=" (db-port) " "
	   destination-db-name)))

;;; Return a list of all user tables in db that have a +record-id-column+
(define (tables db)
  (logged-query
   db
   "restore"
   (string-append
    "SELECT t.table_schema || '.' || t.table_name"
    " FROM information_schema.tables AS t"
    " JOIN information_schema.columns AS c"
    " USING (table_name, table_schema)"
    " WHERE  table_type = 'BASE TABLE'"
    " AND  t.table_schema NOT IN ('pg_catalog', 'information_schema')"
    " AND c.column_name = '" +record-id-column+ "'"))
  (do ((table (dbi-get_row db) (dbi-get_row db))
       (tables '()))
      ((not table) tables)
    (set! tables (cons (cdar table) tables))))

;;; Return a list of columns of schema.table in db
(define (columns db schema table)
  (logged-query
   db
   "restore"
   (string-append
    "SELECT column_name"
    " FROM information_schema.columns"
    " WHERE table_schema = '" schema "' AND table_name = '" table "'"))
  (do ((column (dbi-get_row db) (dbi-get_row db))
       (columns '()))
      ((not column) columns)
    (set! columns (cons (cdar column) columns))))

;;; Remove rows from schema.table that are duplicates (ignoring column
;;; +record-id-column+), keeping the one with the smallest
;;; +record-id-column+
(define (delete-duplicate-rows db schema table)
  (logged-query
   db
   "restore"
   (string-append
    "DELETE FROM " (dot-append schema table)
    " USING " (dot-append schema table) " p_rout_alias"
    " WHERE "
    (string-join
     (map (lambda (column)
	    (string-append
	     "(" (dot-append schema table column)
	     " = " (dot-append "p_rout_alias" column)
	     " OR (" (dot-append schema table column) " IS NULL"
	     " AND " (dot-append "p_rout_alias" column) " IS NULL)"
	     ")"))
	  (delete +record-id-column+ (columns db schema table)))
     " AND ")
    " AND " (dot-append schema table +record-id-column+)
    " > " (dot-append "p_rout_alias" +record-id-column+))))

;;; Remove duplicate rows from all tables in db
(define (delete-all-duplicates db)
  (for-each
   (lambda (schema+table)
     (let* ((schema+table-list (string-split schema+table #\.))
	    (schema (first schema+table-list))
	    (table (second schema+table-list)))
       (delete-duplicate-rows db schema table)))
   (tables db)))

;;; Maximum value of +record-id-column+ in db
(define (max-record-id-column db)
  (if (null? (tables db))
      0
      (apply
       max
       (map (lambda (table)
	      (logged-query
	       db "restore" (string-append
			     "SELECT max(" +record-id-column+ ") FROM " table))
	      (cdar (dbi-get_row db)))
	    (tables db)))))

(define (increase-record-id-column db table amount)
  (logged-query
   db
   "restore"
   (string-append
    "UPDATE " table
    " SET " +record-id-column+
    " = " +record-id-column+
    " + " (number->string amount))))

;;; Add amount to all values of +record-id-column+ in db
(define (increase-record-id-columns db amount)
  (for-each
   (lambda (table)
     (increase-record-id-column db table amount))
   (tables db)))

;; ;;; Return a list of all columns in db, each in its own list
;; ;;; (db schema table column)
;; (define (all-columns db)
;;   (append-map
;;    (lambda (schema+table)
;;      (let* ((schema+table-list (string-split schema+table #\.))
;; 	    (schema (first schema+table-list))
;; 	    (table (second schema+table-list)))
;;        (map (lambda (column)
;; 	      (list db schema table column))
;; 	    (columns db schema table))))
;;    (tables db)))

;; ;;; Create an index named <schema>.<table>_<column>_index if necessary
;; (define (create-index db schema table column)
;;   (logged-query db "restore" (string-append
;; 			      "SELECT "
;; 			      "t.relname AS table_name, "
;; 			      "i.relname AS index_name, "
;; 			      "a.attname AS column_name, "
;; 			      "n.nspname AS schema "
;; 			      "FROM "
;; 			      "pg_namespace n, pg_class t, "
;; 			      "pg_class i, "
;; 			      "pg_index ix, "
;; 			      "pg_attribute a "
;; 			      "WHERE "
;; 			      "n.oid = t.relnamespace "
;; 			      "AND t.oid = ix.indrelid "
;; 			      "AND i.oid = ix.indexrelid "
;; 			      "AND a.attrelid = t.oid "
;; 			      "AND a.attnum = ANY(ix.indkey) "
;; 			      "AND t.relkind = 'r' "
;; 			      "AND n.nspname = '" schema "' "
;; 			      "AND t.relname = '" table "' "
;; 			      "AND a.attname = '" column "'"))
;;   (unless (dbi-get_row *db*)
;;     (logged-query db "restore" (string-append
;; 				"CREATE INDEX " table "_" column "_index "
;; 				"ON " schema "." table " (" column ")"))))

;; (define (drop-index db schema table column)
;;   (logged-query db "restore" (string-append
;; 			      "DROP INDEX IF EXISTS " schema "." table "_" column "_index ")))

;; (define (create-indexes db)
;;   (for-each (lambda (args)
;; 	      (apply create-index args))
;; 	    (all-columns db)))

;; (define (drop-indexes db)
;;   (for-each (lambda (args)
;; 	      (apply drop-index args))
;; 	    (all-columns db)))

(exit-if-running-process "p-rout-collect.scm")

(dynamic-wind
  (lambda () (set! *db* (dbi-open "postgresql" +db-connection+)))
  (lambda ()
    (dynamic-wind
      (lambda () (set! *temp-db* (temp-db)))
      (lambda ()
	(psql-apply-dump *temp-db-name* +dump-file+)
	(increase-record-id-columns
	 *temp-db*
	 (+ 1000 (max-record-id-column *db*)))
	(dump-into (db-name) *temp-db-name*)
	(display "\

Hint: Messages saying
  ERROR:  <some entity> already exists
can be ignored.

Pruning duplicates; please be patient.
")
	;; (create-indexes *db*)
	;; (delete-all-duplicates *db*)
	)
      (lambda ()
	(dbi-close *temp-db*)
	(logged-query
	 *db* "restore" (string-append "DROP DATABASE " *temp-db-name*)))))
  (lambda ()
    ;; (drop-indexes *db*)
    (dbi-close *db*)))


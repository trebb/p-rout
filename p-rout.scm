#!/usr/bin/guile -s
!#

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
	     (ice-9 match))


(define option-spec
  '((help (single-char #\h))
    (verbose (single-char #\v))
    (db-dir (value #t))
    (log-dir (value #t))))

(define options (getopt-long (command-line) option-spec))

(when (option-ref options 'help #f)
  (display (car (command-line)))
  (display "\
 [options]
  -h, --help     Display this help
  -v, --verbose  Display debugging output
  --db-dir       Database directory (default: \"log-db\")
  --log-dir      Log directory (default: \"log-db\")
")
  (exit))

(define +record-id-column+ "pur_r_id")
(define +verbose+ (option-ref options 'verbose #f))
(define +db-dir+ (option-ref options 'db-dir "log-db"))
(define +log-dir+ (option-ref options 'log-dir "log-db"))

(define (p-rout-collector request body)
  (cond ((eq? (request-method request) 'POST)
	 (json-handler request body))
	(else (not-found request))))

(define (not-found request)
  (file-log #f "Unexpected request:" request)
  (values (build-response #:code 404)
	  (string-append "Not found: "
			 (uri->string (request-uri request)))))

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

(define (ht->l x)
;;; Convert JSON hashtable into a list structure
  (cond
   ((hash-table? x)
    (hash-map->list (lambda (a b) (cons a (ht->l b))) x))
   ((list? x)
    (map (lambda (item) (ht->l item)) x))
   (else x)))

(define (ht->tablenames json)
;;; Extract a list of toplevel names from JSON hashtable
  (map car (ht->l json)))

(define (ht->table-content json tablename)
  (cdar (filter (lambda (x) (equal? tablename (car x)))
		(ht->l json))))

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

(define (create-tables db tablenames)
  (for-each
   (lambda (tablename)
     (logged-query db tablename
		   (string-join `("CREATE TABLE IF NOT EXISTS"
				  ,tablename "(pur_r_id INTEGER)"))))
   tablenames))

(define (add-columns db table columnnames)
  (for-each
   (lambda (columnname)
     (logged-query db table
		   (string-join `("ALTER TABLE" ,table
				  "ADD COLUMN" ,columnname))
      1))  ; ignore duplicate column name error
   columnnames))

(define (columnnames table-content)
  ;; Extract from table-content the set of column names
  (let* ((result '())
	 (collect-columnnames
	  (lambda (name-value-pair)
	    (set! result
		  (cons (car name-value-pair) result)))))
    (map-rows collect-columnnames table-content)
    (sort (delete-duplicates result) string<?)))

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

(define (add-row db table columnnames values record-id)
  (let ((names (string-join (cons +record-id-column+ columnnames) ", "))
	(vals (string-join
	       (map (lambda (val)
		      (if (number? val)
			  (number->string val)
			  (string-join `("'" ,val "'") "")))
		    (cons record-id values))
	       ", ")))
    (logged-query db table
		  (string-join `("INSERT INTO" ,table
				 "(" ,names ") VALUES (" ,vals ")")))))

(define (uri-path->basename uri-path)
  (let ((uri-extension ".json")
	(filename (last (split-and-decode-uri-path uri-path))))
    (if (string-suffix? uri-extension filename)
	(string-drop-right filename (string-length uri-extension))
	#f)))
  
(define (store-record uri-path payload)
  (let ((basename (uri-path->basename uri-path)))
    (if basename
	(begin
	  (system (string-append "mkdir -p " +db-dir+))
	  (let ((dbfile (string-append +db-dir+ "/" basename ".sqlite3"))
		(db #f))
	    (dynamic-wind
	      (lambda () (set! db (dbi-open "sqlite3" dbfile)))
	      (lambda () (add-record db payload))
	      (lambda () (dbi-close db)))))
	(file-log #f "Unexpected POST request:" uri-path))))

(define (add-record db json)
  (let ((tablenames (ht->tablenames json)))
    (create-tables db tablenames)
    (logged-query db "db" "BEGIN IMMEDIATE TRANSACTION")
    (let ((next-id
	   (apply
	    max 
	    (map
	     (lambda (table)
	       (logged-query db table
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
	   (add-columns db table (columnnames table-content))
	   (let ((names (map-rows car table-content))
		 (values (map-rows cdr table-content)))
	     (match names
	       (((_ ...) ...)
		(for-each
		 (lambda (names values)
		   (add-row db table names values next-id))
		 names values))
	       ((_ ...)
		(add-row db table names values next-id))))))
       tablenames))
    (logged-query db "db" "COMMIT TRANSACTION")))
	  
	  
;; (define db-obj (dbi-open "sqlite3" "my-example-db"))
;; (dbi-query db-obj "CREATE TABLE IF NOT EXISTS blahblah (id INTEGER)")
;; (dbi-query db-obj "ALTER TABLE blahblah ADD COLUMN xxx1")
;; (dbi-query db-obj "SELECT * FROM blahblah")
;; (dbi-get_status db-obj)
;; (dbi-get_row db-obj)
;; (dbi-close db-obj)

(run-server p-rout-collector
	    'http `(#:port 80 #:addr ,(inet-pton AF_INET "10.11.0.3")))


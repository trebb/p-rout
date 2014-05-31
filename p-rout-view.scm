#!/usr/bin/guile -s
!#

;; On Debian, we may need to
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
	     (rnrs base)
	     (rnrs bytevectors)
	     (rnrs io ports)
	     (os process)
	     (dbi dbi)
	     (srfi srfi-1)
	     (srfi srfi-19)
	     (ice-9 getopt-long)
	     (ice-9 pretty-print)
	     (ice-9 match)
	     (ice-9 rdelim))


(define option-spec
  '((help (single-char #\h))
    (verbose (single-char #\v))
    (db-dir (value #t))
    (log-dir (value #t))
    (gnuplot-lib-dir (value #t))))

(define options (getopt-long (command-line) option-spec))

(when (option-ref options 'help #f)
  (display (car (command-line)))
  (display "\
 [options]
  -h, --help        Display this help
  -v, --verbose     Display debugging output
  --db-dir          Database directory (default: \"log-db\")
  --log-dir         Log directory (default: \"log-db\")
  --gnuplot-lib-dir Gnuplot's Javascript directory
")
  (exit))

(define +record-id-column+ "pur_r_id")
(define +verbose+ (option-ref options 'verbose #f))
(define +db-dir+ (option-ref options 'db-dir "log-db"))
(define +log-dir+ (option-ref options 'log-dir "log-db"))
(define +gnuplot-lib-dir+ "/usr/share/gnuplot/4.6/js")

(define +diagrams+
  '(("Battery"
     ("logs" ("header" "module_statuses") "header.time_send")
     (("Battery Voltage"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 11" "lines smooth bezier")
      ("Charge State" 
       "module_statuses.param_1 + 1" "module_statuses.module_id = 11" "lines smooth bezier")))
    ("Something Else"
     ("logs" ("header" "module_statuses") "header.time_send")
     (("Battery stuff"
       "param_3" "module_statuses.module_id = 11" "lines smooth bezier")
      ("Charge State"
       "param_3" "module_statuses.module_id = 11" "lines smooth bezier")))))

(define (dbfile diagram)
  (first (cadr (assoc diagram +diagrams+))))

(define (tables diagram)
  (second (cadr (assoc diagram +diagrams+))))

(define (date-column diagram)
  (third (cadr (assoc diagram +diagrams+))))

(define (curve-names diagram)
  (map car (caddr (assoc diagram +diagrams+))))

(define (columnname diagram curve-name)
  (first (cdr (assoc curve-name
		     (caddr (assoc diagram +diagrams+))))))

(define (sql-where diagram curve-name)
  (second (cdr (assoc curve-name
		     (caddr (assoc diagram +diagrams+))))))

(define (curve-style diagram curve-name)
  (third (cdr (assoc curve-name
		     (caddr (assoc diagram +diagrams+))))))

(define (p-rout-publisher request body)
  (let ((uri-elements
	 (split-and-decode-uri-path (uri-path (request-uri request)))))
  (cond ((equal? uri-elements '("view"))
	 (view-handler request body))
	((equal? (drop-right uri-elements 1) '("view" "diagram"))
	 (view-diagram-handler request body))
	((equal? (drop-right uri-elements 1) '("view" "lib"))
	 (view-lib-handler request body))
	(else (not-found request)))))

(define (not-found request)
  (values (build-response #:code 404)
	  (string-append "Not found: "
			 (uri->string (request-uri request)))))

(define (view-handler request body)
  (values (build-response #:code 200
			  #:reason-phrase "Ok"
			  #:headers `((content-type . (text/plain)) 
				      (charset . "utf-8")))
	  (view-data "logs" "header")))

(define (view-diagram-handler request body)
  (let* ((uri (request-uri request))
	 (uri-file-name
	  (car (take-right (split-and-decode-uri-path (uri-path uri)) 1)))
	 (uri-query (uri-query uri)))
    (values (build-response #:code 200
			    #:reason-phrase "Ok"
			    #:headers `((content-type . (image/svg+xml)) 
					(charset . "utf-8")))
	    (catch #t
	      (lambda () (assert (apply plot-svg
					(car (string-split uri-file-name #\.))
					(string-split uri-query
						      #\&))))
	      (lambda (err . args) "<svg> </svg>")))))

(define (view-lib-handler request body)
  (values (build-response #:code 200
			  #:reason-phrase "Ok"
			  #:headers `((content-type . (text/javascript)) 
				      (charset . "utf-8")))
	  (let ((file-name
		 (string-append
		  +gnuplot-lib-dir+ "/"
		  (car
		   (take-right
		    (split-and-decode-uri-path (uri-path (request-uri request)))
		    1)))))
	    (catch #t
	      (lambda ()
		(with-input-from-file
		    file-name (lambda () (read-delimited ""))))
	      (lambda (err . args) "nothing here")))))

(define (view-data dbfile table)
  (system (string-append "mkdir -p " +db-dir+))
  (let ((dbfile (string-append +db-dir+ "/" dbfile ".sqlite3"))
	(db #f))
    (dynamic-wind
      (lambda () (set! db (dbi-open "sqlite3" dbfile)))
      (lambda ()
	(dbi-query db (string-join `("SELECT * FROM" ,table)))
	(with-output-to-string
	  (lambda ()
	    (while (= 0 (car (dbi-get_status db)))
	      (display (dbi-get_row db))
	      (newline)))))
      (lambda () (dbi-close db)))))


(define (get-curve-points diagram curve-name from-date to-date)
  (let ((db #f)
	(sql (string-append
	      "SELECT " (date-column diagram)
	      ", " (columnname diagram curve-name)
	      " FROM "
	      (let ((tables (tables diagram)))
		(if (> (length tables) 1)
		    (string-append (string-join tables ", ")
				   " USING (" +record-id-column+ ")")
		    (car tables)))
	      " WHERE ("
	      (date-column diagram) " BETWEEN \"" from-date "\" AND \"" to-date
	      "\")"
	      (let ((sql-where (sql-where diagram curve-name)))
		(if sql-where
		    (string-append " AND " sql-where)
		    ""))
	      " ORDER BY " (date-column diagram))))
    (dynamic-wind
      (lambda ()
	(set! db
	      (dbi-open
	       "sqlite3"
	       (string-append +db-dir+ "/" (dbfile diagram) ".sqlite3"))))
      (lambda ()
	(dbi-query db sql)
	(do ((row (dbi-get_row db)
		  (dbi-get_row db))
	     (result ""))
	    ((not row) (string-append result "e\n"))
	  (set! result
		(string-append
		 result
		 (string-join
		  (map (lambda (x)
			 (with-output-to-string (lambda () (display (cdr x)))))
		       row))
		 "\n"))))
      (lambda () (dbi-close db)))))

(define (gnuplot-commands diagram from-date to-date)
  (let ((curve-names (curve-names diagram)))
    (string-append
     "set terminal 'svg' mouse jsdir '/view/lib/'"
     " size 1200, 800 dynamic fsize 8\n"
     "set output\n"
     "set key left box\n"
     "set timefmt '%Y-%m-%dT%H:%M:%S'\n"
     "set format x \"%a\\n%Y-%m-%d\\n%H:%M:%S\"\n"
     "set xdata time\n"
     "plot "
     (string-join
      (map (lambda (curve-name)
	     (string-append "'-' using 1:2 title '"
			    curve-name
			    "' with "(curve-style diagram curve-name)))
	   curve-names)
      ", ")
     "\n"
     (string-join
      (map (lambda (curve-name)
	     (get-curve-points diagram curve-name from-date to-date))
	   curve-names)
      ""))))

(define (plot-svg diagram from-date to-date)
  (let* ((pp (run-with-pipe "r+" "gnuplot"))
	 (ppin (cadr pp))
	 (ppout (cddr pp))
	 (svg #f))
    (display (gnuplot-commands diagram from-date to-date)
	     ppout)
    (close ppout)
    (set! svg (read-delimited "" ppin))
    (close ppin)
    (if (eof-object? svg)
	#f
	svg)))

;; (run-server p-rout-publisher 'http `(#:port 8080 #:addr ,(inet-pton AF_INET "192.168.178.51")))

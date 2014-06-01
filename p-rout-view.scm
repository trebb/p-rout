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
	     (sxml simple)
	     ((rnrs base) #:select (assert))
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
     ("logs" ("header" "module_statuses") "header.time_send"
     "")
     (("I_{charge}"
       "module_statuses.param_10 / 100" "module_statuses.module_id = 136" "lines smooth bezier")
      ("v_{charge}"
       "module_statuses.param_9 / 100" "module_statuses.module_id = 136" "lines smooth bezier")
      ("I_{discharge}" 
       "module_statuses.param_12 / 100" "module_statuses.module_id = 136" "lines smooth bezier")
      ("V_{discharge}" 
       "module_statuses.param_11 / 100" "module_statuses.module_id = 136" "lines smooth bezier")
      ("V_{batt}" 
       "module_statuses.param_0 / 100" "module_statuses.module_id = 136" "lines smooth bezier")
      ("T_{batt}" 
       "module_statuses.param_7 / 10" "module_statuses.module_id = 136" "lines smooth bezier")
      ("SOC" 
       "module_statuses.param_5" "module_statuses.module_id = 136" "filledcurve x1 lc rgb 'green'")))
    ("Solar"
     ("logs" ("header" "module_statuses") "header.time_send"
      "set keytitle '{/=12 Solar}'\n")
     (("V_{bus_dcac}"
       "module_statuses.param_8 / 100" "module_statuses.module_id = 9" "lines smooth bezier")))      
    ("Power"
     ("logs" ("header" "module_statuses") "header.time_send"
      "set keytitle '{/=12 Power}'\n")
     (("P_{batt}"
       "module_statuses.param_2" "module_statuses.module_id = 136" "lines smooth bezier")
      ("P_{local}"
       "module_statuses.param_6" "module_statuses.module_id = 9" "lines smooth bezier")
      ("P_{grid_{dcac}}"
       "module_statuses.param_2" "module_statuses.module_id = 9" "lines smooth bezier")
      ("P_{grid_{platform}}"
       "module_statuses.param_3" "module_statuses.module_id = 16" "lines smooth bezier")
      ("P_{solar}"
       "module_statuses.param_10" "module_statuses.module_id = 12" "lines smooth bezier")
      ("P_{L1}"
       "module_statuses.param_2" "module_statuses.module_id = 11" "lines smooth bezier")
      ("P_{L2}"
       "module_statuses.param_6" "module_statuses.module_id = 11" "lines smooth bezier")
      ("P_{L3}"
       "module_statuses.param_10" "module_statuses.module_id = 11" "lines smooth bezier")))
    ("Voltage"
     ("logs" ("header" "module_statuses") "header.time_send"
      "set keytitle '{/=12 AC Voltage}'\nset yrange [200:250]\n")
     (("V_{local}"
       "module_statuses.param_5 / 10" "module_statuses.module_id = 9" "lines smooth bezier")
      ("V_{grid_{dcac}}"
       "module_statuses.param_1 / 10" "module_statuses.module_id = 9" "lines smooth bezier")
      ("V_{grid_{platform}}"
       "module_statuses.param_1 / 10" "module_statuses.module_id = 16" "lines smooth bezier")
      ("V_{L1}"
       "module_statuses.param_0 / 10" "module_statuses.module_id = 11" "lines smooth bezier")
      ("V_{L2}"
       "module_statuses.param_4 / 10" "module_statuses.module_id = 11" "lines smooth bezier")
      ("V_{L3}"
       "module_statuses.param_8 / 10" "module_statuses.module_id = 11" "lines smooth bezier")))
    ("Temperature"
     ("logs" ("header" "module_statuses") "header.time_send"
      "set keytitle '{/=12 Temperature}'\n")
     (("T_{dcac}"
       "module_statuses.param_10 / 10" "module_statuses.module_id = 9" "lines smooth bezier")
      ("T_{platform}"
       "module_statuses.param_2 / 10" "module_statuses.module_id = 16" "lines smooth bezier")
      ("T_{batt}"
       "module_statuses.param_7 / 10" "module_statuses.module_id = 136" "lines smooth bezier")
      ("T_{batt_{module}}"
       "module_statuses.param_8 / 10" "module_statuses.module_id = 136" "lines smooth bezier")
      ("T_{solar}"
       "module_statuses.param_9 / 10" "module_statuses.module_id = 12" "lines smooth bezier")))))

(define (dbfile diagram)
  (first (cadr (assoc diagram +diagrams+))))

(define (diagrams)
  (map car +diagrams+))

(define (tables diagram)
  (second (cadr (assoc diagram +diagrams+))))

(define (date-column diagram)
  (third (cadr (assoc diagram +diagrams+))))

(define (gnuplot-settings diagram)
  (fourth (cadr (assoc diagram +diagrams+))))

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

(define (uri-file-name request)
  (car (take-right (uri-elements request) 1)))

(define (uri-elements request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (uri-dir-elements request)
  (drop-right (uri-elements request) 1))

(define (uri-basename request)
  (car (string-split (uri-file-name request) #\.)))

(define (uri-extension request)
  (cadr (string-split (uri-file-name request) #\.)))

(define (uri-query-elements request)
  ;; Split at ',' because the Right Thing (splitting at '&') doesn't
  ;; play well with sxml->xml.
  (string-split (uri-query (request-uri request)) #\,))

(define (date-plus-seconds date seconds)
  (time-utc->date (add-duration (date->time-utc date)
				(make-time 'time-duration 0 seconds))))

(define (p-rout-publisher request body)
  (cond ((equal? '("view") (uri-elements request))
	 (view-handler request body))
	((and (equal? '("view" "diagram") (uri-dir-elements request))
	      (string=? "svg" (uri-extension request)))
	 (view-diagram-handler request body))
	((equal? '("view" "lib") (uri-dir-elements request))
	 (view-lib-handler request body))
	(else (not-found request))))

(define (not-found request)
  (values (build-response #:code 404)
	  (string-append "Not found: "
			 (uri->string (request-uri request)))))

(define (view-handler request body)
  (values (build-response #:code 200
			  #:reason-phrase "Ok"
			  #:headers `((content-type . (text/html)) 
				      (charset . "utf-8")))
	  (html-diagram (first (diagrams))
			(date->string (date-plus-seconds (current-date) (* -3600 24 7)) "~5")
	  		(date->string (current-date) "~5"))))

(define (view-diagram-handler request body)
  (values (build-response #:code 200
			  #:reason-phrase "Ok"
			  #:headers `((content-type . (image/svg+xml)) 
				      (charset . "utf-8")))
	  (catch #t
	    (lambda ()
	      (assert (apply plot-svg
			     (uri-basename request)
			     (uri-query-elements request))))
	    (lambda (err . args) "<svg> </svg>"))))

(define (view-lib-handler request body)
  (values (build-response #:code 200
			  #:reason-phrase "Ok"
			  #:headers `((content-type . (text/javascript)) 
				      (charset . "utf-8")))
	  (let ((file-name
		 (string-append
		  +gnuplot-lib-dir+ "/" (uri-file-name request))))
	    (catch #t
	      (lambda ()
		(with-input-from-file
		    file-name (lambda () (read-delimited ""))))
	      (lambda (err . args) "nothing here")))))

(define (html-diagram diagram from-date to-date)
  (with-output-to-string
    (lambda ()
      (sxml->xml
       `(*TOP* (html
		(body
		 (p
		  (img (@ (width "1200")
			  (height "800")
			  (src ,(string-append "diagram/" diagram ".svg?" from-date "," to-date)))))
		 (p "some text"r))))))))

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
     "set terminal 'svg' enhanced linewidth 2 mouse jsdir '/view/lib/'"
     " size 1200, 800 dynamic fsize 8\n"
     "set encoding utf8\n"
     "set output\n"
     "set key left\n"
     "set keytitle '{/=12 " diagram "}'\n"	;just a default title
     "set timefmt '%Y-%m-%dT%H:%M:%S'\n"
     "set format x \"%a\\n%Y-%m-%d\\n%H:%M:%S\"\n"
     "set xdata time\n"
     "set style fill transparent solid 0.2 noborder\n"
     (gnuplot-settings diagram)
     "\n"
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

(run-server p-rout-publisher 'http `(#:port 8080 #:addr ,(inet-pton AF_INET "192.168.178.51")))

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
	     (ice-9 rdelim)
	     (ice-9 popen))

(define option-spec
  '((help (single-char #\h))
    (verbose (single-char #\v))
    (addr (single-char #\a) (value #t))
    (port (single-char #\p) (value #t))
    (db-connection (value #t))
    (log-dir (value #t))
    (gnuplot-lib-dir (value #t))
    (no-daemon)))

(define options (getopt-long (command-line) option-spec))

(when (option-ref options 'help #f)
  (display (car (command-line)))
  (display "\
 [options]
  -h, --help        Display this help
  -v, --verbose     Display debugging output
  -a, --addr        Address to listen on
  -p, --port        Port to listen on
  --db-connection   <user>:<pass>:<db>:<path/ip>[:port] (default:
                    p-rout:p-rout:p_rout:/run/postgres:localhost)
  --log-dir         Log directory (default: \"log-v\")
  --gnuplot-lib-dir Gnuplot's Javascript directory
  --no-daemon       Remain in foreground
")
  (exit))

(define (gnuplot-version)
  (second (string-split
	   (get-string-all (open-pipe "gnuplot --version" OPEN_READ))
	   #\space)))

(define +record-id-column+ "p_rout_id")
(define +verbose+ (option-ref options 'verbose #f))
(define +db-connection+
  (option-ref options
	      'db-connection
	      "p-rout:p-rout:p_rout:/run/postgres:localhost"))
(define +log-dir+ (option-ref options 'log-dir "log-v"))
(define +gnuplot-lib-dir+
  (option-ref options 'gnuplot-lib-dir (string-append "/usr/share/gnuplot/"
						      (gnuplot-version)
						      "/js")))
(define +addr+ (option-ref options 'addr "192.168.178.51"))
(define +port+ (string->number (option-ref options 'port "80")))
(define +no-daemon+ (option-ref options 'no-daemon #f))
(define +from-label+ "From")
(define +to-label+ "To")
(define +table-number-of-columns+ 80)
(define +diagram-number-of-values+ 400)
(define *db* #f)

(define +output-sets+
  '(("Battery"
     ("logs" ("header" "module_statuses") "header.time_send"
     "" as-diagram)
     (;; ("I_{charge}"
      ;;  "module_statuses.param_10 / 100" "module_statuses.module_id = 136" "lines smooth bezier")
      ("V_{charge}"
       "module_statuses.param_9 / 100" "module_statuses.module_id = 136" "lines smooth bezier")
      ("I_{batt}"
       "module_statuses.param_1 / 100" "module_statuses.module_id = 136" "lines lw 2 lc rgb 'green' smooth bezier")
      ;; ("I_{discharge}" 
      ;;  "module_statuses.param_12 / 100" "module_statuses.module_id = 136" "lines smooth bezier")
      ("V_{discharge}" 
       "module_statuses.param_11 / 100" "module_statuses.module_id = 136" "lines smooth bezier")
      ("V_{batt}" 
       "module_statuses.param_0 / 100" "module_statuses.module_id = 136" "lines smooth bezier")
      ("T_{batt}" 
       "module_statuses.param_7 / 10" "module_statuses.module_id = 136" "lines smooth bezier")
      ("SOC" 
       "module_statuses.param_5" "module_statuses.module_id = 136" "filledcurve y1=0 lc rgb 'green'")))
    ("Solar"
     ("logs" ("header" "module_statuses") "header.time_send"
      "set keytitle '{/=12 Solar}'\n" as-diagram)
     (("P/kW"
       "module_statuses.param_10 / 1000" "module_statuses.module_id = 12" "lines smooth bezier")
      ("V/100V"
       "module_statuses.param_5 / 10000" "module_statuses.module_id = 12" "lines smooth bezier")
      ("I"
       "module_statuses.param_6 / 100" "module_statuses.module_id = 12" "lines smooth bezier")))
    ("Power"
     ("logs" ("header" "module_statuses") "header.time_send"
      "set keytitle '{/=12 Power}'\n" as-diagram)
     (("P_{batt}"
       "module_statuses.param_2" "module_statuses.module_id = 136" "lines lw 3 lc rgb 'green' smooth bezier")
      ("P_{local}"
       "module_statuses.param_6" "module_statuses.module_id = 9" "lines smooth bezier")
      ("P_{grid_{dcac}}"
       "module_statuses.param_2" "module_statuses.module_id = 9" "lines smooth bezier")
      ("P_{grid_{platform}}"
       "module_statuses.param_3" "module_statuses.module_id = 16" "lines smooth bezier")
      ("P_{solar}"
       "module_statuses.param_10" "module_statuses.module_id = 12" "lines lw 3 lc rgb 'red' smooth bezier")
      ("P_{L1}"
       "module_statuses.param_2" "module_statuses.module_id = 11" "lines lw 2 lc rgb 'orange' smooth bezier")
      ("P_{L2}"
       "module_statuses.param_6" "module_statuses.module_id = 11" "lines lw 2 lc rgb 'dark-red' smooth bezier")
      ("P_{L3}"
       "module_statuses.param_10" "module_statuses.module_id = 11" "lines lw 2 lc rgb 'violet' smooth bezier")))
    ("Voltage"
     ("logs" ("header" "module_statuses") "header.time_send"
      "set keytitle '{/=12 AC Voltage}'\nset yrange [220:240]\n" as-diagram)
     (("V_{local}"
       "module_statuses.param_5 / 10" "module_statuses.module_id = 9" "lines smooth bezier")
      ("V_{grid_{dcac}}"
       "module_statuses.param_1 / 10" "module_statuses.module_id = 9" "lines smooth bezier")
      ("V_{grid_{platform}}"
       "module_statuses.param_1 / 10" "module_statuses.module_id = 16" "lines smooth bezier")
      ("V_{L1}"
       "module_statuses.param_0 / 10" "module_statuses.module_id = 11" "lines lw 2 lc rgb 'orange' smooth bezier")
      ("V_{L2}"
       "module_statuses.param_4 / 10" "module_statuses.module_id = 11" "lines lw 2 lc rgb 'dark-red' smooth bezier")
      ("V_{L3}"
       "module_statuses.param_8 / 10" "module_statuses.module_id = 11" "lines lw 2 lc rgb 'violet' smooth bezier")))
    ("Frequency"
     ("logs" ("header" "module_statuses") "header.time_send"
      "" as-diagram)
     (("f_{dcac}"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 9" "lines smooth bezier")
      ("f_{platform}"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 16" "lines smooth bezier")))
    ("Temperature"
     ("logs" ("header" "module_statuses") "header.time_send"
      "set keytitle '{/=12 Temperature}'\n" as-diagram)
     (("T_{dcac}"
       "module_statuses.param_10 / 10" "module_statuses.module_id = 9" "lines lc rgb 'blue' smooth bezier")
      ("T_{platform}"
       "module_statuses.param_2 / 10" "module_statuses.module_id = 16" "lines lc rgb 'dark-green' smooth bezier")
      ("T_{batt}"
       "module_statuses.param_7 / 10" "module_statuses.module_id = 136" "lines lw 3 lc rgb 'green' smooth bezier")
      ("T_{batt_{module}}"
       "module_statuses.param_8 / 10" "module_statuses.module_id = 136" "lines lc rgb 'green' smooth bezier")
      ("T_{solar}"
       "module_statuses.param_9 / 10" "module_statuses.module_id = 12" "lines lc rgb 'red' smooth bezier")))
    ("Energy"
     ("logs" ("header" "module_statuses") "header.time_send"
      "" as-table)
     (("Platform consumed"
       "module_statuses.param_5 / 1000" "module_statuses.module_id = 16")
      ("Platform produced"
       "module_statuses.param_4 / 1000" "module_statuses.module_id = 16")
      ("DC/AC consumed"
       "module_statuses.param_4 / 1000" "module_statuses.module_id = 9")
      ("DC/AC produced"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 9")
      ("Local DC/AC consumed"
       "module_statuses.param_7 / 1000" "module_statuses.module_id = 9")
      ("Local DC/AC produced"
       "module_statuses.param_6 / 1000" "module_statuses.module_id = 9")
      ("Battery consumed"
       "module_statuses.param_4 / 1000" "module_statuses.module_id = 136")
      ("Battery produced"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 136")
      ("Solar produced"
       "module_statuses.param_11 / 1000" "module_statuses.module_id = 12")
      ("L1 consumed"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 11")
      ("L2 consumed"
       "module_statuses.param_7 / 1000" "module_statuses.module_id = 11")
      ("L3 consumed"
       "module_statuses.param_11 / 1000" "module_statuses.module_id = 11")))
    ("Status"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Raw status strings" as-table)
     (("Platform"
       "module_statuses.status" "module_statuses.module_id = 16")
      ("DC/AC"
       "module_statuses.status" "module_statuses.module_id = 9")
      ("Battery"
       "module_statuses.status" "module_statuses.module_id = 136")
      ("Solar"
       "module_statuses.status" "module_statuses.module_id = 12")))
    ("Events"
     ("events" ("header" "event") "header.time_send"
      "Raw event strings" as-table)
     (("Event"
       "event.data" #f)))))

(define (output-sets)
  (map car +output-sets+))

(define (schema output-set)
  (first (cadr (assoc output-set +output-sets+))))

(define (tables output-set)
  (map (lambda (table)
	 (dot-append (schema output-set) table))
       (second (cadr (assoc output-set +output-sets+)))))

(define (date-column output-set)
  (third (cadr (assoc output-set +output-sets+))))

(define (gnuplot-settings output-set)
  (fourth (cadr (assoc output-set +output-sets+))))

(define (render-mode output-set)
  (fifth (cadr (assoc output-set +output-sets+))))

(define (curve-names output-set)
  (map car (caddr (assoc output-set +output-sets+))))

(define (columnname output-set curve-name)
  (first (cdr (assoc curve-name
		     (caddr (assoc output-set +output-sets+))))))

(define (sql-where output-set curve-name)
  (second (cdr (assoc curve-name
		     (caddr (assoc output-set +output-sets+))))))

(define (curve-style output-set curve-name)
  (third (cdr (assoc curve-name
		     (caddr (assoc output-set +output-sets+))))))

(define (uri-elements request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (uri-file-name request)
  (if (null? (uri-elements request))
      ""
      (car (take-right (uri-elements request) 1))))

(define (uri-dir-elements request)
  (if (null? (uri-elements request))
      '()
      (drop-right (uri-elements request) 1)))

(define (now) (date->string (current-date) "~4"))

(define (dot-append . strings) (string-join strings "."))

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
	(for-each
	 (lambda (part)
	   (display " " out)
	   (display part out))
	 message-parts)
	(newline out))
      (lambda () (close out)))))

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

;;; Turn query part of URI into an alist
(define (uri-query-components request)
  (let ((query (uri-query (request-uri request))))
    (map (lambda (query-component)
	   (let ((component (string-split query-component #\=)))
	     (cons (first component) (second component))))
	 (string-split query #\&))))

;;; Put a T between day and hour
(define (normalize-date-string date-string-with-space)
  (date->string (string->date date-string-with-space "~Y-~m-~d ~H:~M")
		"~5"))

;;; Remove the T between day and hour
(define (humanize-date-string date-string-with-a-t)
  (date->string (string->date date-string-with-a-t "~Y-~m-~dT~H:~M")
		"~Y-~m-~d ~H:~M"))

(define (date-plus-seconds date seconds)
  (time-utc->date (add-duration (date->time-utc date)
				(make-time 'time-duration 0 seconds))))

(define (p-rout-view request body)
  (when +verbose+
    (file-log "http" request))
  (cond ((equal? '("view") (uri-elements request))
	 (view-handler request body))
	((equal? '("view" "render") (uri-elements request))
	 (view-render-handler request body))
	((equal? '("view" "lib" "datetimepicker_css.js")
		 (uri-elements request))
	 (view-lib-datetimepicker-handler request body))
	((equal? '("view" "lib") (uri-dir-elements request))
	 (view-lib-handler request body))
	(else (not-found request))))

(define (not-found request)
  (values (build-response #:code 404)
	  (string-append "Not found: "
			 (uri->string (request-uri request)))))

(define (sxml-date-input label)
  `((label (@ (for ,label))
       ,(string-append label " "))
  (input (@ (type "Text")
	    (id ,label)
	    (name ,label)
	    (maxlength "16") (size "16")
	    (onclick ,(string-append "javascript:NewCssCal('"
				     label
				     "','yyyyMMdd','dropdown',true,'24')"))))))

(define (sxml-output-set-inputs)
  (map (lambda (output-set)
	 `(input (@ (type "radio")
		    (name "output-set")
		    (value ,output-set))
		 ,(string-append output-set " ")))
       (output-sets)))

(define (view-handler request body)
  (values (build-response #:code 200
			  #:reason-phrase "Ok"
			  #:headers `((content-type . (text/html)) 
				      (charset . "utf-8")))
	  (with-output-to-string
	    (lambda ()
	      (display "<!DOCTYPE html>\n")
	      (sxml->xml
	       `(html
		 (head
		  (title "p-rout")
		  (script (@ (src "/view/lib/datetimepicker_css.js")) ""))
		 (body
		  (div
		   (@ (style "text-align:center; margin:150px auto 100px auto;"))
		   (form
		    (@ (action "/view/render"))
		    (p ,(sxml-date-input +from-label+)
		       " "
		       ,(sxml-date-input +to-label+))
		    (p ,(sxml-output-set-inputs))
		    (p (input (@ (type "submit")
				 (value "Go")))))))))))))

(define (view-render-handler request body)
  (let ((render-mode
	 (render-mode (cdr (assoc "output-set"
				  (uri-query-components request))))))
    (cond ((eq? 'as-diagram render-mode)
	   (view-render-diagram-handler request body))
	  ((eq? 'as-table render-mode)
	   (view-render-table-handler request body)))))
	   
(define (view-render-diagram-handler request body)
  (let ((query-alist (uri-query-components request)))
    (values
     (build-response #:code 200
		     #:reason-phrase "Ok"
		     #:headers `((content-type . (image/svg+xml)) 
				 (charset . "utf-8")))
     (catch #t
       (lambda ()
	 (assert
	  (plot-svg (cdr (assoc "output-set" query-alist))
		    (normalize-date-string
		     (uri-decode (cdr (assoc +from-label+ query-alist))))
		    (normalize-date-string
		     (uri-decode (cdr (assoc +to-label+ query-alist)))))))
       (lambda (err . args)
	 (with-output-to-string
	   (lambda ()
	     (sxml->xml `(svg (@ (viewBox "0 0 1200 800")
				 (xmlns "http://www.w3.org/2000/svg"))
			      (title "No Data")
			      (text "No Data"))))))))))
	   
(define (view-render-table-handler request body)
  (let ((query-alist (uri-query-components request)))
    (values
     (build-response #:code 200
		     #:reason-phrase "Ok"
		     #:headers `((content-type . (text/html)) 
				 (charset . "utf-8")))
     (catch #t
       (lambda ()
	 (assert
	  (tabulate (cdr (assoc "output-set" query-alist))
		    (normalize-date-string
		     (uri-decode (cdr (assoc +from-label+ query-alist))))
		    (normalize-date-string
		     (uri-decode (cdr (assoc +to-label+ query-alist)))))))
       (lambda (err . args)
	 (with-output-to-string
	   (lambda ()
	     (sxml->xml `(svg (@ (viewBox "0 0 1200 800")
				 (xmlns "http://www.w3.org/2000/svg"))
			      (title "No Data")
			      (text "No Data"))))))))))

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
		(with-input-from-file file-name
		  (lambda () (read-delimited ""))))
	      (lambda (err . args) "nothing here")))))

(define (view-lib-datetimepicker-handler request body)
  (values (build-response #:code 200
			  #:reason-phrase "Ok"
			  #:headers `((content-type . (text/javascript)) 
				      (charset . "utf-8")))
	  (catch #t
	    (lambda ()
	      (with-input-from-file (uri-file-name request)
		(lambda () (read-delimited ""))))
	    (lambda (err . args) "nothing here"))))

(define (get-sql-row-sql output-set curve-name from-date to-date number-of-rows)
  (string-append
   "WITH t (id, date, value) AS"
   " (SELECT " +record-id-column+
   ", CAST (" (date-column output-set) " AS timestamp)"
   ", " (columnname output-set curve-name)
   " FROM "
   (let ((tables (tables output-set)))
     (if (> (length tables) 1)
	 (string-append (string-join tables " JOIN ")
			" USING (" +record-id-column+ ")")
	 (car tables)))
   " WHERE ("
   "CAST (" (date-column output-set) " AS timestamp)"
   " BETWEEN '" from-date "' AND '" to-date "')"
   (let ((sql-where (sql-where output-set curve-name)))
     (if sql-where
	 (string-append " AND " sql-where)
	 ""))
   ")"
   " SELECT date, value FROM t WHERE id % (SELECT 1 + count(*) / "
   (number->string number-of-rows)
   " FROM t) = 0"
   " ORDER BY id LIMIT " (number->string number-of-rows)))

(define (get-curve-points output-set curve-name from-date to-date)
  (let ((sql (get-sql-row-sql output-set curve-name
			      from-date to-date
			      +diagram-number-of-values+)))
    (logged-query "db" sql)
    (do ((row (dbi-get_row *db*)
	      (dbi-get_row *db*))
	 (result ""))
	((not row) (string-append result "e\n"))
      (set! result
	    (string-append
	     result
	     (with-output-to-string
	       (lambda ()
		 (display (normalize-date-string (cdr (first row))))
		 (display " ")
		 (display (cdr (second row)))))
	     "\n")))))

;;; date-column?=#t means return an html table column made of date/time
(define (get-sxml-table-column
	 output-set curve-name from-date to-date date-column?)
  (let ((sql (get-sql-row-sql output-set curve-name
			      from-date to-date
			      +table-number-of-columns+)))
    (logged-query "db" sql)
    (do ((sql-row (dbi-get_row *db*)
		  (dbi-get_row *db*))
	 (result `( ,(if date-column?
			 `(th ,output-set)
			 `(td ,curve-name)))))
	((not sql-row) result)
      (set! result
	    (append
	     result
	     `((td ,(with-output-to-string
		      (lambda ()
			(display (if date-column?
				     (cdr (assoc "date"
						  sql-row))
				     (cdr (assoc "value"
						 sql-row)))))))))))))

(define (gnuplot-commands output-set from-date to-date)
  (let* ((curve-names (curve-names output-set)))
    (string-append
     "set terminal 'svg' enhanced linewidth 2 mouse jsdir '/view/lib/'"
     " size 1200, 800 dynamic fsize 8\n"
     "set encoding utf8\n"
     "set output\n"
     "set key left\n"
     "set keytitle '{/=12 " output-set "}'\n" ;just a default title
     "set timefmt '%Y-%m-%dT%H:%M:%S'\n"
     "set format x \"%a\\n%Y-%m-%d\\n%H:%M:%S\"\n"
     "set xdata time\n"
     "set style fill transparent solid 0.1 noborder\n"
     (gnuplot-settings output-set)
     "\n"
     "plot "
     (string-join
      (map (lambda (curve-name)
	     (string-append "'-' using 1:2 title '"
			    curve-name
			    "' with " (curve-style output-set curve-name)))
	   curve-names)
      ", ")
     "\n"
     (string-join
      (map (lambda (curve-name)
	     (get-curve-points output-set curve-name from-date to-date))
	   curve-names)
      ""))))

(define (get-sxml-table output-set from-date to-date)
  (let* ((row-names (curve-names output-set))
	 (row-lists
	  (append
	   `(,(get-sxml-table-column
	       output-set (first row-names) from-date to-date #t))
	   (map (lambda (row-name)
		  (get-sxml-table-column output-set row-name
					 from-date to-date #f))
		row-names))))
    (cons 'table (apply map
			(lambda (. table-cells)
			  (cons 'tr table-cells))
			row-lists))))

(define (plot-svg output-set from-date to-date)
  (let* ((gp (run-with-pipe "r+" "gnuplot"))
	 (gp-pid (car gp))
	 (gp-in (cadr gp))
	 (gp-out (cddr gp))
	 (svg #f))
    (when +verbose+
      (file-log "gnuplot" (gnuplot-commands output-set from-date to-date)))
    (display (gnuplot-commands output-set from-date to-date)
	     gp-out)
    (close gp-out)
    (set! svg (read-delimited "" gp-in))
    (close gp-in)
    (waitpid gp-pid)
    (if (eof-object? svg)
	#f
	svg)))

(define (tabulate output-set from-date to-date)
  (with-output-to-string
    (lambda ()
      (display "<!DOCTYPE html>\n")
      (sxml->xml
       `(html
	 (head
	  (title ,output-set)
	  (style "table {border-collapse: collapse;}"
	    "td {border: 1px solid black; font-family: monospace;}"))
	 (body
	  (div
	   (@ (style "text-align:center; margin:auto auto 50px auto;"))
	   ,(get-sxml-table output-set from-date to-date))))))))

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

(dynamic-wind
  (lambda ()
    (set! *db* (dbi-open "postgresql" +db-connection+))
    (logged-query "db" "DROP CAST IF EXISTS (text AS numeric)")
    (flush-query)
    (logged-query "db" "CREATE CAST (text AS numeric) WITH INOUT AS IMPLICIT")
    (flush-query))
  (lambda ()
    (run-server p-rout-view
		'http
		`(#:port ,+port+ #:addr ,(inet-pton AF_INET +addr+))))
  (lambda () (dbi-close *db*)))


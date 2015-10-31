#!/usr/bin/guile -s
!#

;;;; Copyright (c) 2014, 2015 Bert Burgemeister  trebbu@googlemail.com
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

;;; Return first line of file at path, or return #f
(define (read-line-from-file path)
  (catch
    #t
    (lambda () (call-with-input-file path (lambda (in) (read-line in))))
    (lambda (k . args) #f)))

(define *version* (or (read-line-from-file "./VERSION")
		      (read-line-from-file "/usr/share/p-rout/VERSION")
		      "unknown"))

(define option-spec
  '((help (single-char #\h))
    (version (single-char #\V))
    (verbose (single-char #\v))
    (addr (single-char #\a) (value #t))
    (port (single-char #\p) (value #t))
    (db-connection (value #t))
    (log-dir (value #t))
    (gnuplot-lib-dir (value #t))
    (lib-dir (value #t))
    (no-daemon)
    (pid-file (value #t))
    (fhem-cfg)))

(define options (getopt-long (command-line) option-spec))

(when (option-ref options 'help #f)
  (display (car (command-line)))
  (display "\
 [options]
  -h, --help        Display this help
  -v, --verbose     Display debugging output
  -a, --addr        Address to listen on
  -p, --port        Port to listen on (default: 80)
  --db-connection   <user>:<pass>:<db>:<path/ip>[:port] (default:
                    p-rout:p-rout:p_rout:/run/postgres:localhost)
  --log-dir         Log directory (default: \"log-v\")
  --gnuplot-lib-dir Gnuplot's Javascript directory
  --lib-dir         p-rout-view's javascript directory (default:
                    /usr/share/p-rout/)
  --no-daemon       Remain in foreground
  --pid-file        Store daemon's PID here (default:
                    /var/run/p-rout/p-rout-view.pid)
  --fhem-cfg        Write an FHEM configuration file fragment to stdout
")
  (exit))

(when (option-ref options 'version #f)
  (display *version*)
  (newline)
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
(define +lib-dir+ (option-ref options 'lib-dir "/usr/share/p-rout/"))
(define +addr+ (option-ref options 'addr "192.168.178.51"))
(define +port+ (string->number (option-ref options 'port "80")))
(define +no-daemon+ (option-ref options 'no-daemon #f))
(define +pid-file+ (option-ref
		    options 'pid-file "/var/run/p-rout/p-rout-view.pid"))
(define +from-label+ "From")
(define +to-label+ "To")
(define +table-number-of-columns+ (* 12 60))
(define +diagram-number-of-values+ 400)
(define *db* #f)

(define +db-indexes+			;((schema table column where) ...)
  '(("logs" "header" "p_rout_id" #f)
    ("logs" "header" "time_send" #f)
    ("logs" "module_statuses" "p_rout_id" #f)
    ;; ("logs" "module_statuses" "ABS(param_2)" "module_id = 136")
    ;; ("logs" "module_statuses" "ABS(param_10)" "module_id = 12")
    ("logs" "module_statuses" "param_2" "module_id = 9")
    ("logs" "module_statuses" "param_10" "module_id = 12")
    ("logs" "module_statuses" "param_1" "module_id = 16")
    ;; ("logs" "module_statuses" "param_5" "module_id = 136")
    ;; ("logs" "module_statuses" "param_2" "module_id = 136")
    ;; ("logs" "module_statuses" "param_6" "module_id = 9")
    ("logs" "module_statuses" "p_rout_id" "module_id = 9")
    ("logs" "module_statuses" "p_rout_id" "module_id = 12")
    ("logs" "module_statuses" "p_rout_id" "module_id = 136")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; +output-sets+: presentation style definitions
;;;
;;; An output-set is a diagram, a table, or a set of latest values
;;; defined like this:
;;;
;;; '((<first-output-set-name>
;;;    (<SQL-schema> (<SQL-table-a> <SQL-table-b> ...) <SQL-time-column>
;;;     <indexpage-title>
;;;     <gnuplot-diagram-title-or-table-title> <as-table-or-diagram-or-values>)
;;;    ((<gnuplot-curve1-title>
;;;      <SQL-value1-column> <SQL-WHERE> <gnuplot-curve1-style>)
;;;     (<gnuplot-curve2-title>
;;;      <SQL-value2-column> <SQL-WHERE> <gnuplot-curve2-style>)
;;;     ...))
;;;   (<second-output-set-name>
;;;    (<SQL-schema> (<SQL-table-a> <SQL-table-b> ...) <SQL-time-column>
;;;     <indexpage-title>
;;;     <gnuplot-diagram-title-or-table-title> <as-table-or-diagram-or-values>)
;;;    ((<gnuplot-curve1-title>
;;;      <SQL-value1-column> <SQL-WHERE> <gnuplot-curve1-style>)
;;;     (<gnuplot-curve2-title>
;;;      <SQL-value2-column> <SQL-WHERE> <gnuplot-curve2-style>)
;;;     ...))
;;;   ...)
(define +output-sets+
  '(;; Diagrams
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; (<output-set-name>
    ;;  (<SQL-schema> (<SQL-table-a> <SQL-table-b> ...) <SQL-time-column>
    ;;                <indexpage-title>
    ;;                <gnuplot-diagram-title> as-diagram)
    ;;  ((<gnuplot-curve1-title>
    ;;    <SQL-value1-column> <SQL-WHERE> <gnuplot-curve1-style>)
    ;;   (<gnuplot-curve2-title>
    ;;    <SQL-value2-column> <SQL-WHERE> <gnuplot-curve2-style>)
    ;;   ...))
    ("overview-diagram"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Overview"
      "set keytitle '{/=12 Overview}'\n" as-diagram)
     (("SOC/100%"
       "module_statuses.param_5 / 100" "module_statuses.module_id = 136" "lines lw 2 lc rgb 'green'")
      ("P_{solar}/kW"
       "module_statuses.param_10 / 1000" "module_statuses.module_id = 12" "lines lw 2 lc rgb 'red'")
      ("P_{grid,dcac}/kW"
       "module_statuses.param_2 / 1000" "module_statuses.module_id = 9" "lines")
      ("P_{grid,platform}/kW"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 16" "lines")))
    ("battery-diagram"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Battery"
      "set keytitle '{/=12 Battery}'\n" as-diagram)
     (;; ("I_{charge}"
      ;;  "module_statuses.param_10 / 100" "module_statuses.module_id = 136" "lines")
      ("V_{charge}/V"
       "module_statuses.param_9 / 100" "module_statuses.module_id = 136" "lines")
      ("I_{batt}/A"
       "module_statuses.param_1 / 100" "module_statuses.module_id = 136" "lines lc rgb 'green'")
      ;; ("I_{discharge}"
      ;;  "module_statuses.param_12 / 100" "module_statuses.module_id = 136" "lines")
      ("V_{discharge}/V"
       "module_statuses.param_11 / 100" "module_statuses.module_id = 136" "lines")
      ("V_{batt}/V"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 136" "lines")
      ("T_{batt}/°C"
       "module_statuses.param_7 / 10" "module_statuses.module_id = 136" "lines")
      ("SOC/%"
       "module_statuses.param_5" "module_statuses.module_id = 136" "lines lw 2 lc rgb 'green'")))
    ("solar-diagram"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Solar"
      "set keytitle '{/=12 Solar}'\n" as-diagram)
     (("P/kW"
       "module_statuses.param_10 / 1000" "module_statuses.module_id = 12" "lines lw 2 lc rgb 'red'")
      ("P_1/kW"
       "module_statuses.param_2 / 1000" "module_statuses.module_id = 12" "lines")
      ("P_2/kW"
       "module_statuses.param_7 / 1000" "module_statuses.module_id = 12" "lines")
      ("V_1/100V"
       "module_statuses.param_0 / 10000" "module_statuses.module_id = 12" "lines")
      ("V_2/100V"
       "module_statuses.param_5 / 10000" "module_statuses.module_id = 12" "lines")
      ("I_1/A"
       "module_statuses.param_1 / 100" "module_statuses.module_id = 12" "lines")
      ("I_2/A"
       "module_statuses.param_6 / 100" "module_statuses.module_id = 12" "lines")))
    ("power-diagram"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Power"
      "set keytitle '{/=12 Power/W}'\n" as-diagram)
     (("P_{batt}"
       "module_statuses.param_2" "module_statuses.module_id = 136" "lines lc rgb 'green'")
      ("P_{solar}"
       "module_statuses.param_10" "module_statuses.module_id = 12" "lines lw 2 lc rgb 'red'")
      ("P_{1,solar}"
       "module_statuses.param_2" "module_statuses.module_id = 12" "lines")
      ("P_{2,solar}"
       "module_statuses.param_7" "module_statuses.module_id = 12" "lines")
      ("P_{local}"
       "module_statuses.param_6" "module_statuses.module_id = 9" "lines")
      ("P_{grid,dcac}"
       "module_statuses.param_2" "module_statuses.module_id = 9" "lines")
      ("P_{grid,platform}"
       "module_statuses.param_3" "module_statuses.module_id = 16" "lines")
      ("P_{L1}"
       "module_statuses.param_2" "module_statuses.module_id = 11" "lines lw 2 lc rgb 'orange'")
      ("P_{L2}"
       "module_statuses.param_6" "module_statuses.module_id = 11" "lines lw 2 lc rgb 'dark-red'")
      ("P_{L3}"
       "module_statuses.param_10" "module_statuses.module_id = 11" "lines lw 2 lc rgb 'violet'")))
    ("voltage-diagram"
     ("logs" ("header" "module_statuses") "header.time_send"
      "AC Voltage"
      "set keytitle '{/=12 AC Voltage/V}'\nset yrange [220:]\n" as-diagram)
     (("V_{local}"
       "module_statuses.param_5 / 10" "module_statuses.module_id = 9" "lines")
      ("V_{grid,dcac}"
       "module_statuses.param_1 / 10" "module_statuses.module_id = 9" "lines")
      ("V_{grid,platform}"
       "module_statuses.param_1 / 10" "module_statuses.module_id = 16" "lines")
      ("V_{L1}"
       "module_statuses.param_0 / 10" "module_statuses.module_id = 11" "lines lw 2 lc rgb 'orange'")
      ("V_{L2}"
       "module_statuses.param_4 / 10" "module_statuses.module_id = 11" "lines lw 2 lc rgb 'dark-red'")
      ("V_{L3}"
       "module_statuses.param_8 / 10" "module_statuses.module_id = 11" "lines lw 2 lc rgb 'violet'")))
    ("frequency-diagram"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Frequency"
      "set keytitle '{/=12 Frequency/Hz}'\nset yrange [48:]" as-diagram)
     (("f_{dcac}"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 9" "lines")
      ("f_{platform}"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 16" "lines")))
    ("temperature-diagram"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Temperature"
      "set keytitle '{/=12 Temperature/°C}'\n" as-diagram)
     (("T_{batt}"
       "module_statuses.param_7 / 10" "module_statuses.module_id = 136" "lines lw 2 lc rgb 'green'")
      ("T_{batt,module}"
       "module_statuses.param_8 / 10" "module_statuses.module_id = 136" "lines lc rgb 'green'")
      ("T_{1,solar}"
       "module_statuses.param_4 / 10" "module_statuses.module_id = 12" "lines lc rgb 'red'")
      ("T_{2,solar}"
       "module_statuses.param_9 / 10" "module_statuses.module_id = 12" "lines lc rgb 'dark-red'")
      ("T_{dcac}"
       "module_statuses.param_10 / 10" "module_statuses.module_id = 9" "lines lc rgb 'blue'")
      ("T_{platform}"
       "module_statuses.param_2 / 10" "module_statuses.module_id = 16" "lines lc rgb 'dark-green'")))
    ;; Tables
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; (<output-set-name>
    ;;  (<SQL-schema> (<SQL-table-a> <SQL-table-b> ...) <SQL-time-column>
    ;;                <indexpage-title>
    ;;                <table-title> as-table)
    ;;  ((<table-column1-header>
    ;;    <SQL-value1-column> <SQL-WHERE>)
    ;;   (<tablecolumn2-header>
    ;;    <SQL-value2-column> <SQL-WHERE>)
    ;;   ...))
    ("battery-table"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Battery"
      "Battery" as-table)
     ( ('("I" (sub "charge") "/A")
       "module_statuses.param_10 / 100" "module_statuses.module_id = 136")
      ('("V" (sub "charge") "/V")
       "module_statuses.param_9 / 100" "module_statuses.module_id = 136")
      ('("I" (sub "batt") "/A")
       "module_statuses.param_1 / 100" "module_statuses.module_id = 136")
      ('("I" (sub "discharge") "/A" )
       "module_statuses.param_12 / 100" "module_statuses.module_id = 136")
      ('("V" (sub "discharge") "/V" )
       "module_statuses.param_11 / 100" "module_statuses.module_id = 136")
      ('("V" (sub "batt") "/V" )
       "module_statuses.param_0 / 100" "module_statuses.module_id = 136")
      ('("P" (sub "batt") "/W")
       "module_statuses.param_2" "module_statuses.module_id = 136")
      ('("T" (sub "batt") "/°C" )
       "module_statuses.param_7 / 10" "module_statuses.module_id = 136")
      ("SOC/%"
       "module_statuses.param_5" "module_statuses.module_id = 136")
      ("Status"
       "module_statuses.status" "module_statuses.module_id = 136")))
    ("solar-table"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Solar"
      "Solar" as-table)
     (("P/W"
       "module_statuses.param_10" "module_statuses.module_id = 12")
      ('("P" (sub "1") "/W")
       "module_statuses.param_2" "module_statuses.module_id = 12")
      ('("P" (sub "2") "/W")
       "module_statuses.param_7" "module_statuses.module_id = 12")
      ('("V" (sub "1") "/V")
       "module_statuses.param_0 / 100" "module_statuses.module_id = 12")
      ('("V" (sub "2") "/V")
       "module_statuses.param_5 / 100" "module_statuses.module_id = 12")
      ('("I" (sub "1") "/A")
       "module_statuses.param_1 / 100" "module_statuses.module_id = 12")
      ('("I" (sub "2") "/A")
       "module_statuses.param_6 / 100" "module_statuses.module_id = 12")
      ("Status"
       "module_statuses.status" "module_statuses.module_id = 12")))
    ("power-table"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Power"
      "Power/W" as-table)
     (('("P" (sub "batt"))
       "module_statuses.param_2" "module_statuses.module_id = 136")
      ('("Stat" (sub "batt"))
       "module_statuses.status" "module_statuses.module_id = 136")
      ('("P" (sub "solar"))
       "module_statuses.param_10" "module_statuses.module_id = 12")
      ('("P" (sub "1,solar"))
       "module_statuses.param_2" "module_statuses.module_id = 12")
      ('("P" (sub "2,solar"))
       "module_statuses.param_7" "module_statuses.module_id = 12")
      ('("Stat" (sub "solar"))
       "module_statuses.status" "module_statuses.module_id = 12")
      ('("P" (sub "local"))
       "module_statuses.param_6" "module_statuses.module_id = 9")
      ('("P" (sub "grid,dcac"))
       "module_statuses.param_2" "module_statuses.module_id = 9")
      ('("Stat" (sub "dcac"))
       "module_statuses.status" "module_statuses.module_id = 9")
      ('("P" (sub "grid,platform"))
       "module_statuses.param_3" "module_statuses.module_id = 16")
      ('("Stat" (sub "platform"))
       "module_statuses.status" "module_statuses.module_id = 16")
      ('("P" (sub "L1"))
       "module_statuses.param_2" "module_statuses.module_id = 11")
      ('("P" (sub "L2"))
       "module_statuses.param_6" "module_statuses.module_id = 11")
      ('("P" (sub "L3"))
       "module_statuses.param_10" "module_statuses.module_id = 11")
      ('("Stat" (sub "grid_sensor"))
       "module_statuses.status" "module_statuses.module_id = 11")))
    ("voltage-table"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Voltage"
      "Voltage/V" as-table)
     (('("V" (sub "batt" ))
       "module_statuses.param_0 / 100" "module_statuses.module_id = 136")
      ('("V" (sub "1,solar"))
       "module_statuses.param_0 / 100" "module_statuses.module_id = 12")
      ('("V" (sub "2,solar"))
       "module_statuses.param_5 / 100" "module_statuses.module_id = 12")
      ('("V" (sub "local"))
       "module_statuses.param_5 / 10" "module_statuses.module_id = 9")
      ('("V" (sub "grid,dcac"))
       "module_statuses.param_1 / 10" "module_statuses.module_id = 9")
      ('("V" (sub "grid,platform"))
       "module_statuses.param_1 / 10" "module_statuses.module_id = 16")
      ('("V" (sub "L1"))
       "module_statuses.param_0 / 10" "module_statuses.module_id = 11")
      ('("V" (sub "L2"))
       "module_statuses.param_4 / 10" "module_statuses.module_id = 11")
      ('("V" (sub "L3"))
       "module_statuses.param_8 / 10" "module_statuses.module_id = 11")))
    ("frequency-table"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Frequency"
      "Frequency/Hz" as-table)
     (('("f" (sub "dcac"))
       "module_statuses.param_0 / 100" "module_statuses.module_id = 9")
      ('("f" (sub "platform"))
       "module_statuses.param_0 / 100" "module_statuses.module_id = 16")))
    ("temperature-table"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Temperature"
      "Temperature/°C" as-table)
     (('("T" (sub "batt"))
       "module_statuses.param_7 / 10" "module_statuses.module_id = 136")
      ('("T" (sub "batt,module"))
       "module_statuses.param_8 / 10" "module_statuses.module_id = 136")
      ('("T" (sub "1,solar"))
       "module_statuses.param_4 / 10" "module_statuses.module_id = 12")
      ('("T" (sub "2,solar"))
       "module_statuses.param_9 / 10" "module_statuses.module_id = 12")
      ('("T" (sub "dcac"))
       "module_statuses.param_10 / 10" "module_statuses.module_id = 9")
      ('("T" (sub "platform"))
       "module_statuses.param_2 / 10" "module_statuses.module_id = 16")))
    ("energy-table"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Energy"
      "Energy/kWh" as-table)
     (("Battery consumed"
       "module_statuses.param_4 / 1000" "module_statuses.module_id = 136")
      ("Battery produced"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 136")
      ("Solar produced"
       "module_statuses.param_11 / 1000" "module_statuses.module_id = 12")
      ("Solar1 produced"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 12")
      ("Solar2 produced"
       "module_statuses.param_8 / 1000" "module_statuses.module_id = 12")
      ("Local DC/AC consumed"
       "module_statuses.param_7 / 1000" "module_statuses.module_id = 9")
      ("DC/AC consumed"
       "module_statuses.param_4 / 1000" "module_statuses.module_id = 9")
      ("DC/AC produced"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 9")
      ("Platform consumed"
       "module_statuses.param_5 / 1000" "module_statuses.module_id = 16")
      ("Platform produced"
       "module_statuses.param_4 / 1000" "module_statuses.module_id = 16")
      ("L1 consumed"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 11")
      ("L2 consumed"
       "module_statuses.param_7 / 1000" "module_statuses.module_id = 11")
      ("L3 consumed"
       "module_statuses.param_11 / 1000" "module_statuses.module_id = 11")))
    ("status-table"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Status"
      "Raw status strings" as-table)
     (("Battery"
       "module_statuses.status" "module_statuses.module_id = 136")
      ("Solar"
       "module_statuses.status" "module_statuses.module_id = 12")
      ("DC/AC"
       "module_statuses.status" "module_statuses.module_id = 9")
      ("Platform"
       "module_statuses.status" "module_statuses.module_id = 16")
      ("Grid sensor"
       "module_statuses.status" "module_statuses.module_id = 11")))
    ("events-table"
     ("events" ("header" "event") "header.time_send"
      "Events"
      "Raw event strings" as-table)
     (("Event"
       "event.data" #f)))
    ;; Front page values
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; (<output-set-name>
    ;;  (<SQL-schema> (<SQL-table-a> <SQL-table-b> ...) <SQL-time-column>
    ;;                <title-unused>
    ;;                <table-title> as-values)
    ;;  ((<row1-title>
    ;;    <SQL-value1-column> <SQL-WHERE>)
    ;;   (<row2-title>
    ;;    <SQL-value2-column> <SQL-WHERE>)
    ;;   ...))
    ("current-battery"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Battery"
      "Battery" as-values)
     (('("I" (sub "charge") "/A")
       "module_statuses.param_10 / 100" "module_statuses.module_id = 136")
      ('("V" (sub "charge") "/V")
       "module_statuses.param_9 / 100" "module_statuses.module_id = 136")
      ('("I" (sub "batt") "/A")
       "module_statuses.param_1 / 100" "module_statuses.module_id = 136")
      ('("I" (sub "discharge") "/A" )
       "module_statuses.param_12 / 100" "module_statuses.module_id = 136")
      ('("V" (sub "discharge") "/V" )
       "module_statuses.param_11 / 100" "module_statuses.module_id = 136")
      ('("V" (sub "batt") "/V" )
       "module_statuses.param_0 / 100" "module_statuses.module_id = 136")
      ('("P" (sub "batt") "/W")
       "module_statuses.param_2" "module_statuses.module_id = 136")
      ('("T" (sub "batt") "/°C" )
       "module_statuses.param_7 / 10" "module_statuses.module_id = 136")
      ("SOC/%"
       "module_statuses.param_5" "module_statuses.module_id = 136")))
    ("current-solar"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Solar"
      "Solar" as-values)
     (("P/W"
       "module_statuses.param_10" "module_statuses.module_id = 12")
      ('("P" (sub "1") "/W")
       "module_statuses.param_2" "module_statuses.module_id = 12")
      ('("P" (sub "2") "/W")
       "module_statuses.param_7" "module_statuses.module_id = 12")
      ('("V" (sub "1") "/V")
       "module_statuses.param_0 / 100" "module_statuses.module_id = 12")
      ('("V" (sub "2") "/V")
       "module_statuses.param_5 / 100" "module_statuses.module_id = 12")
      ('("I" (sub "1") "/A")
       "module_statuses.param_1 / 100" "module_statuses.module_id = 12")
      ('("I" (sub "2") "/A")
       "module_statuses.param_6 / 100" "module_statuses.module_id = 12")))
    ("current-power"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Power"
      "Power/W" as-values)
     (('("P" (sub "batt"))
       "module_statuses.param_2" "module_statuses.module_id = 136")
      ('("P" (sub "bus,batt"))
       "module_statuses.param_2" "module_statuses.module_id = 136")
      ('("P" (sub "solar"))
       "module_statuses.param_10" "module_statuses.module_id = 12")
      ('("P" (sub "1,solar"))
       "module_statuses.param_2" "module_statuses.module_id = 12")
      ('("P" (sub "2,solar"))
       "module_statuses.param_7" "module_statuses.module_id = 12")
      ('("P" (sub "local"))
       "module_statuses.param_6" "module_statuses.module_id = 9")
      ('("P" (sub "grid,dcac"))
       "module_statuses.param_2" "module_statuses.module_id = 9")
      ('("P" (sub "grid,platform"))
       "module_statuses.param_3" "module_statuses.module_id = 16")
      ('("P" (sub "L1"))
       "module_statuses.param_2" "module_statuses.module_id = 11")
      ('("P" (sub "L2"))
       "module_statuses.param_6" "module_statuses.module_id = 11")
      ('("P" (sub "L3"))
       "module_statuses.param_10" "module_statuses.module_id = 11")))
    ("current-voltage"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Voltage"
      "AC Voltage/V" as-values)
     (('("V" (sub "local"))
       "module_statuses.param_5 / 10" "module_statuses.module_id = 9")
      ('("V" (sub "grid,dcac"))
       "module_statuses.param_1 / 10" "module_statuses.module_id = 9")
      ('("V" (sub "bus,dcac"))
       "module_statuses.param_8 / 100" "module_statuses.module_id = 9")
      ('("V" (sub "grid,platform"))
       "module_statuses.param_1 / 10" "module_statuses.module_id = 16")
      ('("V" (sub "L1"))
       "module_statuses.param_0 / 10" "module_statuses.module_id = 11")
      ('("V" (sub "L2"))
       "module_statuses.param_4 / 10" "module_statuses.module_id = 11")
      ('("V" (sub "L3"))
       "module_statuses.param_8 / 10" "module_statuses.module_id = 11")))
    ("current-current"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Current"
      "Current/A" as-values)
     (('("I" (sub "charge"))
       "module_statuses.param_10 / 100" "module_statuses.module_id = 136")
      ('("I" (sub "discharge"))
       "module_statuses.param_12 / 100" "module_statuses.module_id = 136")
      ('("I" (sub "batt"))
       "module_statuses.param_1 / 100" "module_statuses.module_id = 136")
      ('("I" (sub "1,solar"))
       "module_statuses.param_1 / 100" "module_statuses.module_id = 12")
      ('("I" (sub "2,solar"))
       "module_statuses.param_6 / 100" "module_statuses.module_id = 12")
      ('("I" (sub "L1"))
       "module_statuses.param_1 / 100" "module_statuses.module_id = 11")
      ('("I" (sub "L2"))
       "module_statuses.param_5 / 100" "module_statuses.module_id = 11")
      ('("I" (sub "L3"))
       "module_statuses.param_9 / 100" "module_statuses.module_id = 11")))
    ("current-frequency"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Frequency"
      "Frequency/Hz" as-values)
     (('("f" (sub "dcac"))
       "module_statuses.param_0 / 100" "module_statuses.module_id = 9")
      ('("f" (sub "platform"))
       "module_statuses.param_0 / 100" "module_statuses.module_id = 16")))
    ("current-temperature"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Temperature"
      "Temperature/°C" as-values)
     (('("T" (sub "batt"))
       "module_statuses.param_7 / 10" "module_statuses.module_id = 136")
      ('("T" (sub "batt,module"))
       "module_statuses.param_8 / 10" "module_statuses.module_id = 136")
      ('("T" (sub "1,solar"))
       "module_statuses.param_4 / 10" "module_statuses.module_id = 12")
      ('("T" (sub "2,solar"))
       "module_statuses.param_9 / 10" "module_statuses.module_id = 12")
      ('("T" (sub "dcac"))
       "module_statuses.param_10 / 10" "module_statuses.module_id = 9")
      ('("T" (sub "platform"))
       "module_statuses.param_2 / 10" "module_statuses.module_id = 16")))
    ("current-energy"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Energy"
      "Energy/kWh" as-values)
     (("Battery consumed"
       "module_statuses.param_4 / 1000" "module_statuses.module_id = 136")
      ("Battery produced"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 136")
      ("Solar produced"
       "module_statuses.param_11 / 1000" "module_statuses.module_id = 12")
      ("Solar 1 produced"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 12")
      ("Solar 2 produced"
       "module_statuses.param_8 / 1000" "module_statuses.module_id = 12")
      ("Local DC/AC consumed"
       "module_statuses.param_7 / 1000" "module_statuses.module_id = 9")
      ("DC/AC consumed"
       "module_statuses.param_4 / 1000" "module_statuses.module_id = 9")
      ("DC/AC produced"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 9")
      ("Platform consumed"
       "module_statuses.param_5 / 1000" "module_statuses.module_id = 16")
      ("Platform produced"
       "module_statuses.param_4 / 1000" "module_statuses.module_id = 16")
      ("L1 consumed"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 11")
      ("L2 consumed"
       "module_statuses.param_7 / 1000" "module_statuses.module_id = 11")
      ("L3 consumed"
       "module_statuses.param_11 / 1000" "module_statuses.module_id = 11")))
    ("current-status"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Status"
      "Status" as-values)
     (("Battery"
       "module_statuses.status" "module_statuses.module_id = 136")
      ("Solar"
       "module_statuses.status" "module_statuses.module_id = 12")
      ("DC/AC"
       "module_statuses.status" "module_statuses.module_id = 9")
      ("Platform"
       "module_statuses.status" "module_statuses.module_id = 16")
      ("Grid sensor"
       "module_statuses.status" "module_statuses.module_id = 11")
      ("PR Id"
       "header.powerrouter_id" "TRUE")))
    ;; Aggregate values: Energy balance
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; (<output-set-name>
    ;;  (<SQL-schema> (<SQL-table-a> <SQL-table-b> ...) <SQL-time-column>
    ;;                <>
    ;;                <> as-balance)
    ;;  ((<value1-title>
    ;;    <SQL-value1-column> <SQL-WHERE>)
    ;;   (<value2-title>
    ;;    <SQL-value2-column> <SQL-WHERE>)
    ;;   ...))
    ("balance"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Balance"
      "Balance" as-balance)
     (('("W" (sub "prod") "/kWh")
       "(w_dcac_1::NUMERIC - w_dcac_0::NUMERIC) / 1000" "")
      ('("W" (sub "sold") "/kWh")
       "(w_platform_1::NUMERIC - w_platform_0::NUMERIC) / 1000" "")
      ('("W" (sub "used") "/kWh")
       "(w_dcac_1::NUMERIC - w_dcac_0::NUMERIC - (w_platform_1::NUMERIC - w_platform_0::NUMERIC)) / 1000" "")))
    ;; Aggregate values: DCAC efficiency
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; (<output-set-name>
    ;;  (<SQL-schema> (<SQL-table-a> <SQL-table-b> ...) <SQL-time-column>
    ;;                <>
    ;;                <> as-dcac-efficiency)
    ;;  ((<value1-title>
    ;;    <SQL-value1-column> <SQL-WHERE>)
    ;;   (<value2-title>
    ;;    <SQL-value2-column> <SQL-WHERE>)
    ;;   ...))
    ("dcac-efficiency"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Efficiency"
      "Efficiency" as-dcac-efficiency)
     (('("eta" (sub "solar,DC/AC"))
       "((ROUND((AVG(p_dcac_local::NUMERIC - p_dcac::NUMERIC) / AVG(p_solar::NUMERIC)), 3))::DOUBLE PRECISION)" "ABS(p_batt::NUMERIC) < 5")
      ('("eta" (sub "batt,DC/AC"))
       "((ROUND((AVG(p_dcac_local::NUMERIC - p_dcac::NUMERIC) / AVG(p_batt::NUMERIC)), 3))::DOUBLE PRECISION)" "ABS(p_solar::NUMERIC) < 1")))
    ;; Aggregate values: Battery efficiency
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; (<output-set-name>
    ;;  (<SQL-schema> (<SQL-table-a> <SQL-table-b> ...) <SQL-time-column>
    ;;                <>
    ;;                <> as-battery-efficiency)
    ;;  ((<value1-title>
    ;;    <SQL-value1-column> <SQL-WHERE>)
    ;;   (<value2-title>
    ;;    <SQL-value2-column> <SQL-WHERE>)
    ;;   ...))
    ("battery-efficiency"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Efficiency"
      "Efficiency" as-battery-efficiency)
     (('("eta" (sub "batt"))
       "" "soc = 95")))
    ;; Simple aggregate values
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; (<output-set-name>
    ;;  (<SQL-schema> (<SQL-table-a> <SQL-table-b> ...) <SQL-time-column>
    ;;                <>
    ;;                <> as-extremum)
    ;;  ((<value1-title>
    ;;    <SQL-value1-column> <SQL-WHERE>)
    ;;   (<value2-title>
    ;;    <SQL-value2-column> <SQL-WHERE>)
    ;;   ...))
    ("extrema"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Extrema"
      "Extrema" as-extremum)
     (('("P" (sub "1,solar,max"))
       "max(module_statuses.param_2::NUMERIC)" "module_statuses.module_id = 12")
      ('("P" (sub "2,solar,max"))
       "max(module_statuses.param_7::NUMERIC)" "module_statuses.module_id = 12")))
    ;; Raw values
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Parameter names should be alphanumeric strings (including '_')
    ;; without whitespace.
    ;; (<output-set-name>
    ;;  (<SQL-schema> (<SQL-table-a> <SQL-table-b> ...) <SQL-time-column>
    ;;                <title-unused>
    ;;                <title-unused> as-raw-values)
    ;;  ((<value1-title>
    ;;    <SQL-value1-column> <SQL-WHERE>)
    ;;   (<value2-title>
    ;;    <SQL-value2-column> <SQL-WHERE>)
    ;;   ...))
    ("current-raw"
     ("logs" ("header" "module_statuses") "header.time_send"
      ""
      "" as-raw-values)
     (("date"
       "header.time_send" "TRUE")
      ("SOC/percent"
       "module_statuses.param_5" "module_statuses.module_id = 136")
      ("P_batt/W"
       "module_statuses.param_2" "module_statuses.module_id = 136")
      ("P_bus_batt/W"
       "module_statuses.param_2" "module_statuses.module_id = 136")
      ("P_local/W"
       "module_statuses.param_6" "module_statuses.module_id = 9")
      ("P_grid_dcac/W"
       "module_statuses.param_2" "module_statuses.module_id = 9")
      ("P_grid_platform/W"
       "module_statuses.param_3" "module_statuses.module_id = 16")
      ("P_solar/W"
       "module_statuses.param_10" "module_statuses.module_id = 12")
      ("P_1_solar/W"
       "module_statuses.param_2" "module_statuses.module_id = 12")
      ("P_2_solar/W"
       "module_statuses.param_7" "module_statuses.module_id = 12")
      ("P_L1/W"
       "module_statuses.param_2" "module_statuses.module_id = 11")
      ("P_L2/W"
       "module_statuses.param_6" "module_statuses.module_id = 11")
      ("P_L3/W"
       "module_statuses.param_10" "module_statuses.module_id = 11")
      ("V_local/V"
       "module_statuses.param_5 / 10" "module_statuses.module_id = 9")
      ("V_grid_dcac/V"
       "module_statuses.param_1 / 10" "module_statuses.module_id = 9")
      ("V_bus_dcac/V"
       "module_statuses.param_8 / 100" "module_statuses.module_id = 9")
      ("V_grid_platform/V"
       "module_statuses.param_1 / 10" "module_statuses.module_id = 16")
      ("V_L1/V"
       "module_statuses.param_0 / 10" "module_statuses.module_id = 11")
      ("V_L2/V"
       "module_statuses.param_4 / 10" "module_statuses.module_id = 11")
      ("V_L3/V"
       "module_statuses.param_8 / 10" "module_statuses.module_id = 11")
      ("V_1_solar/V"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 12")
      ("V_2_solar/V"
       "module_statuses.param_5 / 100" "module_statuses.module_id = 12")
      ("V_charge/V"
       "module_statuses.param_9 / 100" "module_statuses.module_id = 136")
      ("V_discharge/V"
       "module_statuses.param_11 / 100" "module_statuses.module_id = 136")
      ("V_batt/V"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 136")
      ("I_charge/A"
       "module_statuses.param_10 / 100" "module_statuses.module_id = 136")
      ("I_discharge/A"
       "module_statuses.param_12 / 100" "module_statuses.module_id = 136")
      ("I_batt/A"
       "module_statuses.param_1 / 100" "module_statuses.module_id = 136")
      ("I_1_solar/A"
       "module_statuses.param_1 / 100" "module_statuses.module_id = 12")
      ("I_2_solar/A"
       "module_statuses.param_6 / 100" "module_statuses.module_id = 12")
      ("I_L1/A"
       "module_statuses.param_1 / 100" "module_statuses.module_id = 11")
      ("I_L2/A"
       "module_statuses.param_5 / 100" "module_statuses.module_id = 11")
      ("I_L3/A"
       "module_statuses.param_9 / 100" "module_statuses.module_id = 11")
      ("f_dcac/Hz"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 9")
      ("f_platform/Hz"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 16")
      ("T_dcac/degC"
       "module_statuses.param_10 / 10" "module_statuses.module_id = 9")
      ("T_platform/degC"
       "module_statuses.param_2 / 10" "module_statuses.module_id = 16")
      ("T_batt/degC"
       "module_statuses.param_7 / 10" "module_statuses.module_id = 136")
      ("T_batt_module/degC"
       "module_statuses.param_8 / 10" "module_statuses.module_id = 136")
      ("T_1_solar/degC"
       "module_statuses.param_4 / 10" "module_statuses.module_id = 12")
      ("T_2_solar/degC"
       "module_statuses.param_9 / 10" "module_statuses.module_id = 12")
      ("W_platform_consumed/kWh"
       "module_statuses.param_5 / 1000" "module_statuses.module_id = 16")
      ("W_platform_produced/kWh"
       "module_statuses.param_4 / 1000" "module_statuses.module_id = 16")
      ("W_dcac_consumed/kWh"
       "module_statuses.param_4 / 1000" "module_statuses.module_id = 9")
      ("W_dcac_produced/kWh"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 9")
      ("W_local_dcac_consumed/kWh"
       "module_statuses.param_7 / 1000" "module_statuses.module_id = 9")
      ("W_battery_consumed/kWh"
       "module_statuses.param_4 / 1000" "module_statuses.module_id = 136")
      ("W_battery_produced/kWh"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 136")
      ("W_solar_produced/kWh"
       "module_statuses.param_11 / 1000" "module_statuses.module_id = 12")
      ("W_solar_1_produced/kWh"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 12")
      ("W_solar_2_produced/kWh"
       "module_statuses.param_8 / 1000" "module_statuses.module_id = 12")
      ("W_L1_consumed/kWh"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 11")
      ("W_L2_consumed/kWh"
       "module_statuses.param_7 / 1000" "module_statuses.module_id = 11")
      ("W_L3_consumed/kWh"
       "module_statuses.param_11 / 1000" "module_statuses.module_id = 11")
      ("status_platform"
       "module_statuses.status" "module_statuses.module_id = 16")
      ("status_battery"
       "module_statuses.status" "module_statuses.module_id = 136")
      ("status_dcac"
       "module_statuses.status" "module_statuses.module_id = 9")
      ("status_solar"
       "module_statuses.status" "module_statuses.module_id = 12")
      ("status_grid_sensor"
       "module_statuses.status" "module_statuses.module_id = 11")
      ("pr_id"
       "header.powerrouter_id" "TRUE")))))

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

(define (indexpage-title output-set)
  (fourth (cadr (assoc output-set +output-sets+))))

(define (gnuplot-settings output-set)
  (fifth (cadr (assoc output-set +output-sets+))))

(define (table-title output-set)
  (gnuplot-settings output-set))

(define (render-mode output-set)
  (sixth (cadr (assoc output-set +output-sets+))))

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

(define (underscorify string)
  (string-downcase
   (string-join (string-tokenize string char-set:letter+digit) "_")))

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
  ;; guile-dbd-postgresql v2.1.4 refuses to do anything until we've read
  ;; any previous results
  (while (dbi-get_row *db*))
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

;;; Return in a list two date-strings of around a day before and after now
(define (around-now)
  (let* ((now (string->date (now) "~Y-~m-~dT~H:~M"))
	 (year (date-year now))
	 (month (date-month now))
	 (day (date-day now))
	 (hour (date-hour now))
	 (zone-offset (date-zone-offset now))
	 (latest-halfday-hour (- hour (modulo hour 12)))
         (latest-halfday-time
          (date->time-utc
           (make-date 0 0 0 latest-halfday-hour day month year zone-offset)))
         (one-day (make-time time-duration 0 (* 24 3600))))
    (map
     (lambda (time) (date->string (time-utc->date time) "~Y-~m-~dT~H:~M"))
     (list
      (subtract-duration latest-halfday-time one-day)
      (add-duration latest-halfday-time one-day)))))

;;; Return date-intervals appended with n previous date intervals of the
;;; same duration
(define (previous-date-intervals n date-intervals)
  (if (zero? n)
      date-intervals
      (let* ((current-date0 (last (car date-intervals)))
	     (current-date1 (last (cdr date-intervals)))
	     (current-time0
	      (date->time-utc (string->date current-date0
					    "~Y-~m-~dT~H:~M")))
	     (current-time1
	      (date->time-utc (string->date current-date1
					    "~Y-~m-~dT~H:~M")))
	     (duration (time-difference current-time1 current-time0))
	     (previous-time0 (subtract-duration current-time0 duration)))
	(previous-date-intervals
	 (1- n)
	 (cons (append (car date-intervals)
		       (list (date->string (time-utc->date previous-time0)
					   "~Y-~m-~dT~H:~M")))
	       (append (cdr date-intervals) (list current-date0)))))))

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
	((equal? '("view" "raw") (take (uri-elements request) 2))
	 (view-raw-handler request body))
	(else (not-found request))))

;;; Wrong URL
(define (not-found request)
  (values (build-response #:code 404)
	  (string-append "Not found: "
			 (uri->string (request-uri request)))))

;;; SXML for a date input stuffed with default-date
(define (sxml-date-input label default-date)
  `((label (@ (for ,label))
       ,(string-append label " "))
    (input (@ (type "Text")
	      (id ,label)
	      (name ,label)
	      (maxlength "16")
	      (size "16")
	      (value ,default-date)
	      (onclick ,(string-append "javascript:NewCssCal('"
				       label
				       "','yyyyMMdd','dropdown',true,'24')"))))))

;;; List of Powerrouters
(define (powerrouters)
  (logged-query "db" (string-append
		      "SELECT powerrouter_id"
		      " FROM logs.header"
		      " GROUP BY powerrouter_id"
		      " ORDER BY powerrouter_id DESC"))
  (do ((powerrouter (dbi-get_row *db*)
		    (dbi-get_row *db*))
       (powerrouters '()))
      ((not powerrouter)
       powerrouters)
    (set! powerrouters (cons (cdar powerrouter) powerrouters))))

;;; SXML for the selection of a particular Powerrouter (if there are
;;; Powerrouters to select from)
(define (sxml-powerrouter-select)
  `(span
    (@ (,(if (> (length (powerrouters)) 1)
	     'dummy
	     'hidden)))
    "PR Id "
    ,(list* 'select
	    '(@ (name "powerrouter-id"))
	    (map (lambda (powerrouter)
		   `(option (@ (value ,powerrouter))
			    ,powerrouter))
		 (powerrouters)))
    " "))

;;; SXML for a bunch of radio buttons for output sets of wanted-render-mode
(define (sxml-output-set-inputs wanted-render-mode)
  (map (lambda (output-set)
	 `(input (@ (type "radio")
		    (name "output-set")
		    (value ,output-set))
		 ,(string-append (indexpage-title output-set) " ")))
       (filter (lambda (x) (eq? wanted-render-mode (render-mode x)))
	       (output-sets))))

;;; SXML for a bunch of tables of latest values
(define (sxml-latest-value-tables-div)
  `(div
    (@ (style "width:80%;" "margin:0 auto;" "text-align:center;"))
    ,@(map
       (lambda (powerrouter-id)
	 `(div (@ (style "clear:both;" "padding-top:10px;"))
	       (h4 "Current Values"
		   ,(if (> (length (powerrouters)) 1)
			(string-append " of " powerrouter-id)
			"")
		   " [" ,(current-value-date powerrouter-id) "]")
	       ,@(map (lambda (output-set)
			`(div
			  (@ (style "float:left;" "padding:10px 2px"))
			  ,(get-latest-value-sxml-table output-set
							powerrouter-id)))
		      (filter (lambda (x) (eq? 'as-values (render-mode x)))
			      (output-sets)))))
       (powerrouters))))

(define (view-handler request body)
  (values (build-response #:code 200
			  #:reason-phrase "Ok"
			  #:headers `((content-type . (text/html))
				      (charset . "utf-8")))
	  (let ((default-dates (map humanize-date-string (around-now))))
	    (with-output-to-string
	      (lambda ()
		(display "<!DOCTYPE html>\n")
		(sxml->xml
		 `(html
		   (head
		    (title "p-rout")
		    (style "td {border: 1px solid black;}")
		    (script (@ (src "/view/lib/datetimepicker_css.js")) ""))
		   (body
		    (@ (style "font-family: monospace;"))
		    "p-rout v" ,*version*
		    (div
		     (@ (style "text-align:center;"
			  "margin:50px auto 50px auto;"))
		     (form
		      (@ (action "/view/render"))
		      (p ,(sxml-powerrouter-select)
			 ,(sxml-date-input +from-label+ (first default-dates))
			 " "
			 ,(sxml-date-input +to-label+ (second default-dates)))
		      (h4 "Diagrams")
		      (p ,(sxml-output-set-inputs 'as-diagram))
		      (h4 "Tables")
		      (p ,(sxml-output-set-inputs 'as-table))
		      (p (input (@ (type "radio")
				(name "output-set")
				(value "AGGREGATES"))
				"Energy Balance & Efficiency (be patient)"))
		      (p (input (@ (type "submit")
				   (value "Go"))))))
		    ,(sxml-latest-value-tables-div)))))))))

(define (view-raw-handler request body)
  (values (build-response #:code 200
			  #:reason-phrase "Ok"
			  #:headers `((content-type . (text/plain))
				      (charset . "utf-8")))
	  (let ((powerrouter-id (third (uri-elements request))))
	    (get-raw-values-text "current-raw" powerrouter-id))))

(define (view-render-handler request body)
  (let* ((output-set (cdr (assoc "output-set"
				 (uri-query-components request))))
	 (render-mode (if (equal? output-set "AGGREGATES")
			  'as-table
			  (render-mode output-set))))
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
		    (cdr (assoc "powerrouter-id" query-alist))
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
		    (cdr (assoc "powerrouter-id" query-alist))
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
	  (let ((file-name
		 (string-append
		  +lib-dir+ "/" (uri-file-name request))))
	    (catch #t
	      (lambda ()
		(with-input-from-file file-name
		  (lambda () (read-delimited ""))))
	      (lambda (err . args) "nothing here")))))

;;; Return a SQL statement that fetches up to number-of-rows (date value) pairs
;;; for a curve from an output-set and from a time interval
(define (get-sql-row-sql
	 output-set curve-name powerrouter-id from-date to-date number-of-rows)
  (string-append
   "WITH t (row_number, date, value) AS"
   " (SELECT row_number() OVER (ORDER BY " (date-column output-set) ")"
   ", " (date-column output-set)
   ", " (columnname output-set curve-name)
   " FROM "
   (let ((tables (tables output-set)))
     (if (> (length tables) 1)
	 (string-append (string-join tables " JOIN ")
			" USING (" +record-id-column+ ")")
	 (car tables)))
   " WHERE ("
   (date-column output-set)
   " BETWEEN '" from-date "' AND '" to-date "')"
   " AND powerrouter_id = '" powerrouter-id "'"
   (let ((sql-where (sql-where output-set curve-name)))
     (if sql-where
	 (string-append " AND " sql-where)
	 ""))
   ")"
   " SELECT date, value FROM t WHERE row_number % (SELECT 1 + count(*) / "
   (number->string number-of-rows)
   " FROM t) = 0"
   " LIMIT " (number->string number-of-rows)))

;;; Return a SQL statement that fetches an aggregate value
(define (get-extremum-sql
	 output-set curve-name powerrouter-id from-date to-date)
  (string-append
   "SELECT " (columnname output-set curve-name) " AS value "
   "FROM " (let ((tables (tables output-set)))
	     (if (> (length tables) 1)
		 (string-append (string-join tables " JOIN ")
				" USING (" +record-id-column+ ") ")
		 (car tables)))
   "WHERE ("
   (date-column output-set)
   " BETWEEN '" from-date "' AND '" to-date "')"
   " AND powerrouter_id = '" powerrouter-id "'"
   (let ((sql-where (sql-where output-set curve-name)))
     (if sql-where
	 (string-append " AND " sql-where)
	 ""))))

;;; Return a SQL statement that fetches the estimated dcac efficiency
;;; during a time interval
(define (get-dcac-efficiency-sql
	 output-set curve-name powerrouter-id from-date to-date)
  (string-append
   "WITH "
   "int (date0, date1) AS"
   " (VALUES ('" from-date "', '" to-date "')), "
   "batt (date, p_batt, " +record-id-column+ ", powerrouter_id) AS"
   " (SELECT " (date-column output-set) ", module_statuses.param_2, "
   +record-id-column+ ", header.powerrouter_id"
   "  FROM logs.header JOIN logs.module_statuses USING (" +record-id-column+ ")"
   "  WHERE module_statuses.module_id = 136), "
   "dcac (p_dcac, p_dcac_local, " +record-id-column+ ") AS"
   " (SELECT module_statuses.param_2, module_statuses.param_6, "
   +record-id-column+
   "  FROM logs.header JOIN logs.module_statuses USING (" +record-id-column+ ")"
   "  WHERE module_statuses.module_id = 9), "
   "solar (p_solar, " +record-id-column+ ") AS"
   " (SELECT module_statuses.param_10, " +record-id-column+ ""
   "  FROM logs.header JOIN logs.module_statuses USING (" +record-id-column+ ")"
   "  WHERE module_statuses.module_id = 12) "
   "SELECT " (columnname output-set curve-name) " AS value "
   "FROM int,  batt JOIN dcac USING (" +record-id-column+ ") JOIN solar"
   "   USING (" +record-id-column+ ") "
   " WHERE (date BETWEEN date0 AND date1)"
   " AND powerrouter_id = '" powerrouter-id "'"
   (let ((sql-where (sql-where output-set curve-name)))
     (if sql-where
	 (string-append " AND " sql-where)
	 ""))
   " GROUP BY date0, date1"))

;;; Return a SQL statement that fetches the estimated battery efficiency
;;; during a time interval
(define (get-battery-efficiency-sql
	 output-set curve-name powerrouter-id from-date to-date)
  (string-append
   "WITH "
   "batt (date, w_batt_in, w_batt_out, soc, powerrouter_id) AS"
   " (SELECT " (date-column output-set) ", module_statuses.param_4,"
   "   module_statuses.param_3, module_statuses.param_5, header.powerrouter_id"
   "  FROM logs.header JOIN logs.module_statuses USING (" +record-id-column+ ")"
   "  WHERE module_statuses.module_id = 136), "
   "b_soc (date, w_batt_in, w_batt_out, soc, powerrouter_id) AS"
   " (SELECT date, w_batt_in, w_batt_out, soc, powerrouter_id"
   "  FROM batt"
   " WHERE (date BETWEEN '" from-date "' AND '" to-date "')"
   " AND powerrouter_id = '" powerrouter-id "'"
   (let ((sql-where (sql-where output-set curve-name)))
     (if sql-where
	 (string-append " AND " sql-where)
	 ""))
   " ), "
   "b_0 (w_batt_in_0, w_batt_out_0, soc_0) AS"
   " (SELECT w_batt_in, w_batt_out, soc"
   "  FROM b_soc"
   "  ORDER BY date"
   "  LIMIT 1), "
   "b_1 (w_batt_in_1, w_batt_out_1, soc_1) AS"
   " (SELECT w_batt_in, w_batt_out, soc"
   "  FROM b_soc"
   "  ORDER BY date DESC"
   "  LIMIT 1) "
   "SELECT ((ROUND(((w_batt_out_1::NUMERIC - w_batt_out_0::NUMERIC) /"
   "              (w_batt_in_1::NUMERIC - w_batt_in_0::NUMERIC)), 3))::DOUBLE PRECISION) AS value "
   "FROM b_0, b_1"))

;;; Return a SQL statement that fetches the energy usage
;;; over a time interval
(define (get-balance-sql
	 output-set curve-name powerrouter-id from-date to-date)
  (string-append
   "WITH "
   "dcac (date, w_dcac, " +record-id-column+ ", powerrouter_id) AS"
   " (SELECT " (date-column output-set) ", module_statuses.param_3, "
   +record-id-column+ ", header.powerrouter_id"
   "  FROM logs.header JOIN logs.module_statuses USING (" +record-id-column+ ")"
   "  WHERE module_statuses.module_id = 9), "
   "platform (w_platform, " +record-id-column+ ") AS"
   " (SELECT module_statuses.param_4, " +record-id-column+ ""
   "  FROM logs.header JOIN logs.module_statuses USING (" +record-id-column+ ")"
   "  WHERE module_statuses.module_id = 16), "
   "w (date, w_dcac, w_platform, " +record-id-column+ ") AS"
   " (SELECT date, w_dcac, w_platform, " +record-id-column+ ""
   "  FROM dcac JOIN platform USING (" +record-id-column+ ")"
   " WHERE (date BETWEEN '" from-date "' AND '" to-date "')"
   " AND powerrouter_id = '" powerrouter-id "'), "
   "w_0 (w_dcac_0, w_platform_0) AS"
   " (SELECT w_dcac, w_platform"
   "  FROM w"
   "  ORDER BY date"
   "  LIMIT 1), "
   "w_1 (w_dcac_1, w_platform_1) AS"
   " (SELECT w_dcac, w_platform"
   "  FROM w"
   "  ORDER BY date DESC"
   "  LIMIT 1) "
   "SELECT " (columnname output-set curve-name) " AS value "
   "FROM w_0, w_1"))

;;; Return data for one curve the way Gnuplot understands it
(define (get-curve-points output-set curve-name powerrouter-id from-date to-date)
  (let ((sql (get-sql-row-sql output-set curve-name
			      powerrouter-id
			      from-date to-date
			      +diagram-number-of-values+)))
    (logged-query "db" sql)
    (do ((row (dbi-get_row *db*)
	      (dbi-get_row *db*))
	 (result ""))
	((not row)
	 (string-append result "e\n"))
      (set! result
	    (string-append
	     result
	     (with-output-to-string
	       (lambda ()
		 (display (cdr (first row)))
		 (display " ")
		 (display (cdr (second row)))))
	     "\n")))))

;;; date-column?=#t means return an sxml table column made of date/time
(define (get-sxml-table-column
	 output-set curve-name powerrouter-id from-date to-date date-column?)
  (let ((sql (get-sql-row-sql output-set
			      curve-name
			      powerrouter-id
			      from-date to-date
			      +table-number-of-columns+)))
    (logged-query "db" sql)
    (do ((sql-row (dbi-get_row *db*)
		  (dbi-get_row *db*))
	 (result `( ,(if date-column?
			 `(th ,(table-title output-set))
			 `(td ,curve-name)))))
	((not sql-row)
	 result)
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

(define (get-current-value-sql output-set curve-name powerrouter-id)
  (string-append
   "WITH t (date, value) AS"
   " (SELECT " (date-column output-set)
   ", " (columnname output-set curve-name)
   " FROM "
   (let ((tables (tables output-set)))
     (if (> (length tables) 1)
	 (string-append (string-join tables " JOIN ")
			" USING (" +record-id-column+ ")")
	 (car tables)))
   " WHERE header.powerrouter_id = '" powerrouter-id "'"
   (let ((sql-where (sql-where output-set curve-name)))
     (if sql-where
	 (string-append " AND " sql-where)
	 ""))
   " ORDER BY " (date-column output-set) " DESC LIMIT 1"
   ")"
   " SELECT date, value FROM t"))

;;; A single sxml table row comprising curve-name, newest value
(define (get-sxml-current-value-row output-set curve-name powerrouter-id)
  (logged-query
   "db" (get-current-value-sql output-set curve-name powerrouter-id))
  (let ((row (dbi-get_row *db*)))
    `(tr (td ,curve-name)
	 (td ,(if row
		  (cdr (assoc "value" row))
		  "0")))))

;;; A single sxml table cell containing an aggregate value
(define (get-sxml-aggregate-value-cell
	 output-set curve-name powerrouter-id date0 date1)
  (let* ((render-mode (render-mode output-set))
	 (sql-function (cond ((eq? render-mode 'as-dcac-efficiency)
			      get-dcac-efficiency-sql)
			     ((eq? render-mode 'as-extremum)
			      get-extremum-sql)
			     ((eq? render-mode 'as-battery-efficiency)
			      get-battery-efficiency-sql)
			     ((eq? render-mode 'as-balance)
			      get-balance-sql))))
    (logged-query
     "db" (sql-function output-set curve-name powerrouter-id date0 date1))
    (let ((row (dbi-get_row *db*)))
      `(td ,(if row
		(cdr (assoc "value" row))
		"-")))))

;;; header-row?=#t means return an sxml table row of column headers
(define (get-sxml-aggregate-value-row
	 powerrouter-id from-date to-date header-row?)
  `(tr
    ,@(if header-row?
	  '((th "From") (th "To"))
	  `((td ,(humanize-date-string from-date))
	    (td ,(humanize-date-string to-date))))
    ,@(map
       (lambda (x)
	 (if header-row?
	     `(td ,(cdr x))
	     (get-sxml-aggregate-value-cell
	      (car x) (cdr x) powerrouter-id from-date to-date)))
       (append-map (lambda (output-set)
		     (map (lambda (curve-name) (cons output-set curve-name))
			  (curve-names output-set)))
		   (filter (lambda (output-set) (memq (render-mode output-set)
						      '(as-dcac-efficiency
							as-battery-efficiency
							as-extremum
							as-balance)))
			   (output-sets))))))

;;; Table of output-set with one line per date interval
(define (get-sxml-aggregate-value-table powerrouter-id from-dates to-dates)
  `(table
    (tr (th (@ (colspan "2")))
	(th (@ (colspan "8")) "Energy Balance & Efficiency"))
    ,(get-sxml-aggregate-value-row #f #f #f #t)
    ,@(map
       (lambda (from-date to-date)
	 (get-sxml-aggregate-value-row powerrouter-id from-date to-date #f))
       from-dates to-dates)))

;;; Date of newest record for powerrouter-id
(define (current-value-date powerrouter-id)
  (do ((i 0 (1+ i))			;skip missing PR modules
       (row #f))
      (row
       (humanize-date-string (cdr (assoc "date" row))))
    (let* ((output-set (list-ref (output-sets) i))
	   (curve-name (first (curve-names output-set))))
      (logged-query
       "db" (get-current-value-sql output-set curve-name powerrouter-id))
      (set! row (dbi-get_row *db*)))))

(define (gnuplot-commands output-set powerrouter-id from-date to-date)
  (let* ((curve-names (curve-names output-set)))
    (string-append
     "set terminal 'svg' enhanced mouse jsdir '/view/lib/'"
     " size 1200, 800 dynamic fsize 8\n"
     "set encoding utf8\n"
     "set output\n"
     "set key left\n"
     "set keytitle '{/=12 " output-set "}'\n" ;just a default title
     "set timefmt '%Y-%m-%dT%H:%M:%S'\n"
     "set format x \"%a\\n%Y-%m-%d\\n%H:%M:%S\"\n"
     "set xdata time\n"
     "set style fill transparent solid 0.1\n"
     "set mytics 4\n"
     "set grid xtics ytics mxtics mytics lw 1, lw 0.3\n"
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
	     (get-curve-points
	      output-set curve-name powerrouter-id from-date to-date))
	   curve-names)
      ""))))

;;; Table of output-set with rows of latest values
(define (get-latest-value-sxml-table output-set powerrouter-id)
  `(table
    (th (@ (colspan "2")) ,(table-title output-set))
    ,@(map
       (lambda (curve-name)
	 (get-sxml-current-value-row output-set curve-name powerrouter-id))
       (curve-names output-set))))

;;; List of pairs of strings (name value)
(define (get-raw-value-pairs output-set powerrouter-id)
  (map
   (lambda (curve-name)
     (logged-query
      "db" (get-current-value-sql output-set curve-name powerrouter-id))
     (let ((row (dbi-get_row *db*)))
       (cons
	curve-name
	(with-output-to-string
	  (lambda () (display
		      (if row
			  (cdr (assoc "value" row))
			  "0")))))))
   (curve-names output-set)))

;;; String comprising lines of "name value"
(define (get-raw-values-text output-set powerrouter-id)
  (map-to-string
   (lambda (n raw-value-pair)
     (string-append (car raw-value-pair) " " (cdr raw-value-pair) "\n"))
   (get-raw-value-pairs output-set powerrouter-id)))

;;; Table of output-set with one line per date
(define (get-sxml-table output-set powerrouter-id from-date to-date)
  (let* ((column-names (curve-names output-set))
	 (date-column
	  (do ((i 0 (1+ i))   ;find a date column that actually exists
	       (column '(())))
	      ((not (null? (cdr column)))
	       `(,column))
	    (set! column
		  (get-sxml-table-column
		   output-set (list-ref column-names i) powerrouter-id
		   from-date to-date #t))))
	 (value-columns
	  (filter
	   (lambda (x) (not (null? (cdr x)))) ;get rid of non-existent columns
	   (map (lambda (column-name)
		  (get-sxml-table-column
		   output-set column-name powerrouter-id from-date to-date #f))
		column-names)))
	 (all-columns (append date-column value-columns)))
    `(table ,(apply map (lambda (. table-cells) `(tr ,table-cells))
		    all-columns))))

(define (plot-svg output-set powerrouter-id from-date to-date)
  (let* ((gp (run-with-pipe "r+" "gnuplot"))
	 (gp-pid (car gp))
	 (gp-in (cadr gp))
	 (gp-out (cddr gp))
	 (svg #f))
    (when +verbose+
      (file-log "gnuplot" (gnuplot-commands
			   output-set powerrouter-id from-date to-date)))
    (display (gnuplot-commands output-set powerrouter-id from-date to-date)
	     gp-out)
    (close gp-out)
    (set! svg (read-delimited "" gp-in))
    (close gp-in)
    (waitpid gp-pid)
    (if (eof-object? svg)
	#f
	svg)))

(define (tabulate output-set powerrouter-id from-date to-date)
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
	   (@ (style "text-align:center;" "margin:auto auto 50px auto;"))
	   ,(if (equal? output-set "AGGREGATES")
		(begin
		  (let ((intervals (previous-date-intervals
				    0
				    (cons (list from-date) (list to-date)))))
		    (get-sxml-aggregate-value-table
		     powerrouter-id (car intervals) (cdr intervals))))
		(get-sxml-table output-set powerrouter-id from-date to-date)))))))))

;;; Create an index named <schema>.<table>_<column>_index_where_<where>
;;; if necessary
(define (create-index schema table column where)
  (let* ((index-name
	  (string-append table "_" (underscorify column)
			 (if where
			     (string-append "_where_" (underscorify where))
			     "")
			 "_index"))
	 (create-index-sql
	  (string-append
	   "CREATE INDEX " index-name " ON " schema "." table " (" column ")"
	   (if where
	       (string-append " WHERE " where)
	       ";"))))
    (logged-query "db"
		  (string-append
		   "SELECT i.relname "
		   "FROM pg_index AS idx"
		   " JOIN pg_class AS i ON i.oid = idx.indexrelid"
		   " JOIN pg_am AS am ON i.relam = am.oid"
		   " JOIN pg_namespace AS ns ON i.relnamespace = ns.oid "
		   "WHERE nspname = '" schema "'"
		   " AND relname = '" index-name "';"))
    (unless (dbi-get_row *db*)
      (logged-query "db" create-index-sql))))

;;; Return text, one piece per element of stuff, created in fun, a function
;;; of the element number (as a string), and of the list element
(define (map-to-string fun stuff . delimiter)
  (let ((delimiter (if (null? delimiter) "" (car delimiter))))
    (let ((line-number 0))
      (string-join
       (map (lambda (element)
	      (set! line-number (1+ line-number))
	      (let ((line-number-string (number->string line-number)))
		(apply fun (list line-number-string element))))
	    stuff)
       delimiter))))

;;; Build an attr line usable in FHEM configuration
(define (fhem-attr powerrouter-id attr . values)
  (string-append "attr P_ROUT_" powerrouter-id " " attr " "
		 (string-join values "") "\n"))

;;; Return FHEM configuration
(define (fhem-cfg)
  (let ((powerrouter-count 0))
    (string-append
     "\n"
     "### BEGIN OF AUTO-GENERATED P-ROUT CONFIGURATION #############\n"
     "### The generating command was:\n"
     "### " (string-join (command-line)) "\n"
     (string-join
      (map
       (lambda (powerrouter-id)
	 (set! powerrouter-count (1+ powerrouter-count))
	 (string-append
	  "\n"
	  "define P_ROUT_" powerrouter-id
	  " HTTPMOD http://" +addr+ ":" (number->string +port+)
	  "/view/raw/" powerrouter-id
	  " 60\n"
	  (fhem-attr
	   powerrouter-id "alias" "p-rout-" (number->string powerrouter-count))
	  (fhem-attr powerrouter-id "group" "powerrouters")
	  (fhem-attr powerrouter-id "comment" "p-rout v" *version*)
	  (map-to-string
	   (lambda (line-number name)
	     (fhem-attr powerrouter-id
			(string-append "readingsName" line-number)
			name))
	   (curve-names "current-raw"))
	  (map-to-string
	   (lambda (line-number name)
	     (fhem-attr powerrouter-id
			(string-append "readingsRegex" line-number)
			name " (.*)"))
	   (curve-names "current-raw"))
	  (fhem-attr powerrouter-id
		     "stateFormat"
		     "{sprintf(\""
		     (map-to-string
		      (lambda (line-number name) (string-append name "=%s"))
		      (curve-names "current-raw")
		      ", ")
		     "\", "
		     (map-to-string
		      (lambda (line-number name)
			(string-append "ReadingsVal($name, \"" name "\", 0)"))
		      (curve-names "current-raw")
		      ", ")
		     ")}")))
       (powerrouters))
      "")
     "\n"
     "### END OF AUTO-GENERATED P-ROUT CONFIGURATION ##############\n")))

;;; Open PostgreSQL database; do some preparation work
(define (prepare-db)
  (set! *db* (dbi-open "postgresql" +db-connection+))
  (logged-query "db" "DROP CAST IF EXISTS (text AS double precision)")
  (logged-query
   "db" "CREATE CAST (text AS double precision) WITH INOUT AS IMPLICIT")
  (for-each (lambda (names) (apply create-index names))
	    +db-indexes+))

(when (option-ref options 'fhem-cfg #f)
  (dynamic-wind
    (lambda ()
      (prepare-db))
    (lambda ()
      (display (fhem-cfg)))
    (lambda ()
      (dbi-close *db*)
      (exit))))

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
  (lambda ()
    (prepare-db))
  (lambda ()
    (run-server p-rout-view
		'http
		`(#:port ,+port+ #:addr ,(inet-pton AF_INET +addr+))))
  (lambda () (dbi-close *db*)))

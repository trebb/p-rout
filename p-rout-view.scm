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
    (pid-file (value #t))))

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
  --lib-dir         p-rout-view's javascript directory (default:
                      /usr/share/p-rout/)
  --no-daemon       Remain in foreground
  --pid-file        Store daemon's PID here (default:
                      /var/run/p-rout/p-rout-view.pid)
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

(define +db-indexes+			;((schema table column) ...)
  '(("logs" "header" "p_rout_id")
    ("logs" "header" "time_send")
    ("logs" "module_statuses" "p_rout_id")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; +output-sets+: presentation style definitions
;;;
;;; An output-set is either a diagram or a table defined like this:
;;;
;;; '((<first-output-set-name>
;;;    (<SQL-schema> (<SQL-table-a> <SQL-table-b> ...) <SQL-time-column>
;;;     <indexpage-title>
;;;     <gnuplot-diagram-title-or-table-title> <as-table-or-as-diagram>)
;;;    ((<gnuplot-curve1-title>
;;;      <SQL-value1-column> <SQL-WHERE> <gnuplot-curve1-style>)
;;;     (<gnuplot-curve2-title>
;;;      <SQL-value2-column> <SQL-WHERE> <gnuplot-curve2-style>)
;;;     ...))
;;;   (<second-output-set-name>
;;;    (<SQL-schema> (<SQL-table-a> <SQL-table-b> ...) <SQL-time-column>
;;;     <indexpage-title>
;;;     <gnuplot-diagram-title-or-table-title> <as-table-or-as-diagram>)
;;;    ((<gnuplot-curve1-title>
;;;      <SQL-value1-column> <SQL-WHERE> <gnuplot-curve1-style>)
;;;     (<gnuplot-curve2-title>
;;;      <SQL-value2-column> <SQL-WHERE> <gnuplot-curve2-style>)
;;;     ...))
;;;   ...)
(define +output-sets+
  '(;; Diagrams
    ("overview-diagram"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Overview"
      "set keytitle '{/=12 Overview}'\n" as-diagram)
     (("P_{solar}/kW"
       "module_statuses.param_10 / 1000" "module_statuses.module_id = 12" "lines lw 2 lc rgb 'red' smooth bezier")
      ("P_{grid_{dcac}}/kW"
       "module_statuses.param_2 / 1000" "module_statuses.module_id = 9" "lines smooth bezier")
      ("P_{grid_{platform}}/kW"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 16" "lines smooth bezier")
      ("SOC/100%" 
       "module_statuses.param_5 / 100" "module_statuses.module_id = 136" "lines lw 2 lc rgb 'green'")))
    ("battery-diagram"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Battery"
      "set keytitle '{/=12 Battery}'\n" as-diagram)
     (;; ("I_{charge}"
      ;;  "module_statuses.param_10 / 100" "module_statuses.module_id = 136" "lines smooth bezier")
      ("V_{charge}"
       "module_statuses.param_9 / 100" "module_statuses.module_id = 136" "lines")
      ("I_{batt}"
       "module_statuses.param_1 / 100" "module_statuses.module_id = 136" "lines lc rgb 'green'")
      ;; ("I_{discharge}" 
      ;;  "module_statuses.param_12 / 100" "module_statuses.module_id = 136" "lines smooth bezier")
      ("V_{discharge}" 
       "module_statuses.param_11 / 100" "module_statuses.module_id = 136" "lines")
      ("V_{batt}" 
       "module_statuses.param_0 / 100" "module_statuses.module_id = 136" "lines")
      ("T_{batt}" 
       "module_statuses.param_7 / 10" "module_statuses.module_id = 136" "lines")
      ("SOC" 
       "module_statuses.param_5" "module_statuses.module_id = 136" "lines lw 2 lc rgb 'green'")))
    ("solar-diagram"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Solar"
      "set keytitle '{/=12 Solar}'\n" as-diagram)
     (("P/kW"
       "module_statuses.param_10 / 1000" "module_statuses.module_id = 12" "lines lw 2 lc rgb 'red' smooth bezier")
      ("P1/kW"
       "module_statuses.param_2 / 1000" "module_statuses.module_id = 12" "lines smooth bezier")
      ("P2/kW"
       "module_statuses.param_7 / 1000" "module_statuses.module_id = 12" "lines smooth bezier")
      ("V1/100V"
       "module_statuses.param_0 / 10000" "module_statuses.module_id = 12" "lines smooth bezier")
      ("V2/100V"
       "module_statuses.param_5 / 10000" "module_statuses.module_id = 12" "lines smooth bezier")
      ("I1"
       "module_statuses.param_1 / 100" "module_statuses.module_id = 12" "lines smooth bezier")
      ("I2"
       "module_statuses.param_6 / 100" "module_statuses.module_id = 12" "lines smooth bezier")))
    ("power-diagram"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Power"
      "set keytitle '{/=12 Power}'\n" as-diagram)
     (("P_{batt}"
       "module_statuses.param_2" "module_statuses.module_id = 136" "lines lc rgb 'green' smooth bezier")
      ("P_{local}"
       "module_statuses.param_6" "module_statuses.module_id = 9" "lines smooth bezier")
      ("P_{grid_{dcac}}"
       "module_statuses.param_2" "module_statuses.module_id = 9" "lines smooth bezier")
      ("P_{grid_{platform}}"
       "module_statuses.param_3" "module_statuses.module_id = 16" "lines smooth bezier")
      ("P_{solar}"
       "module_statuses.param_10" "module_statuses.module_id = 12" "lines lw 2 lc rgb 'red' smooth bezier")
      ("P1_{solar}"
       "module_statuses.param_2" "module_statuses.module_id = 12" "lines smooth bezier")
      ("P2_{solar}"
       "module_statuses.param_7" "module_statuses.module_id = 12" "lines smooth bezier")
      ("P_{L1}"
       "module_statuses.param_2" "module_statuses.module_id = 11" "lines lw 2 lc rgb 'orange' smooth bezier")
      ("P_{L2}"
       "module_statuses.param_6" "module_statuses.module_id = 11" "lines lw 2 lc rgb 'dark-red' smooth bezier")
      ("P_{L3}"
       "module_statuses.param_10" "module_statuses.module_id = 11" "lines lw 2 lc rgb 'violet' smooth bezier")))
    ("voltage-diagram"
     ("logs" ("header" "module_statuses") "header.time_send"
      "AC Voltage"
      "set keytitle '{/=12 AC Voltage}'\nset yrange [220:]\n" as-diagram)
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
    ("frequency-diagram"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Frequency"
      "set keytitle '{/=12 Frequency}'\n" as-diagram)
     (("f_{dcac}"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 9" "lines smooth bezier")
      ("f_{platform}"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 16" "lines smooth bezier")))
    ("temperature-diagram"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Temperature"
      "set keytitle '{/=12 Temperature}'\n" as-diagram)
     (("T_{dcac}"
       "module_statuses.param_10 / 10" "module_statuses.module_id = 9" "lines lc rgb 'blue' smooth bezier")
      ("T_{platform}"
       "module_statuses.param_2 / 10" "module_statuses.module_id = 16" "lines lc rgb 'dark-green' smooth bezier")
      ("T_{batt}"
       "module_statuses.param_7 / 10" "module_statuses.module_id = 136" "lines lw 2 lc rgb 'green' smooth bezier")
      ("T_{batt_{module}}"
       "module_statuses.param_8 / 10" "module_statuses.module_id = 136" "lines lc rgb 'green' smooth bezier")
      ("T1_{solar}"
       "module_statuses.param_4 / 10" "module_statuses.module_id = 12" "lines lc rgb 'red' smooth bezier")
      ("T2_{solar}"
       "module_statuses.param_9 / 10" "module_statuses.module_id = 12" "lines lc rgb 'dark-red' smooth bezier")))
    ;; Tables
    ("battery-table"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Battery"
      "Battery" as-table)
     ( ("I_charge"
       "module_statuses.param_10 / 100" "module_statuses.module_id = 136")
      ("V_charge"
       "module_statuses.param_9 / 100" "module_statuses.module_id = 136")
      ("I_batt"
       "module_statuses.param_1 / 100" "module_statuses.module_id = 136")
      ("I_discharge" 
       "module_statuses.param_12 / 100" "module_statuses.module_id = 136")
      ("V_discharge" 
       "module_statuses.param_11 / 100" "module_statuses.module_id = 136")
      ("V_batt" 
       "module_statuses.param_0 / 100" "module_statuses.module_id = 136")
      ("T_batt" 
       "module_statuses.param_7 / 10" "module_statuses.module_id = 136")
      ("SOC" 
       "module_statuses.param_5" "module_statuses.module_id = 136")))
    ("solar-table"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Solar"
      "Solar" as-table)
     (("P"
       "module_statuses.param_10" "module_statuses.module_id = 12")
      ("P1"
       "module_statuses.param_2" "module_statuses.module_id = 12")
      ("P2"
       "module_statuses.param_7" "module_statuses.module_id = 12")
      ("V1"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 12")
      ("V2"
       "module_statuses.param_5 / 100" "module_statuses.module_id = 12")
      ("I1"
       "module_statuses.param_1 / 100" "module_statuses.module_id = 12")
      ("I2"
       "module_statuses.param_6 / 100" "module_statuses.module_id = 12")))
    ("power-table"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Power"
      "Power" as-table)
     (("P_batt"
       "module_statuses.param_2" "module_statuses.module_id = 136")
      ("P_local"
       "module_statuses.param_6" "module_statuses.module_id = 9")
      ("P_grid_dcac"
       "module_statuses.param_2" "module_statuses.module_id = 9")
      ("P_grid_platform"
       "module_statuses.param_3" "module_statuses.module_id = 16")
      ("P1_solar"
       "module_statuses.param_2" "module_statuses.module_id = 12")
      ("P2_solar"
       "module_statuses.param_7" "module_statuses.module_id = 12")
      ("P_L1"
       "module_statuses.param_2" "module_statuses.module_id = 11")
      ("P_L2"
       "module_statuses.param_6" "module_statuses.module_id = 11")
      ("P_L3"
       "module_statuses.param_10" "module_statuses.module_id = 11")))
    ("voltage-table"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Voltage"
      "Voltage" as-table)
     (("V_batt" 
       "module_statuses.param_0 / 100" "module_statuses.module_id = 136")
      ("V_local"
       "module_statuses.param_5 / 10" "module_statuses.module_id = 9")
      ("V_grid_dcac"
       "module_statuses.param_1 / 10" "module_statuses.module_id = 9")
      ("V_grid_platform"
       "module_statuses.param_1 / 10" "module_statuses.module_id = 16")
      ("V1_solar"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 12")
      ("V2_solar"
       "module_statuses.param_5 / 100" "module_statuses.module_id = 12")
      ("V_L1"
       "module_statuses.param_0 / 10" "module_statuses.module_id = 11")
      ("V_L2"
       "module_statuses.param_4 / 10" "module_statuses.module_id = 11")
      ("V_L3"
       "module_statuses.param_8 / 10" "module_statuses.module_id = 11")))
    ("frequency-table"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Frequency"
      "Frequency" as-table)
     (("f_dcac"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 9")
      ("f_platform"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 16")))
    ("temperature-table"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Temperature"
      "Temperature" as-table)
     (("T_dcac"
       "module_statuses.param_10 / 10" "module_statuses.module_id = 9")
      ("T_platform"
       "module_statuses.param_2 / 10" "module_statuses.module_id = 16")
      ("T_batt"
       "module_statuses.param_7 / 10" "module_statuses.module_id = 136")
      ("T_batt_module"
       "module_statuses.param_8 / 10" "module_statuses.module_id = 136")
      ("T1_solar"
       "module_statuses.param_4 / 10" "module_statuses.module_id = 12")
      ("T2_solar"
       "module_statuses.param_9 / 10" "module_statuses.module_id = 12")))
    ("energy-table"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Energy"
      "Energy" as-table)
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
      ("Solar1 produced"
       "module_statuses.param_3 / 1000" "module_statuses.module_id = 12")
      ("Solar2 produced"
       "module_statuses.param_8 / 1000" "module_statuses.module_id = 12")
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
     (("Platform"
       "module_statuses.status" "module_statuses.module_id = 16")
      ("DC/AC"
       "module_statuses.status" "module_statuses.module_id = 9")
      ("Battery"
       "module_statuses.status" "module_statuses.module_id = 136")
      ("Solar"
       "module_statuses.status" "module_statuses.module_id = 12")
      ("Grid sensor"
       "module_statuses.status" "module_statuses.module_id = 11")))
    ("events-table"
     ("events" ("header" "event") "header.time_send"
      "Events"
      "Raw event strings" as-table)
     (("Event"
       "event.data" #f)))
    ;; Front page values
    ("current-battery"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Battery"
      "Battery" as-values)
     (("I_charge"
       "module_statuses.param_10 / 100" "module_statuses.module_id = 136")
      ("V_charge"
       "module_statuses.param_9 / 100" "module_statuses.module_id = 136")
      ("I_batt"
       "module_statuses.param_1 / 100" "module_statuses.module_id = 136")
      ("I_discharge" 
       "module_statuses.param_12 / 100" "module_statuses.module_id = 136")
      ("V_discharge" 
       "module_statuses.param_11 / 100" "module_statuses.module_id = 136")
      ("V_batt" 
       "module_statuses.param_0 / 100" "module_statuses.module_id = 136")
      ("T_batt" 
       "module_statuses.param_7 / 10" "module_statuses.module_id = 136")
      ("SOC" 
       "module_statuses.param_5" "module_statuses.module_id = 136")))
    ("current-solar"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Solar"
      "Solar" as-values)
     (("P"
       "module_statuses.param_10" "module_statuses.module_id = 12")
      ("P1"
       "module_statuses.param_2" "module_statuses.module_id = 12")
      ("P2"
       "module_statuses.param_7" "module_statuses.module_id = 12")
      ("V1"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 12")
      ("V2"
       "module_statuses.param_5 / 100" "module_statuses.module_id = 12")
      ("I1"
       "module_statuses.param_1 / 100" "module_statuses.module_id = 12")
      ("I2"
       "module_statuses.param_6 / 100" "module_statuses.module_id = 12")))
    ("current-power"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Power"
      "Power" as-values)
     (("P_batt"
       "module_statuses.param_2" "module_statuses.module_id = 136")
      ("P_bus_batt"
       "module_statuses.param_2" "module_statuses.module_id = 136")
      ("P_local"
       "module_statuses.param_6" "module_statuses.module_id = 9")
      ("P_grid_dcac"
       "module_statuses.param_2" "module_statuses.module_id = 9")
      ("P_grid_platform"
       "module_statuses.param_3" "module_statuses.module_id = 16")
      ("P_solar"
       "module_statuses.param_10" "module_statuses.module_id = 12")
      ("P1_solar"
       "module_statuses.param_2" "module_statuses.module_id = 12")
      ("P2_solar"
       "module_statuses.param_7" "module_statuses.module_id = 12")
      ("P_L1"
       "module_statuses.param_2" "module_statuses.module_id = 11")
      ("P_L2"
       "module_statuses.param_6" "module_statuses.module_id = 11")
      ("P_L3"
       "module_statuses.param_10" "module_statuses.module_id = 11")))
    ("current-voltage"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Voltage"
      "Voltage" as-values)
     (("V_local"
       "module_statuses.param_5 / 10" "module_statuses.module_id = 9")
      ("V_grid_dcac"
       "module_statuses.param_1 / 10" "module_statuses.module_id = 9")
      ("V_bus_dcac"
       "module_statuses.param_8 / 100" "module_statuses.module_id = 9")
      ("V_grid_platform"
       "module_statuses.param_1 / 10" "module_statuses.module_id = 16")
      ("V_L1"
       "module_statuses.param_0 / 10" "module_statuses.module_id = 11")
      ("V_L2"
       "module_statuses.param_4 / 10" "module_statuses.module_id = 11")
      ("V_L3"
       "module_statuses.param_8 / 10" "module_statuses.module_id = 11")))
    ("current-current"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Current"
      "Current" as-values)
     (("I_charge"
       "module_statuses.param_10 / 100" "module_statuses.module_id = 136")
      ("I_discharge"
       "module_statuses.param_12 / 100" "module_statuses.module_id = 136")
      ("I_batt"
       "module_statuses.param_1 / 100" "module_statuses.module_id = 136")
      ("I1_solar"
       "module_statuses.param_1 / 100" "module_statuses.module_id = 12")
      ("I2_solar"
       "module_statuses.param_6 / 100" "module_statuses.module_id = 12")
      ("I_L1"
       "module_statuses.param_1 / 100" "module_statuses.module_id = 11")
      ("I_L2"
       "module_statuses.param_5 / 100" "module_statuses.module_id = 11")
      ("I_L3"
       "module_statuses.param_9 / 100" "module_statuses.module_id = 11")))							    
    ("current-frequency"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Frequency"
      "Frequency" as-values)
     (("f_dcac"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 9")
      ("f_platform"
       "module_statuses.param_0 / 100" "module_statuses.module_id = 16")))
    ("current-temperature"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Temperature"
      "Temperature" as-values)
     (("T_dcac"
       "module_statuses.param_10 / 10" "module_statuses.module_id = 9")
      ("T_platform"
       "module_statuses.param_2 / 10" "module_statuses.module_id = 16")
      ("T_batt"
       "module_statuses.param_7 / 10" "module_statuses.module_id = 136")
      ("T_batt_module"
       "module_statuses.param_8 / 10" "module_statuses.module_id = 136")
      ("T1_solar"
       "module_statuses.param_4 / 10" "module_statuses.module_id = 12")
      ("T2_solar"
       "module_statuses.param_9 / 10" "module_statuses.module_id = 12")))
    ("current-energy"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Energy"
      "Energy" as-values)
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
    ("current-status"
     ("logs" ("header" "module_statuses") "header.time_send"
      "Status"
      "Status" as-values)
     (("Platform"
       "module_statuses.status" "module_statuses.module_id = 16")
      ("Battery"
       "module_statuses.status" "module_statuses.module_id = 136")
      ("DC/AC"
       "module_statuses.status" "module_statuses.module_id = 9")
      ("Solar"
       "module_statuses.status" "module_statuses.module_id = 12")
      ("Grid sensor"
       "module_statuses.status" "module_statuses.module_id = 11")
      ("PR Id"
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
	 (latest-halfday-hour (- hour (modulo hour 12))))
    (map
     (lambda (date) (date->string date "~Y-~m-~dT~H:~M"))
     (list 
      (make-date 0 0 0 latest-halfday-hour (1- day) month year zone-offset)
      (make-date 0 0 0 latest-halfday-hour (1+ day) month year zone-offset)))))

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
    (h4 "Current Values")
    ,(map (lambda (output-set)
	    `(div
	      '(@ (style "float:left;" "padding:10px 2px"))
	      ,(get-latest-value-sxml-table output-set)))
	  (filter (lambda (x) (eq? 'as-values (render-mode x)))
		  (output-sets)))))

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
			  "margin:100px auto 100px auto;"))
		     (form
		      (@ (action "/view/render"))
		      (p ,(sxml-date-input +from-label+ (first default-dates))
			 " "
			 ,(sxml-date-input +to-label+ (second default-dates)))
		      (h4 "Diagrams")
		      (p ,(sxml-output-set-inputs 'as-diagram))
		      (h4 "Tables")
		      (p ,(sxml-output-set-inputs 'as-table))
		      (p (input (@ (type "submit")
				   (value "Go"))))))
		    ,(sxml-latest-value-tables-div)))))))))

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
(define (get-sql-row-sql output-set curve-name from-date to-date number-of-rows)
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
   (let ((sql-where (sql-where output-set curve-name)))
     (if sql-where
	 (string-append " AND " sql-where)
	 ""))
   ")"
   " SELECT date, value FROM t WHERE row_number % (SELECT 1 + count(*) / "
   (number->string number-of-rows)
   " FROM t) = 0"
   " LIMIT " (number->string number-of-rows)))

;;; Return data for one curve the way Gnuplot understands it
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
		 (display (cdr (first row)))
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
			 `(th ,(table-title output-set))
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

;;; A single sxml table row comprising curve-name, newest value
(define (get-sxml-current-value-row output-set curve-name)
  (let ((sql (string-append
	      "WITH t (date, value) AS"
	      " (SELECT " (date-column output-set)
	      ", " (columnname output-set curve-name)
	      " FROM "
	      (let ((tables (tables output-set)))
		(if (> (length tables) 1)
		    (string-append (string-join tables " JOIN ")
				   " USING (" +record-id-column+ ")")
		    (car tables)))
	      (let ((sql-where (sql-where output-set curve-name)))
		(if sql-where
		    (string-append " WHERE " sql-where)
		    ""))
	      " ORDER BY " (date-column output-set) " DESC LIMIT 1"
	      ")"
	      " SELECT date, value FROM t")))
    (logged-query "db" sql)
    `(tr (td ,curve-name)
	 (td ,(cdr (assoc "value"
			  (dbi-get_row *db*)))))))	

(define (gnuplot-commands output-set from-date to-date)
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
     "set grid ytics\n"
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

;;; Table of output-set with rows of latest values
(define (get-latest-value-sxml-table output-set)
  (cons* 'table
	 `(th (@ (colspan "2")) ,(table-title output-set))
	 (map
	  (lambda (curve-name)
	    (get-sxml-current-value-row output-set curve-name))
	  (curve-names output-set))))

;;; Table of output-set with one line per date
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
	   (@ (style "text-align:center;" "margin:auto auto 50px auto;"))
	   ,(get-sxml-table output-set from-date to-date))))))))

;;; Create an index named <schema>.<table>_<column>_index if necessary
(define (create-index schema table column)
  (logged-query "db" (string-append
		      "SELECT "
		      "t.relname AS table_name, "
		      "i.relname AS index_name, "
		      "a.attname AS column_name, "
		      "n.nspname AS schema "
		      "FROM "
		      "pg_namespace n, pg_class t, "
		      "pg_class i, "
		      "pg_index ix, "
		      "pg_attribute a "
		      "WHERE "
		      "n.oid = t.relnamespace "
		      "AND t.oid = ix.indrelid "
		      "AND i.oid = ix.indexrelid "
		      "AND a.attrelid = t.oid "
		      "AND a.attnum = ANY(ix.indkey) "
		      "AND t.relkind = 'r' "
		      "AND n.nspname = '" schema "' "
		      "AND t.relname = '" table "' "
		      "AND a.attname = '" column "';"))
  (unless (dbi-get_row *db*)
    (logged-query "db" (string-append
			"CREATE INDEX " table "_" column "_index "
			"ON " schema "." table " (" column ");"))))

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
    (set! *db* (dbi-open "postgresql" +db-connection+))
    (logged-query "db" "DROP CAST IF EXISTS (text AS double precision)")
    (logged-query
     "db"
     "CREATE CAST (text AS double precision) WITH INOUT AS IMPLICIT")
    (for-each (lambda (names) (apply create-index names))
	      +db-indexes+))
  (lambda ()
    (run-server p-rout-view
		'http
		`(#:port ,+port+ #:addr ,(inet-pton AF_INET +addr+))))
  (lambda () (dbi-close *db*)))

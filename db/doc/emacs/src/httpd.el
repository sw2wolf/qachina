;;; httpd.el -- a web server in Emacs Lisp
;;;
;;; Author: Eric Marsden <emarsden@laas.fr>
;;; Version: 0.5
;;; Keywords: games
;;; Copyright: (C) 2001  Eric Marsden
;;   
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;; The latest version of this package should be available from
;;
;;     <URL:http://purl.org/net/emarsden/home/downloads/>

;;; Commentary:
;;
;; httpd.el is an HTTP server embedded in the Emacs. It can handle GET
;; and HEAD requests; adding support for POST should not be too
;; difficult. Since Emacs does not (yet!) provide server sockets, you
;; need a helper program to bind to the socket and forward requests.
;; There are two ways of doing this: use a service like inetd to
;; launch a fresh emacs instance for each incoming request, or use a
;; program which forwards requests via gnuserv. The second method
;; obviously provides better performance.
;;
;; First method: To run this from a service such as inetd, using a
;; line such as the following in /etc/inetd.conf :
;; 
;; 8080 stream tcp nowait.10000 nobody /usr/bin/emacs emacs -batch \
;;    -l /path/to/httpd.el -f httpd-serve
;;
;; To use tcpserver instead, invoke as
;;
;;  /usr/bin/tcpserver 0 8080 /usr/bin/fixcrio /usr/bin/emacs -batch \
;;    -l /path/to/httpd.el -f httpd-serve
;;
;; (tcpserver is part of the ucspi-tcp package by Dan Bernstein; see
;; <URL:http://cr.yp.to/ucspi-tcp.html>). You could also use netcat
;; with an appropriate shell script emacs-httpd
;;
;;  while : ; do nc -l -p 8080 -e emacs-httpd ; done
;;
;; however for me this doesn't work, perhaps because of buffering
;; problems.
;;
;;
;; Second method (thanks to John Wiegley): use httpd-serve, a Python
;; script which forwards HTTP requests to an emacs process via
;; gnuserv. This method provides much better performance than the
;; first, since you avoid forking a new emacs process for each
;; request. Download httpd-serve from
;; <URL:http://www.gci-net.com/users/j/johnw/emacs.html>
;;
;; 
;;
;; I have only tested this code with Emacs; it may need modifications
;; to work with XEmacs.
;;
;;
;;; Acknowledgements
;;
;; httpd.el was inspired by pshttpd, an HTTP server written in
;; Postscript by Anders Karlsson <URL:http://www.pugo.org:8080/>.
;;
;; Thanks to John Wiegley and Cyprian Adam Laskowski.


;;; Code

(require 'cl)


(defvar httpd-document-root "/var/www")



(defvar httpd-path-handlers '()
  "Alist of (path-regexp . handler) forms.
If a GET request is made for an URL whose path component matches
a PATH-REGEXP, the corresponding handler is called to generate
content.")

(defvar httpd-mime-types-alist
  '(("html" . "text/html; charset=iso-8859-1")
    ("txt"  . "text/plain; charset=iso-8859-1")
    ("jpg"  . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif"  . "image/gif")
    ("png"  . "image/png")
    ("tif"  . "image/tiff")
    ("tiff" . "image/tiff")
    ("css"  . "text/css")
    ("gz"   . "application/octet-stream")
    ("ps"   . "application/postscript")
    ("pdf"  . "application/pdf")
    ("eps"  . "application/postscript")
    ("tar"  . "application/x-tar")
    ("rpm"  . "application/x-rpm")
    ("zip"  . "application/zip")
    ("mp3"  . "audio/mpeg")
    ("mp2"  . "audio/mpeg")
    ("mid"  . "audio/midi")
    ("midi" . "audio/midi")
    ("wav"  . "audio/x-wav")
    ("au"   . "audio/basic")
    ("ram"  . "audio/pn-realaudio")
    ("ra"   . "audio/x-realaudio")
    ("mpg"  . "video/mpeg")
    ("mpeg" . "video/mpeg")
    ("qt"   . "video/quicktime")
    ("mov"  . "video/quicktime")
    ("avi"  . "video/x-msvideo")))

(defun httpd-mime-type (filename)
  (or (cdr (assoc (file-name-extension filename) httpd-mime-types-alist))
      "text/plain"))

(put 'httpd-exception 'error-conditions
     '(httpd-exception error))

(defmacro defhttpd-exception (name code msg)
  `(progn
     (put ',name 'error-conditions '(,name httpd-exception error))
     (put ',name 'httpd-code ,code)
     (put ',name 'httpd-msg ,msg)))

(defhttpd-exception httpd-moved/perm       301 "Moved permanently")
(defhttpd-exception httpd-moved/temp       302 "Moved temporarily")
(defhttpd-exception httpd-bad-request      400 "Bad request")
(defhttpd-exception httpd-forbidden        403 "Forbidden")
(defhttpd-exception httpd-file-not-found   404 "Object does not Exist (Sartre, 1945)")
(defhttpd-exception httpd-method-forbidden 405 "Method not allowed")
(defhttpd-exception httpd-unimplemented    500 "Internal server error")
(defhttpd-exception httpd-unimplemented    501 "Not implemented")
(defhttpd-exception httpd-unimplemented    503 "Service unavailable")

;; could use `insert-file-contents-literally' to disable certain
;; decoding and conversion features, but we don't want to disable them
;; all (find-file-hooks and auto decompression for instance)
(defun httpd-send-file (filename)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents filename)
    (princ (buffer-string))))

(defvar httpd-terminate-emacs t)
(defvar httpd-line-terminator "\r\n")

(defun httpd-lose (code msg)
  (princ "HTTP/1.0 ")
  (princ code)
  (princ " ")
  (princ msg)
  (princ httpd-line-terminator)
  (princ "Content-Type: text/html")
  (princ httpd-line-terminator)
  (princ "Connection: close")       
  (princ httpd-line-terminator)
  (princ httpd-line-terminator)
  (princ "<html><head><title>Error</title></head>")
  (princ httpd-line-terminator)
  (princ "<body><h1>")
  (princ msg)
  (princ "</h1>")
  (princ httpd-line-terminator)
  (princ "<p>")
  (princ msg)
  (princ httpd-line-terminator)
  (princ "</body></html>")
  (princ httpd-line-terminator)
  (when httpd-terminate-emacs
    (kill-emacs 0)))

;; when running in batch mode, `princ' output goes to stdout, and
;; `message' output to stderr. inetd directs stderr to the socket
;; (whereas we would prefer it to go to a log file), so here we
;; redefine message to be silent. 
(defmacro with-httpd-io-redirections (&rest forms)
  `(let ((old-message (symbol-function 'message)))
    (fset 'message (lambda (fmt &rest args) nil))
    (unwind-protect
        (progn ,@forms)
      (fset 'message old-message))))

(defmacro with-httpd-error-handler (&rest forms)
  `(condition-case why
       (princ (with-output-to-string ,@forms))
     (httpd-exception
      (httpd-lose (get (car why) 'httpd-code)
                  (get (car why) 'httpd-msg)))
     (error (httpd-lose 500 (format "Emacs Lisp error: %s" why)))))

(defun httpd-add-handler (path-regexp handler)
  (push (cons path-regexp handler) httpd-path-handlers))

(defun httpd-try-internal-handler (path &optional cont)
  (loop for (regexp . handler) in httpd-path-handlers
        when (string-match regexp path) 
        return (funcall handler path cont)))

(defun httpd-handle-GET+HEAD (path &optional want-data)
  (when (zerop (length path))
    (setq path "index.html"))
  ;; could use `expand-file-name' instead of `concat', but don't want
  ;; tilde expansion etc.
  (let ((filename (concat httpd-document-root "/" path)))
    (cond ((httpd-try-internal-handler path) t)
          ((file-directory-p filename)
           (httpd-handle-redirect
            path
            (concat "http://" (system-name) "/" path "/")))
          ((file-readable-p filename)
           (princ "HTTP/1.0 200 OK")
	   (princ httpd-line-terminator)
           (princ "Server: Emacs/httpd.el")
	   (princ httpd-line-terminator)
           (princ "Connection: close")
	   (princ httpd-line-terminator)
           (princ "MIME-Version: 1.0")
	   (princ httpd-line-terminator)
           (princ "Content-Type: ")
           (princ (httpd-mime-type filename))
	   (princ httpd-line-terminator)
           (princ "Content-Length: ")
           (princ (nth 7 (file-attributes filename)))
	   (princ httpd-line-terminator)
	   (princ httpd-line-terminator)
           (when want-data (httpd-send-file filename)))
          (t (signal 'httpd-file-not-found path)))))

(defun httpd-handle-GET (path)
  (httpd-handle-GET+HEAD path t))

(defun httpd-handle-HEAD (path)
  (httpd-handle-GET+HEAD path nil))

(defun httpd-handle-POST (req &optional cont)
  (unless (httpd-try-internal-handler req cont)
    (signal 'httpd-unimplemented req)))

(defun httpd-handle-redirect (req where)
  "Redirect the client to new location WHERE."
  (princ "HTTP/1.0 301 Moved permanently")
  (princ httpd-line-terminator)
  (princ "Location: ")
  (princ where)
  (princ httpd-line-terminator)
  (princ "URI: ")
  (princ where)
  (princ httpd-line-terminator)
  (princ "Connection: close")
  (princ httpd-line-terminator)
  (princ httpd-line-terminator))

(defun httpd-handle-request (req &optional cont)
  ;; reject requests containing ".." in the path. Should really
  ;; URI-decode first. 
  (when (string-match "\\.\\." req)
    (signal 'httpd-forbidden req))
  (cond ((string-match "\\`GET\\s-/\\([^ \t\r\n]*\\)" req)
         (httpd-handle-GET (match-string 1 req)))
        ((string-match "\\`HEAD\\s-/\\([^ \t\r\n]*\\)" req)
         (httpd-handle-HEAD (match-string 1 req)))
        ((string-match "\\`POST\\s-/\\([^ \t\r\n]*\\)" req)
         (httpd-handle-POST (match-string 1 req) cont))
        (t (signal 'httpd-bad-request req))))

;; inetd seems to normalize end of line markers in input: CR becomes
;; CRLF.
;;
;; The arguments `request' and `content' are nil when called from
;; inetd; in this case we read the request from stdin. The variables
;; are used by the persistent execution mode (from gnuserv).
(defun httpd-serve (&optional request content)
  (with-httpd-error-handler
   (with-httpd-io-redirections
    (if request
	(httpd-handle-request request content)
      (with-temp-buffer
	(loop for ch =  (read-event nil t)
	      do (unless (= 10 ch) (insert ch))
	      until (and (= 13 (char-after (1- (point))))
			 (= 13 (char-after (- (point) 2)))))
	(httpd-handle-request (buffer-string)))))))


;; an example of adding path handlers for dynamic content
(defun httpd-calendar-demo (path &optional content)
  (require 'calendar)
  (unless (string-match "^\\([0-9]+\\)/\\([0-9]+\\)" path)
    (httpd-lose 400 (format "Date should be YYYY/MM, not %s" path)))
  (let ((year (string-to-number (match-string 1 path)))
        (month (string-to-number (match-string 2 path))))
    (princ "HTTP/1.0 200 OK")
    (princ httpd-line-terminator)
    (princ "Server: Emacs/httpd.el")
    (princ httpd-line-terminator)
    (princ "Connection: close")
    (princ httpd-line-terminator)
    (princ "MIME-Version: 1.0")
    (princ httpd-line-terminator)
    (princ "Content-Type: text/html")
    (princ httpd-line-terminator)
    (princ httpd-line-terminator)
    (princ "<html><head><title>Emacs calendar</title></head>")
    (princ httpd-line-terminator)
    (princ "<body> <h1>Calendar for ")
    (princ year)
    (princ "/")
    (princ month)
    (princ "</h1>")
    (princ httpd-line-terminator)
    (princ "<pre>")
    (princ httpd-line-terminator)
    (princ
     (with-temp-buffer
       (generate-calendar month year)
       (buffer-string)))
    (princ httpd-line-terminator)
    (princ "</pre></body></html>")
    (princ httpd-line-terminator)))

(httpd-add-handler "^[0-9]+/[0-9]+" 'httpd-calendar-demo)


(provide 'httpd)

;; httpd.el ends here

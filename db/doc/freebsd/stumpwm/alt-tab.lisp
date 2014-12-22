;n-*-lisp-*-
; vim:ft=lisp
;;; Cycling windows in Microsoft Most Recently Used Style aka Alt-Tab
;;; author ruthard baudach <ruthard.baudach@web.de>
;;; Version 0.1, 2012
;;;
;;; usage: put (load "/path/to/alt-tab.lisp") ind stumpwmrc
;;;
;;; known bugs: this script does not work as exspected, if there are split frames without window

;;; Where are we?
(in-package :stumpwm)

;; timestamp function
(defun get-milli-secs () ()
  "get timestamp in milliseconds"
  (floor (* 1000 (/ (get-internal-real-time) internal-time-units-per-second))))

;;;; ToDo: use hash of group-number => tab-lists to preserve alt-tab list during group change
;; Variables we need
;(defvar *alt-tab-hash* (make-hash-table))
;(mapcar (lambda (x) (setf (gethash x *alt-tab-hash*) (make-list 0))) (mapcar 'group-number (screen-groups (current-group))))

(defvar *alt-tab-list* (make-list 0))
(defvar *alt-tab-cycle* (make-list 0))
(defvar *alt-tab-timeout* 500)              ; in milliseconds
(defvar *alt-tab-index* 0)              ; in milliseconds
(defvar *alt-tab-last-call* (get-milli-secs))

;; call this to use mru-style window cycling
(defcommand cycle-alt-tab () ()
	"focus next windows according to Windows's most recently used policy"
    (cond 
      ;; if we're still in timeout
      ((< (- (get-milli-secs) *alt-tab-last-call*) 500)
        ;; cycle through windows
        (setf *alt-tab-index* (mod (1+ *alt-tab-index*) (list-length *alt-tab-cycle*)))
        (select-window-by-number (nth *alt-tab-index* *alt-tab-cycle*)))
      ;; else start anew
      (
        (setf *alt-tab-index* 1)
        (setf *alt-tab-cycle* (copy-list *alt-tab-list*))
	; if we've fokussed a frame without window, current-window will return nil => select last used
	(cond 
	  ((not (current-window))
	   (select-window-by-number (first *alt-tab-cycle*)))
	  (
	   (push (window-number (current-window)) *alt-tab-cycle*)
           (select-window-by-number (second *alt-tab-cycle*))))))

    ;; update timestamp
    (setf *alt-tab-last-call* (get-milli-secs)))

;;; hooks:
(defun alt-tab-new-window (win) ()
    "function called by new-window-hook. updates alt-tab-list"
  (push (window-number win)  *alt-tab-list*))

(defun alt-tab-destroy-window (win) ()
    "function called by destroy-window-hook. deletes window number from alt-tab-list"
  (setf *alt-tab-list* (delete (window-number win) *alt-tab-list*)))

(defun alt-tab-focus-window (new-win old-win) ()
    "function called by focus-window-hook. updates alt-tab-list"
  (when new-win (setf *alt-tab-list* (delete (window-number new-win) *alt-tab-list*)))
  (when old-win (push (window-number old-win) *alt-tab-list*)))

(setf *new-window-hook* (list 'alt-tab-new-window))
(setf *destroy-window-hook* (list 'alt-tab-destroy-window))
(setf *focus-window-hook* (list 'alt-tab-focus-window))

;;; group handling:
;; 
(defun alt-tab-focus-group (to-group from-group) ()
    "handling group switching"
    (setf *alt-tab-list* (mapcar 'window-number (group-windows to-group))))

(setf *focus-group-hook* (list 'alt-tab-focus-group))
;;; bind to Meta-Tab, as stumpwm knows no Alt
(define-key *top-map* (kbd "M-TAB") "cycle-alt-tab")



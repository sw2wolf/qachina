;; activate debugging
(setq debug-on-error t)
(setq debug-on-quit t)

;; change the pathnames appropriately!
;(add-to-list 'load-path (expand-file-name "~/src/org-mode/lisp"))
;(add-to-list 'load-path (expand-file-name "~/src/org-mode/contrib/lisp"))

;; (require 'org)
;; (require 'org-agenda)
;; (require 'outline)
;; (require 'font-lock)
;; (require 'flyspell)
;; (require 'ispell)
;; (require 'calendar)
;; (require 'cal-iso)
;; (require 'diary-lib)

;; profile some times... CAUTION: require the packages first (to get results)!
(when (require 'elp)
  ;; (elp-instrument-package "org")
  ;; (elp-instrument-package "org-agenda")
  ;; (elp-instrument-package "outline")
  ;; (elp-instrument-package "font-lock")
  ;; (elp-instrument-package "flyspell")
  ;; (elp-instrument-package "ispell")
  ;; (elp-instrument-package "emacs-leuven")
  ;; (elp-instrument-package "calendar")
  ;; (elp-instrument-package "cal-iso")
  ;; (elp-instrument-package "diary-lib")
  (elp-instrument-package "mew-")
  (global-set-key "\C-cp" 'elp-results))

;; improve readability of profile results, give milliseconds
(defun elp-pack-number (number width)
  (format (concat "%" (number-to-string (- width 3)) ".2f")
          (* 100 (string-to-number number))))

;; (setq org-agenda-files
;;       (append
;;        ;; org-directory
;;        (file-expand-wildcards "~/Personal/*.org")
;;        (file-expand-wildcards "~/Projects/*.org")))

;; (defconst em/emacs-load-time-start (float-time))

;; (org-agenda-list)

;; (message "Loading Minimal Emacs... Done (in %.2f s)"
;;          (- (float-time) em/emacs-load-time-start))

;; You can also use the Emacs Lisp Profiler. This is rather under-documented: you'll have to read the comments in elp.el for the details, but basically you run elp-instrument-package to turn on profiling for all the functions with a given prefix, and then elp-results to see the results.

;; Here's some typical output after typing M-x elp-instrument-package RET c- RET, fontifying 4,000 lines of C, and then running elp-results (and using elp-sort-by-function to sort by call count):

;; Function Name                  Call Count  Elapsed Time  Average Time
;; =============================  ==========  ============  ============
;; c-skip-comments-and-strings    107         0.0           0.0
;; c-valid-offset                 78          0.0           0.0
;; c-set-offset                   68          0.031         0.0004558823
;; c-end-of-macro                 52          0.0           0.0
;; c-neutralize-CPP-line          52          0.0           0.0
;; c-font-lock-invalid-string     20          0.0           0.0
;; c-set-style-1                  19          0.031         0.0016315789
;; ...
;; In your particular case the profiler doesn't help immediately, because you don't know which package is at fault. But if you can make a guess (or use debug-on-quit to find it for sure) then the profiler can help you diagnose the problem in detail.

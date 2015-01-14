(setq sql-sqlite-program "sqlite3")

(add-hook 'sql-interactive-mode-hook
    (function (lambda ()
        (setq comint-output-filter-functions 'comint-truncate-buffer))))

(setq comint-output-filter-functions
       (function (lambda (STR) (comint-show-output))))


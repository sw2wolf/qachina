(add-to-list 'load-path "/usr/local/lib/erlang/lib/tools-2.6.8/emacs/")

(setq erlang-root-dir "/usr/local/otp")
(setq exec-path (cons "/usr/local/otp/bin" exec-path))

(require 'erlang-start)

(in-package #:qachina)

(defvar *this-file* (load-time-value (or #.*compile-file-pathname* *load-pathname*)))

(setq *dispatch-table*
    (nconc
        (list 'dispatch-easy-handlers
            (create-static-file-dispatcher-and-handler
                "/image/GameVoter.png"
                (make-pathname :name "GameVoter" :type "png" :version nil :defaults *this-file*)
                "image/png")
            (create-static-file-dispatcher-and-handler
                "/qa.css"
                (make-pathname :name "qa" :type "css" :version nil :defaults *this-file*))
            (create-folder-dispatcher-and-handler
                "/doc/"
                (make-pathname :name nil :type nil :version nil :defaults *this-file*)
                "text/plain"))
        (mapcar (lambda (args) (apply 'create-prefix-dispatcher args))
            '(("/index" index)
              ("/new-game" new-game)
              ("/game-added" game-added)
              ("/vote" vote)))))

(pushnew (hunchentoot-cgi:create-cgi-dispatcher-and-handler
          "/cgi-bin/"
          (make-pathname :directory '(:absolute "media" "D" "www" "qachina" "cgi-bin"))
          ) *dispatch-table* :test #'equal)

(setf chunga:*accept-bogus-eols* t)

(setf *web-server* (start  (make-instance 'hunchentoot:easy-acceptor :port 8000
    :document-root #P"/media/D/www/qachina/"
    ;:access-log-destination (strcat *log-path* "access.log")
    ;:message-log-destination (strcat *log-path* "message.log")
    )))


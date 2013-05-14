(ext:saveinitmem "stumpwm" :init-function (lambda ()
                                            (stumpwm:stumpwm)
                                            (ext:quit))
     :executable t :keep-global-handlers t :quiet t :norc nil :documentation "The StumpWM Executable")

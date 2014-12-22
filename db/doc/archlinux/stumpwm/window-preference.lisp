    (define-frame-preference "car"
        (0 nil t :restore   "car-dump" :class "Stardict"))

    (define-frame-preference "cadr"
        (0 t t :create   "cadr-dump" :class "URxvt")
        (0 t t :create   "cadr-dump" :class "Emacs")
        (0 t t :create   "cadr-dump" :class "Xpdf"))

    (define-frame-preference "pidgin"
        (2 t t :create   "pidgin-dump"  :class "Pidgin" :instance "pidgin" :title nil :role nil)
        (0 t t :create   "pidgin-dump"  :class "Pidgin" :instance "pidgin" :title nil :role "conversation")
        (1 t t :create   "pidgin-dump"  :class "Pidgin" :instance "pidgin" :title nil :role "buddy_list"))

    (define-frame-preference "gimp"
        (1 t t :create   "gimp-dump"  :class "Gimp" :title nil :role nil)
        (0 t t :create   "gimp-dump"  :class "Gimp" :title nil :role "gimp-toolbox")
        (2 t t :create   "gimp-dump"  :class "Gimp" :title nil :role "gimp-dock")
        (1 t t :create   "gimp-dump"  :class "Gimp" :title nil :role "gimp-image-window"))

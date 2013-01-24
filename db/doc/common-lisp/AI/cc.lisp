;; This page describes a chess Programm that I am writing  in Common Lisp. I
;; am trying to do this in
;; [[http://en.wikipedia.org/wiki/Literate_programming][Literate
;; Programming]]. This page is generated from the source code by a
;; trivial perl script. The output was then
;; further processed with
;; [[http://www.gnu.org/software/emacs/][emacs]]
;; [[http://orgmode.org/][org-mode]] to generate this page
;;
;;#+TITLE: A Chess Program in Common Lisp
;;#+AUTHOR: Patrick Krusenotto
;;#+EMAIL: 
;;#+STYLE:<style>
;;#+STYLE:html {font-size:13pt;width: 800px}
;;#+STYLE:pre  {font-size:13pt}
;;#+STYLE:</style>
;; ============================================================================
;; * Introduction
;; ============================================================================
;;
;; This is a lisp program to play chess. I tried to keep it as simple
;; as possible but i wanted to know it all the rules of chess.
;;
;; The chess board is made up as vector of 120 fields. The 8x8 is
;; embedded in into a matrix of 10*12 fields. This is very common in
;; chess programming. The overall advantage is that one does not have
;; to mess around with two indices. Any direction can be expressed by
;; only one delta, e.g if a pawn moves one field foreward, it
;; increases its index by +10. The pawns capturing moves are +9 and
;; +11.  Around the 64 fields of the chessboard there are two columns
;; of width 1 and two rows of width 2. On these fields there stands a
;; pseudo piece. If we try moves, then we do not have to check if the
;; index points off the board: we just check, if the targeted field is
;; free or kept by a piece of the other color. If we find a pseudo
;; piece, then the move is impossible. To handle the knight correctly,
;; we need 2 rows of pseudo pieces at the bottom and the top of the
;; array. To the left and the right 1 colum is enough, sice the knight
;; would just skip to the opposite site of the board. Have a look at
;; the definition of function "start position" to find out how all
;; this works.
;;
;; ============================================================================
;; * Code
;; ============================================================================
;; ============================================================================
;; ** Generic Stuff
;; ============================================================================
;;
;; This is some more generic stuff that is need for some
;; operations. These functions are not about chess.

(defun deep-mapcar (f l)
  "perform mapcar on all leaves of a tree. non-destructive"
  (cond ((null l) nil)
	((atom l) (funcall f l))
	(t (cons (deep-mapcar f (car l))
		 (deep-mapcar f (cdr l))))))

(defun symbol-ascii (s)
  "find ascii-code of the first char of a symbol"
  (char-code (char (symbol-name s) 0))
)

;; ============================================================================
;; ** Constants
;; ============================================================================
;;
;; We need a lot of constants to get a Domain Specific Language that
;; we need to interact with the program. We will even define constants
;; for all the 64 fields of the chess board.
;;
;; first of all we need to define contants for the colors an the
;; pieces with the property "color=(signum p)"
(defconstant white  1)
(defconstant black -1)

(defconstant none   0)
(defconstant pawn   1)
(defconstant pawn2  2)
(defconstant pawn3  3)
(defconstant knight 4)
(defconstant bishop 5)
(defconstant rook   6)
(defconstant rook2  7)
(defconstant queen  8)
(defconstant king   9)
(defconstant stop  10)

(defconstant -pawn   (- pawn))
(defconstant -pawn2  (- pawn2))
(defconstant -pawn3  (- pawn3))
(defconstant -knight (- knight))
(defconstant -bishop (- bishop))
(defconstant -rook   (- rook))
(defconstant -rook2  (- rook2))
(defconstant -queen  (- queen))
(defconstant -king   (- king))

;; Any piece has a value. We define here a the value of pawn to be
;; 100. The other values are: knight,bishop=3 pawns, rook=4.5 pawns,
;; queen=9 pawns and king = indefinite.

(defparameter *piece-value* 
  (vector 0 100 100 100 300 300 450 450 900 1000000 0))

;; The funcion start-position generates a board at game start

(defun start-position () 
  (vector
   stop    stop    stop    stop    stop    stop    stop    stop    stop    stop   
   stop    stop    stop    stop    stop    stop    stop    stop    stop    stop   
   stop    rook    knight  bishop  queen   king    bishop  knight  rook    stop
   stop    pawn    pawn    pawn    pawn    pawn    pawn    pawn    pawn    stop
   stop    none    none    none    none    none    none    none    none    stop
   stop    none    none    none    none    none    none    none    none    stop
   stop    none    none    none    none    none    none    none    none    stop
   stop    none    none    none    none    none    none    none    none    stop
   stop   -pawn   -pawn   -pawn   -pawn   -pawn   -pawn   -pawn   -pawn    stop
   stop   -rook   -knight -bishop -queen  -king   -bishop -knight -rook    stop
   stop    stop    stop    stop    stop    stop    stop    stop    stop    stop   
   stop    stop    stop    stop    stop    stop    stop    stop    stop    stop   
   ))

;; This one is to generate an empty board

(defun empty-board ()
  (vector
   stop stop stop stop stop stop stop stop stop stop   
   stop stop stop stop stop stop stop stop stop stop   
   stop none none none none none none none none stop
   stop none none none none none none none none stop
   stop none none none none none none none none stop
   stop none none none none none none none none stop
   stop none none none none none none none none stop
   stop none none none none none none none none stop
   stop none none none none none none none none stop
   stop none none none none none none none none stop
   stop stop stop stop stop stop stop stop stop stop   
   stop stop stop stop stop stop stop stop stop stop   
   )
  )

;; These are the indices of all the 64 fields. We need them to be able
;; to conveniently refer to the 64 fields of the chess board. We do
;; not use =*earmuffs*= here

(defconstant A1 21) (defconstant B1 22) (defconstant C1 23) (defconstant D1 24)
(defconstant E1 25) (defconstant F1 26) (defconstant G1 27) (defconstant H1 28)

(defconstant A2 31) (defconstant B2 32) (defconstant C2 33) (defconstant D2 34)
(defconstant E2 35) (defconstant F2 36) (defconstant G2 37) (defconstant H2 38)

(defconstant A3 41) (defconstant B3 42) (defconstant C3 43) (defconstant D3 44)
(defconstant E3 45) (defconstant F3 46) (defconstant G3 47) (defconstant H3 48)

(defconstant A4 51) (defconstant B4 52) (defconstant C4 53) (defconstant D4 54)
(defconstant E4 55) (defconstant F4 56) (defconstant G4 57) (defconstant H4 58)

(defconstant A5 61) (defconstant B5 62) (defconstant C5 63) (defconstant D5 64)
(defconstant E5 65) (defconstant F5 66) (defconstant G5 67) (defconstant H5 68)

(defconstant A6 71) (defconstant B6 72) (defconstant C6 73) (defconstant D6 74)
(defconstant E6 75) (defconstant F6 76) (defconstant G6 77) (defconstant H6 78)

(defconstant A7 81) (defconstant B7 82) (defconstant C7 83) (defconstant D7 84)
(defconstant E7 85) (defconstant F7 86) (defconstant G7 87) (defconstant H7 88)

(defconstant A8 91) (defconstant B8 92) (defconstant C8 93) (defconstant D8 94)
(defconstant E8 95) (defconstant F8 96) (defconstant G8 97) (defconstant H8 98)

;; We need to be able to map between board indices and symbolic field
;; names. This is done by the following vector =*fieldname*=

(defparameter *fieldname* 
  (vector
   nil nil nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil nil nil
   nil 'a1 'b1 'c1 'd1 'e1 'f1 'g1 'h1 nil 
   nil 'a2 'b2 'c2 'd2 'e2 'f2 'g2 'h2 nil 
   nil 'a3 'b3 'c3 'd3 'e3 'f3 'g3 'h3 nil 
   nil 'a4 'b4 'c4 'd4 'e4 'f4 'g4 'h4 nil 
   nil 'a5 'b5 'c5 'd5 'e5 'f5 'g5 'h5 nil 
   nil 'a6 'b6 'c6 'd6 'e6 'f6 'g6 'h6 nil 
   nil 'a7 'b7 'c7 'd7 'e7 'f7 'g7 'h7 nil 
   nil 'a8 'b8 'c8 'd8 'e8 'f8 'g8 'h8 nil 
   nil nil nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil nil nil
   ))


;; The table =*center-factor*= defines a partition of the board that
;; shows how central a field is in "manhatten distance". It is used to
;; calculate scores for the position of the knights and bishops a
;; bishop an position P get a score of (* (centralization-weight (aref
;; center-factor P)))


(defparameter *center-factor* 
  (vector
   0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0
   0 0 1 1 1 1 1 1 0 0
   0 0 1 2 2 2 2 1 0 0
   0 0 1 2 3 3 2 1 0 0
   0 0 1 2 3 3 2 1 0 0
   0 0 1 2 2 2 2 1 0 0
   0 0 1 1 1 1 1 1 0 0
   0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0
   ))

;; Move generation is done by the use of offsets that lead from one
;; field to the attacked fields. We make up some lists for for this
;; purpose.

(defparameter rook-dirs 
  '(10 1 -10 -1))
(defparameter bishop-dirs 
  '(9 11 -9 -11))
(defparameter knight-dirs 
  '( 8 19  21 12 -8 -19 -21 -12))
(defparameter queen-dirs 
  (append rook-dirs bishop-dirs))
;; the two pawn attack directions are reverted since
;; attack-checking starts form the field that could be attacked
(defparameter white-pawn-attack-dirs 
  '(-9 -11))
(defparameter black-pawn-attack-dirs  
  '(9 11))

;; The =*attack-table*= is needed to score attacks between various
;; pieces. E.g. with =(aref *attack-table* rook knight)= you can
;; retrieve a score how valueable it is considered to attack a knight
;; with a rook. The =*attack-table*= is primary used to support sorting
;; the movelist.

(defparameter *attack-table* (make-array (list (1+ king) (1+ king))))

;; This function is for filling the attack-table. Special manipulation is needed to deal with =pawn2=, =pawn3= and =rook2=

(defun set-attacks (attacker values)
  (loop for to in (list pawn knight bishop rook king queen)
        for v in values
     do (setf (aref *attack-table* attacker to) v))

  (setf (aref *attack-table* attacker pawn2) 
	(aref *attack-table* attacker pawn)) 
  (setf (aref *attack-table* attacker pawn3) 
	(aref *attack-table* attacker pawn)) 
  (setf (aref *attack-table* attacker rook2) 
	(aref *attack-table* attacker rook))) 

                   ;  p k b r q k 
(set-attacks king   '(1 2 2 3 4 5))
(set-attacks queen  '(2 3 3 4 5 6))
(set-attacks rook   '(3 4 4 5 6 7))
(set-attacks bishop '(4 5 5 6 7 8))
(set-attacks knight '(4 5 5 6 7 8))
(set-attacks pawn   '(5 6 6 7 8 9))
(loop for p in (list pawn pawn2 pawn3 knight bishop rook rook2 king queen) do
     (setf (aref *attack-table* pawn2 p) (aref *attack-table* pawn p))  
     (setf (aref *attack-table* pawn3 p) (aref *attack-table* pawn p))
     (setf (aref *attack-table* rook2 p) (aref *attack-table* rook p)) 
)
;; ============================================================================
;; ** Tuning Parameters
;; ============================================================================
;; This section defines constants that are used in heuristics

(defparameter *centralization-weight* 5)

;; ============================================================================
;; ** Structures
;; ============================================================================

;; *** Struct =castling-data=
;; Struct =castling-data= contains the information which of the
;; pieces, that are involved in a castling, have made a move already.

(defstruct castling-data 
  (king-has-moved        nil)
  (kings-rook-has-moved  nil)
  (queens-rook-has-moved nil)
)

;; *** Struct =node=
;; Struct =node= defines a node of the game tree. It consists of:
;;
;; | Slot Name          | Intital Value          | Purpose                                  |
;; |--------------------+------------------------+------------------------------------------|
;; | =board=            | =(empty-board)=        | the chees board                          |
;; | =tomove=           | =white=                | who moves next?                          |
;; | =castling-white=   | =(make-castling-data)= | castling info for white                  |
;; | =castling-black=   | =(make-castling-data)= | castling info for black                  |
;; | =ep=               | =nil=                  | position of the pawn                     |
;; |                    |                        | that made a double step in the last move |
;; | =50-moves-counter= | =0=                    | number of moves (50 moves rule)          |
;; | =moves=            | =nil=                  | all the moves that were made so far      |

(defstruct node 
  (board            (empty-board))  
  (tomove           'white)         
  (last-moves       nil)
  (castling-white   (make-castling-data))
  (castling-black   (make-castling-data))
  (ep               nil)
  (50-moves-counter 0)
  (moves            nil)            
)

(defparameter *node* (make-node :board (empty-board)))

;; ============================================================================
;; ** Data Handling
;; ============================================================================

(defun board-index (col row) 
  "calculate the array index of a specific field e.g (board-index 'a 1) ==>   21"
  (let((c (- (symbol-ascii col) 65))
       (r (- row 1)))
    (+ 21 (* 10 r) c)))

(defun place (board index piece)
  "place a piece onto a board"
  (setf (aref board index) piece)
)

(defmacro in-row? (n f)
  "is the field in row 1 ?"
  `(= (+ 1 ,n) (truncate (/ ,f 10))))


(defun move (board from to)
  "let a piece perform a move on a board"
  (place board to (aref board from))
  (setf (aref board from) none)
  (if (and (eql pawn (aref board to))
	   (in-row? 8 to))
      (setf (aref board to) queen))
  (if (and (eql -pawn (aref board to))
	   (in-row? 1 to))
      (setf (aref board to) -queen)))

;; ============================================================================
;; ** Predicates
;; ============================================================================

;; Various stuff to check properties

(defun opponent-or-free? (color your-piece)
"check, if a player of color can move to a field with 'your-piece' on it" 
  (and (not (eql your-piece stop))
       (>= 0 (* color your-piece))))

(defun opponent? (color your-piece) 
"check, if your-piece belongs to my opponent"
  (and (not (eql your-piece stop))
       (> 0 (* color your-piece))))

(defun my-color? (color piece) ;;; ok
"check, if this piece is mine"
  (and (not (eql piece stop))
       (> 0 (* (- color) piece))))

(defun white? (x) 
  (and (> x 0) (not (eql x stop))))

(defun black? (x) 
  (< x 0))

;; ============================================================================
;; ** Attack
;; ============================================================================

;; The Functions defined here are used to check if special fields are
;; under attack. This is useful to check if castling is legal or if
;; someone's king is under attack

(defun long-attack?(board field dirs pieces)
  (declare (type integer field))
  (dolist (d dirs) ; for every direction
    (do ((f (+ field d) (+ f d))) ; f iterates
	((not (eql 0 (aref board f))) ; until board[f] contains a piece
	 (let ((piece (aref board f))) ; get piece
	   (when (member piece pieces)
	     (return-from long-attack? f ))))))) ; then return f

(defun short-attack?(board field dirs pieces)
  (declare (type integer field))
  (dolist (d dirs)
    (let* ((f (+ field d)) (p (aref board (+ field d))))
      (if (member p pieces :test #'eql)
	  (return-from short-attack? f)))))

(defun any-attack (board field color) ;;; seems to be ok
(declare (type integer color field))
  (or (if (eql color white)
	  (short-attack? board field white-pawn-attack-dirs 
			 (list (* color pawn)))
	  (short-attack? board field black-pawn-attack-dirs 
			 (list (* color pawn))))
      (short-attack? board field bishop-dirs 
		     (list (* color bishop) (* color queen) (* color king)))
      (short-attack? board field rook-dirs
		     (list (* color rook)   (* color queen) (* color king)))
      (short-attack? board field knight-dirs (list (* color knight)))
      (long-attack?  board field bishop-dirs 
		     (list (* color bishop) (* color queen)))
      (long-attack?  board field rook-dirs   
		     (list (* color rook)   (* color queen)))))

;; ============================================================================
;; ** Move Generation
;; ============================================================================

;; Function =movelist= is the entire move generator. It generates a
;; list of all possible moves for a given color
(defun movelist (node color)
  "generate an unsorted moveslist for  given color and node" 
  (let ((board (node-board node)) (ml (castling-moves node color nil )))
    (do ((i a1 (+ 1 i))) 
	((> i h8) ml)
    (let* ((piece (aref board i))
	   (abs-piece (abs piece)))
      (if (my-color? color piece)
	  (setq ml
		(cond
		  ((eql abs-piece pawn)
		   (pawn-moves board color i ml))
		  ((eql abs-piece rook)
		   (long-move board color i rook-dirs ml))
		  ((eql abs-piece bishop)
		   (long-move board color i bishop-dirs ml))
		  ((eql abs-piece knight)
		   (short-move board color i knight-dirs ml))
		  ((eql abs-piece king)
		   (short-move board color i queen-dirs ml))
		  ((eql abs-piece queen)
		   (long-move board color i queen-dirs ml))
		  (t ml))))))))

;; =kings-position= calculates the position of a king
(defun kings-position (board color)
  "find the position of the king"
  (declare (type integer color))
  (let ((p (position (* king color) board :start a1 :end (1+ h8))))
;;    p))
(if p p (error "king has vanished"))))

;; Nomen est omen: This checks, if color's king is given check
;;; ok
(defun check? (node color)
  "is color in check?"
  (declare (type integer color))
  (declare (type node node))
  (let* ((board (node-board node))
	 (fking (kings-position board color)))
;;    (or (not fking) ;; not sure if this is good. This is more a hack:
		    ;; The king has vanished due to an illegal
		    ;; move. We are evaluating this as check here,
		    ;; because the move will be ignored this way. in a
		    ;; correct solution we would simply not inspect
		    ;; the move that captured the king
    (any-attack board fking (- color))))
;;)

;; ** TODO this is ineffective because the kings position is
;; calculated multiple times


(defun check-mate? (node color)
  (declare (type integer color))
  (declare (type node node))
  (and (check? node color)
       (loop for m in (movelist node color)
	  always (check? (do-move node m) color))))

;; The move generator functions calculates a list of possible
;; moves. Any routine takes an argument =ml= for the move list, that
;; is calculated already. It conses the newly generated moves to it
;; and returns the extended move list. We distinguish between short
;; moves (king,knight), long moves (queen,rook,bishop), pawn moves and
;; castling moves.

;;*** Short Moves
(defun short-move (board color from dirs ml);;; ok
"move-generator for king and knight"
  (dolist (d dirs ml)
    (let ((to (+ from d)))
      (if (opponent-or-free? color (aref board to))
	  (setq ml (cons (list from to) ml))))))

;;*** Long Moves
(defun long-move-1-dir (board color from dir ml) ;;; ok
  (do ((to (+ from dir) (+ to dir)))
      ((not (eql none (aref board to))) 
       (if (opponent? color (aref board to))
	   (cons (list from to) ml)
	   ml))
    (setq ml (cons (list from to) ml))))


(defun long-move (board color from dirs ml);;; ok
"move-generator for bishop, rook and queen" 
  (dolist (d dirs ml)
    (setq ml (long-move-1-dir board color from d ml))))


;; *** Pawn Moves

(defun pawn-moves (board color from ml)
  (let* ((*10color (* 10 color)))
    (let ((dest (+ from *10color)))
      (when (eql 0  (aref board dest))
	(setq ml (cons (list from dest) ml)) ; single step
	(let ((pawn-row (if (eql color white) 2 7)))
	  (let ((dest2 (+ from *10color *10color)))
	    (when (and (in-row? pawn-row from)
		       (eql 0 (aref board dest2)))
	      (setq ml (cons (list from dest2) ml))))))) ; double step
    (if (opponent? color (aref board (+ 1 from *10color))) 
	(setq ml (cons (list from (+ 1 from *10color)) ml))) ; capture right
    (if (opponent? color (aref board (+ -1  from *10color)))
	(setq ml (cons (list from (+ -1 from *10color)) ml))) ; capture left
    ml))
;; *** Castling Moves 
;; the following has to be checked in the order below:
;;  1. is the king on e1 resp. e8?
;;  2. did the king not move yet
;;  3. is the rook on a1/h1 a8/h8
;;  4. both involved pieces not moves already?
;;  5. are the fields between king and rook not under attack?
;;  6. if no question was answered with yes, then the castiling is allowed.
;; castling-moves checks posiltoions 1 and 2. the other ones are
;; checked by white... and black...

(defun castling-moves (node color ml)
"generate the castling moves for color 'color'"
  (let ((board (node-board node))
	(castling (if (eql color white) 
		      (node-castling-white node)
		      (node-castling-black node)))
	(kingpos  (if (eql color white) e1 e8)))
    (and (eql (aref board kingpos) (* color king))
	 (not (castling-data-king-has-moved castling))
	 (if (eql color white)
	     (white-castling-moves board castling ml)
	     (black-castling-moves board castling ml)))))

(defun white-castling-moves (board castling ml)
"generate the castling moves for white"
(if  (and (= none (aref board f1)) (= none (aref board g1))
          (not (castling-data-kings-rook-has-moved castling))
	  (not (any-attack board f1 black))
	  (not (any-attack board g1 black))
	  (not (any-attack board h1 black)))
     (setq ml (cons 'kingside-castling ml)))
(if  (and (= none (aref board d1)) (= none (aref board c1)) (= none (aref board b1))
	  (not (castling-data-queens-rook-has-moved castling))
	  (not (any-attack board d1 black))
	  (not (any-attack board c1 black))
	  (not (any-attack board b1 black))
	  (not (any-attack board a1 black)))
     (setq ml (cons 'queenside-castling ml)))
ml) 

(defun black-castling-moves (board castling ml)
"generate the castling moves for black"
(if  (and (= none (aref board f8)) (= none (aref board g8))
	  (not (castling-data-kings-rook-has-moved castling))
	  (not (any-attack board f8 white))
	  (not (any-attack board g8 white))
	  (not (any-attack board h8 white)))
     (setq ml (cons 'kingside-castling ml)))
(if  (and (= none (aref board d8)) (= none (aref board c8)) (= none (aref board b8))
	  (not (castling-data-queens-rook-has-moved castling))
	  (not (any-attack board d8 white))
	  (not (any-attack board c8 white))
	  (not (any-attack board b8 white))
	  (not (any-attack board a8 white)))
     (setq ml (cons 'queenside-castling ml)))
ml)

;; ============================================================================
;; ** Analysis
;; ============================================================================

(defun terminal? (node)
node
nil
)
;;  (let ((b (node-board node)))
;;    nil))

(defun static-value (piece)
  "calculate the static value of a given piece"
  (* (signum piece) (aref *piece-value* (abs piece))))

;; todo: this is not very much yet. misses movability, pawn structure
;; and any positional asspect. We will implement the following
;; criteria here: value of all pieces movability of the officers

(defun heuristic (node)
  (let ((sum 0) (board (node-board node)))
    (do ((i a1 (+ 1 i))) 
	((> i h8) sum)
      (setq sum (+ sum (static-value (aref board i)))))))


(defvar *n* 0)

(defun heuristic-solve (node color)
  (setq *n* (1+ *n*))
  (if (check-mate? node color)
      1 0))

(defun do-move (node move)
"Do a move and return the resulting node"
  (let ((newboard (copy-seq (node-board node)))
	(newnode (copy-node node)))
    (move newboard (first move) (second move))
    (setf (node-board newnode) newboard)
    (setf (node-last-moves newnode) (cons (decode-move move) (node-last-moves node)))
    newnode))

;; ** Minimax (Negamax) Procedure 

;; This is the core procedure to calculate the best move for a given
;; color at a given node. It is essential for any two-person game with
;; full information. We use the more concise negamax version here.
;; This is from wikipedia
;; #+BEGIN_SRC
;;
;; function negamax(node, depth, α, β, color)
;;     if node is a terminal node or depth = 0
;;         return color * the heuristic value of node
;;     else
;;         foreach child of node
;;             α := max(α, -negamax(child, depth-1, -β, -α, -color))
;;             {the following if statement constitutes alpha-beta pruning}
;;             if α≥β
;;                 return α
;;         return α
;;
;; #+END_SRC

(defun negamax (node depth alpha beta color)
  (if (or (terminal? node) (eql 0 depth))
      (* color (heuristic node))
      (let ((variant nil) 
	    (ml (movelist node color)))
	(dolist (move ml 
		 (values alpha variant))
	  (let ((child (do-move node move)))
	    (multiple-value-bind (result following-variant) 
		(- (negamax child (- depth 1) (- beta) (- alpha) (- color))) 
	      (when (> result alpha) 
		(setq alpha result)
		(setq variant (cons move following-variant))
		)
	      (when (>= alpha beta)
		(return (values alpha variant)))))))))

;; This is a simplified version for testing the engine. It shall solve
;; "mate-in-X"-problems

(defun negamax-solve% (node depth alpha beta color)
  (if (or (terminal? node) (eql 0 depth))
      (* color (heuristic-solve node color))
      (let ((variant nil) 
	    (ml (movelist node color)))
	(dolist (move ml 
		 (values alpha variant))
	  (let ((child (do-move node move)))
	    (when (not (check? child color))
	      (multiple-value-bind (result following-variant) 
		  (- (negamax-solve child (- depth 1) (- beta) (- alpha) (- color))) 
		(when (> result alpha) 
		  (setq alpha result)
		  (setq variant (cons move following-variant))
		  )
		(when (>= alpha beta)
		  (return (values alpha variant))))))))))

(defun sort-ml-solve (node ml color)
;; this handler sorts the ml in a way that the check giving moves
;; appear first.
  (let ((cmoves
	 (loop for m in ml 
	    when (check? (do-move node m) (- color))
	      collect m)))
    (append cmoves (remove-if #'(lambda (m) (member m cmoves)) ml))))

(defun negamax-solve (node depth alpha beta color)
  (if (or (terminal? node) (eql 0 depth))
      (* color (heuristic-solve node color))
      (let ((variant nil) 
	    (ml (sort-ml-solve node (movelist node color) color)))
	(dolist (move ml 
		 (values alpha variant))
	  (let ((child (do-move node move)))
	    (when (not (check? child color))
	      (multiple-value-bind (result following-variant) 
		  (- (negamax-solve child (- depth 1) (- beta) (- alpha) (- color))) 
		(when (> result alpha) 
		  (setq alpha result)
		  (setq variant (cons move following-variant))
		  )
		(when (>= alpha beta)
		  (return (values alpha variant))))))))))


;; ============================================================================
;; ** Output
;; ============================================================================

;; The following functions are needed to generate readable output
;; of the chess board and move lists.

(defun piece-char (n) 
"convert the code of a piece into a mnemonic string"
  (cond 
    ((eql n -king)    "*K")
    ((eql n -queen)   "*D")
    ((eql n -rook2)   "*T")
    ((eql n -rook)    "*T")
    ((eql n -bishop)  "*L")
    ((eql n -knight)  "*S")
    ((eql n -pawn3)   "*B")
    ((eql n -pawn2)   "*B")
    ((eql n -pawn)    "*B")
    ((eql n  none)    " -")
    ((eql n king)     " K")
    ((eql n queen)    " D")
    ((eql n rook2)    " T")
    ((eql n rook)     " T")
    ((eql n bishop)   " L")
    ((eql n knight)   " S")
    ((eql n pawn3)    " B")
    ((eql n pawn2)    " B")
    ((eql n pawn)     " B")
    (t n)))

(defun ml-readable (ml) 
"convert a board index into an readable symbol"
  (deep-mapcar #'(lambda (x) (if (numberp x)  (aref *fieldname* x) x)) ml))

(defun decode-move (m) 
  (list (aref *fieldname* (first m)) (aref *fieldname* (second m))))

(defun pp (node)
  "print a chess board"
  (terpri)
  (let ((b (node-board node)))
    (format t "  A B C D E F G H~%")
    (dolist (l '(8 7 6 5 4 3 2 1))
      (format t "~d" l)
      (dolist (r '(A B C D E F G H))
	(format t "~A" (piece-char (aref b (board-index r l)))))
      (terpri))))

;; ============================================================================
;; ** Input / DSL
;; ============================================================================

;; This is the userinterface

(defun m (f to) (move  (node-board *node*) f to))
(defun p (p f) (place (node-board *node*) f p))
(defun b () (pp *node*))
(defun empty () (setq *node* (make-node :board (empty-board))))
(defun new ()   (setq *node* (make-node :board (start-position))))
(defun s ()
  (print (list 'black (ml-readable (movelist *node* black))))
  (print (list 'white (ml-readable (movelist *node* white))))
  t
)

(defun bwm (d) (negamax *node* d -2000000 2000000 1))
(defun bbm (d) (negamax *node* d -2000000 2000000 -1))

(defun solve (moves color)
  (setq *n* 0)
  (let ((depth (1- (* 2 moves))))
    (multiple-value-bind (result variant) 
	(negamax-solve *node* depth -2000000 2000000 color)
      (list   (decode-move (car variant)) result *n*))))

(defun tsolve (moves color)
  (time (solve moves color)))

(defun sb (name) 
  (with-open-file (qux name :direction :output) 
    (print *node* qux)))

(defun lb (name) 
  (with-open-file (qux name :direction :input) 
    (setq *node* (read qux))))

;; ======================================
;; (m e2 e3)        --- move
;; (p e2 -king..)v  --- place
;; (s)              --- status
;; (b)              --- show board
;; (sb "filename")  --- save *node* to disk
;; (lb "filename")  --- load *node* from disk
;; (sc)             --- switch color

;; (sp x x x x)     --- set search parameter
;; (new)            --- start new game
;; (empty)          --- empty board 
;; (evaluate)       --- do static evaluation
;; (solve 2 white)  --- solve a mate-in-2 problem
;; ============================================================================
;; ** Main
;; ============================================================================

(setf (node-board *node*) #(10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 0
			    9 0 6 0 0 0 10 10 0 0 1 1 8 0 0 0 10 10 0 1 0 0 0 0 0 0 10 10 0 0 0
			    -8 0 0 0 0 10 10 0 0 0 0 0 0 0 0 10 10 0 0 0 0 0 0 0 0 10 10 0 0 0
			    0 0 -1 -1 -1 10 10 0 0 0 -6 0 -9 0 0 10 10 10 10 10 10 10 10 10 10
			    10 10 10 10 10 10 10 10 10 10 10))

(print (ml-readable (movelist *node* white)))
(pp *node*)

;; * Code Download
;; [[file:cc.lisp][download the whole code file here]]
;;(print (ml-readable (movelist board black)))
;;
;; Chess-Symbols unicode x2654 - x265F
;; (Emacs-Eingabe mit Ctrl-x 8 [Enter])
;; * License
;; Do what ever you want to do with this code.
;; * To-Do
;; ** TODO Regularium: e.p double step
;; ** TODO Strategie: Vorsortierung, Zugauswahl
;; ** TODO performance: copy of node is ineffective
;; ** TODO do-move: The castling-variables have to be maintained
;; *IDEA*
;;'(setq evaluator (make-negamax 'param 'for 'level 1 
;;                   (make-negamax 'param 'for 'level 2 
;;                     (make-negamax 'param 'for 'level 3))))





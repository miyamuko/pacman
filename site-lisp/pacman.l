; -*- mode: lisp; package: pacman -*-

;;; pacman.l
;;
;; Copyright (c) 2010 MIYAMUKO Katsuyuki.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;; * Description
;;
;;   xyzzy Lisp implementation of Google Developer Day 2010 DevQuiz PAC-MAN game.
;;
;; * How to use:
;;
;;   See http://github.com/miyamuko/pacman/README.rdoc
;;

;;; Code:

(in-package :user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "cmu_loop")

  (unless (find-package :pacman)
    (defpackage :pacman
      (:use :lisp :editor))
    (use-package :pacman :user)
    ))

(in-package :pacman)

(export '(pacman-start-from-clipboard
          ))

(defparameter *pacman-debug* nil)
(defparameter *pacman-debug-message-list* nil)
(defparameter *pacman-debug-message-max-count* 10)
(defparameter *pacman-debug-symbols*
  '(*pacman-char-list*
    *pacman-stage*
    *pacman-debug-message-list*
    *pacman-move-history*
    ))

(defparameter *pacman-raw-data* nil)
(defparameter *pacman-timelimit* nil)
(defparameter *pacman-stage-width* nil)
(defparameter *pacman-stage-height* nil)

(defparameter *pacman-stage-dot-char* #\+)
(defparameter *pacman-stage* nil)
(defparameter *pacman-stage-history* nil)

(defparameter *pacman-char-list* nil)
(defparameter *pacman-char-list-history* nil)

(defparameter *pacman-move-history* nil)

(defparameter *pacman-game-clear-pattern-list* nil)
(defvar *pacman-game-over-pattern-list* nil)


;;; データ型

(defmacro make-pos (x y)
  `(list ,x ,y))

(defmacro pos-x (pos)
  `(car ,pos))

(defmacro pos-y (pos)
  `(cadr ,pos))


(defstruct pacman-char
  id kind pos prev-pos direction state)

(defun pacman-char-enemy-p (char)
  (not (pacman-char-player-p char)))

(defun pacman-char-player-p (char)
  (char= (pacman-char-kind char) #\@))


;;; pacman-mode 初期化

(defparameter *pacman-regexp-keyword-list* nil)
(setf *pacman-regexp-keyword-list*
      (compile-regexp-keyword-list
       `(
         ("#" nil ((0 . :comment)))
         ("@" nil ((0 . 3)))
         ("[VH]" nil ((0 . 4)))
         ("[LRJ]" nil ((0 . 5)))
         ("\\(time\\|game\\) over!" nil ((0 . (:keyword 1 :bold))))
         )))

(defparameter *pacman-mode-map* nil)
(unless *pacman-mode-map*
  (setf *pacman-mode-map* (make-sparse-keymap))
  ;; common
  (define-key *pacman-mode-map* #\C-h 'pacman-undo-to-intersection)
  (define-key *pacman-mode-map* #\C-z 'pacman-undo-to-intersection)
  (define-key *pacman-mode-map* #\C-M-h 'pacman-undo-to-last-eat)
  (define-key *pacman-mode-map* #\C-M-z 'pacman-undo-to-last-eat)
  (define-key *pacman-mode-map* #\a 'pacman-auto-play)
  (define-key *pacman-mode-map* #\d 'toggle-pacman-debug)
  (define-key *pacman-mode-map* #\b 'pacman-batch-play)
  (define-key *pacman-mode-map* #\r 'pacman-reset)
  (define-key *pacman-mode-map* #\q 'pacman-quit)
  ;; for Windows user
  (define-key *pacman-mode-map* #\Left 'pacman-move-left)
  (define-key *pacman-mode-map* #\Down 'pacman-move-down)
  (define-key *pacman-mode-map* #\Up 'pacman-move-up)
  (define-key *pacman-mode-map* #\Right 'pacman-move-right)
  (define-key *pacman-mode-map* #\C-m 'pacman-stay)
  ;; for Emacs user
  (define-key *pacman-mode-map* #\C-b 'pacman-move-left)
  (define-key *pacman-mode-map* #\C-n 'pacman-move-down)
  (define-key *pacman-mode-map* #\C-p 'pacman-move-up)
  (define-key *pacman-mode-map* #\C-f 'pacman-move-right)
  (define-key *pacman-mode-map* #\SPC 'pacman-stay)
  ;; for VI user
  (define-key *pacman-mode-map* #\h 'pacman-move-left)
  (define-key *pacman-mode-map* #\j 'pacman-move-down)
  (define-key *pacman-mode-map* #\k 'pacman-move-up)
  (define-key *pacman-mode-map* #\l 'pacman-move-right)
  (define-key *pacman-mode-map* #\. 'pacman-stay)
  )

(defun pacman-mode ()
  (interactive)
  (kill-all-local-variables)
  (setf mode-name "PAC-MAN")
  (setf buffer-mode 'pacman-mode)
  (use-keymap *pacman-mode-map*)
  (make-local-variable 'regexp-keyword-list)
  (setf regexp-keyword-list *pacman-regexp-keyword-list*))


;;; pacman-mode コマンド

(defun pacman-start-from-clipboard ()
  (interactive)
  (let ((buf (get-buffer-create "*PAC-MAN*")))
    (set-buffer buf)
    (setq need-not-save t)
    (setq auto-save nil)
    (unless (init-frame (get-clipboard-data))
      (delete-buffer buf)
      (return-from pacman-start-from-clipboard))
    (stage-message "start!")
    (pacman-mode)
    (pop-to-buffer buf t)
    ))

(defparameter *pacman-auto-play-p* nil)
(defparameter *pacman-auto-play-count* 0)
(defparameter *pacman-auto-play-high-score* nil)
(defparameter *pacman-auto-play-refresh-count* 100)
(defun pacman-auto-play ()
  (interactive)
  (setf *pacman-auto-play-count* 0)
  (let ((*pacman-auto-play-p* t))
    (labels ((player ()
               (pacman-player-char))
             (refresh-maybe ()
               (when (zerop (mod *pacman-auto-play-count*
                                 *pacman-auto-play-refresh-count*))
                 (refresh-screen)
                 (do-events))
               (incf *pacman-auto-play-count*))
             (try1 (direction)
               (multiple-value-bind (char next-pos pos)
                   (char-next-pos (player) direction)
                 (let ((moveable (find-moveable-directions (player))))
                   ;; 「移動可能」 かつ
                   ;; 「前にいた場所ではない (他に選択肢がない場合を除く)」
                   ;; に移動してみる
                   (when (and (can-move-p (pacman-player-char) direction)
                              (or (not (and (pacman-char-prev-pos (player))
                                            (equal (pacman-char-prev-pos (player)) next-pos)))
                                  (= (length moveable) 1)))
                     (pacman-move direction)
                     (refresh-maybe)
                     (when (or (game-over-p) (not (try-all)))
                       (let ((leftovers (count-remaining-dot))
                             (elapsed (current-time-elapsed)))
                         (when (or
                                ;; 初期状態
                                (not *pacman-auto-play-high-score*)
                                ;; 食べ残しがより少ない
                                (< leftovers (car *pacman-auto-play-high-score*))
                                ;; 食べ残しは同じだが時間がより短い
                                (and (= leftovers (car *pacman-auto-play-high-score*))
                                     (< elapsed (cadr *pacman-auto-play-high-score*))))
                           (setf *pacman-auto-play-high-score*
                                 (list leftovers elapsed (pacman-answer)))))
                       (pacman-undo))))))
             (try-all ()
               (or (try1 :up)
                   (try1 :right)
                   (try1 :down)
                   (try1 :left)
                   (try1 :stay)
                   )))
      (try-all))
    (stage-message "探索終了 (answer count ~D)" (length *pacman-game-clear-pattern-list*))))

(defun pacman-batch-play (str)
  (interactive "sEnter Answer: ")
  (if (string-matchp "[^hjkl.]" str)
      (msgbox "回答文字列に不正な文字が含まれています: ~S at ~D"
              (match-string 0) (match-beginning 0))
    (progn
      (loop for c across str
        do (progn
             (pacman-move (direction-char>direction c))
             (refresh-screen)
             (do-events)
             (sit-for 0.1)
             )))))

(defun pacman-reset ()
  (interactive)
  (when (init-frame *pacman-raw-data*)
    (stage-message "reset!")))

(defun pacman-quit ()
  (interactive)
  (when (eql buffer-mode 'pacman-mode)
    (delete-buffer (selected-buffer)))
  (init-variables))

(defun pacman-undo ()
  (interactive)
  (restore-frame))

(defun pacman-undo-to-intersection ()
  (interactive)
  (while (can-undo-p)
    (restore-frame)
    (multiple-value-bind (_ pos)
        (find-char #\@)
      (when (stage-intersection-p pos)
        (return)))))

(defun pacman-undo-to-last-eat ()
  (interactive)
  (let ((current (count-remaining-dot)))
    (while (and (= current (count-remaining-dot))
                (can-undo-p))
      (restore-frame))))

(defun pacman-move-up ()
  (interactive)
  (pacman-move :up))

(defun pacman-move-down ()
  (interactive)
  (pacman-move :down))

(defun pacman-move-left ()
  (interactive)
  (pacman-move :left))

(defun pacman-move-right ()
  (interactive)
  (pacman-move :right))

(defun pacman-stay ()
  (interactive)
  (pacman-move :stay))

(defun toggle-pacman-debug ()
  (interactive)
  (setf *pacman-debug* (not *pacman-debug*))
  (render-stage))

(defun d (fmt &rest args)
  (push (apply 'format nil fmt args) *pacman-debug-message-list*)
  (let ((n (- (length *pacman-debug-message-list*)
              *pacman-debug-message-max-count*)))
    (when (< 0 n)
      (setf *pacman-debug-message-list*
            (nbutlast *pacman-debug-message-list* n)))))


;;; 状態管理

(defun init-variables ()
  (setf *pacman-debug-message-list* nil
        *pacman-raw-data* nil
        *pacman-timelimit* nil
        *pacman-stage-width* nil
        *pacman-stage-height* nil
        *pacman-stage* nil
        *pacman-stage-history* nil
        *pacman-char-list* nil
        *pacman-char-list-history* nil
        *pacman-move-history* nil))


(defun pop-move-history ()
  (setf *pacman-move-history* (nbutlast *pacman-move-history*)))

(defun save-move-history (direction)
  (let ((char (direction->direction-char direction)))
    ;; 表示処理を高速化するため保存時に順番を維持する (push を使わない)
    (setf *pacman-move-history* (append *pacman-move-history* (list char)))))

(defun init-frame (stage)
  (init-variables)
  (init-stage stage))

(defun new-frame ()
  (push (copy-stage *pacman-stage*) *pacman-stage-history*)
  (push (copy-char-list *pacman-char-list*) *pacman-char-list-history*)
  )

(defun can-undo-p ()
  (not (null *pacman-move-history*)))

(defun restore-frame ()
  (if (can-undo-p)
      (progn
        (setf *pacman-stage* (pop *pacman-stage-history*)
              *pacman-char-list* (pop *pacman-char-list-history*))
        (pop-move-history)
        (render-stage))
    (progn
      (stage-message "これ以上undoできません"))))

(defun init-stage (stage-str)
  (handler-case
      (progn
        (let ((stage (split-string stage-str #\LFD)))
          (setf *pacman-timelimit* (parse-integer (pop stage)))
          (when (string-matchp "^\\([0-9]+\\) +\\([0-9]+\\)$" (pop stage))
            (setf *pacman-stage-width* (parse-integer (match-string 1)))
            (setf *pacman-stage-height* (parse-integer (match-string 2))))

          ;; プレイヤーと敵のリストを作成
          (setf *pacman-char-list* (make-pacman-char-list stage))
          (setf *pacman-stage* (make-stage stage))
          (setf *pacman-raw-data* stage-str)))
    (error (c)
      (msgbox "入力が不正です。クリップボードに DevQuiz の入力をコピーして実行してください")
      nil
      )))

(defun make-stage (stage)
  (format nil "~{~A~%~}"
          (mapcar #'(lambda (line)
                      (substitute-string
                       (substitute-string line "[A-Z@]" " ")
                       "\\." (string *pacman-stage-dot-char*)))
                  stage)))

;; 初期状態は次の条件を満たします。
;;
;; - 同種の敵(おなじ英字で表される敵)が複数存在することがあります。
(defun make-pacman-char-list (stage)
  (let ((r (loop with id = 0 for line in stage and y = 0 then (1+ y) append
             (loop for cell across line and x = 0 then (1+ x)
               when (or (alpha-char-p cell)
                        (char= cell #\@))
               collect (make-pacman-char
                        ;; player の id は 0
                        :id (if (char= cell #\@) 0 (incf id))
                        :kind cell :pos (make-pos x y))))))
    ;; player が先頭に来るようにする
    (stable-sort r #'< :key #'pacman-char-id)))

(defun pacman-player-char ()
  (car *pacman-char-list*))

(defun pacman-enemy-char-list ()
  (cdr *pacman-char-list*))

;; 自機と敵が衝突するとゲームオーバーです。
;; ここで自機とある敵 x が時刻 t = 1, 2, ..., T に衝突するとは次のいずれかの条件を満たす場合をいいます。
;;
;; - 時刻 t に自機と敵 x が同じマスに存在する。
;; - 時刻 t-1 における自機と敵 x の位置と、時刻 t における自機と敵 x の位置が交換されている。
;;
;; 時刻 t に衝突した場合、自機は時刻 t にドットを回収することはできません。
(defun game-clear-p ()
  (zerop (count-remaining-dot)))

(defun game-over-p ()
  (let ((player (pacman-player-char))
        (enemy-list (pacman-enemy-char-list)))
    (or (game-clear-p)
        (time-over-p)
        (find-if #'(lambda (enemy)
                     (equal (pacman-char-pos player) (pacman-char-pos enemy)))
                 enemy-list)
        (find-if #'(lambda (enemy)
                     (and (not (first-frame-p))
                          (equal (pacman-char-pos player) (pacman-char-prev-pos enemy))
                          (equal (pacman-char-prev-pos player) (pacman-char-pos enemy))))
                 enemy-list))))

(defun first-frame-p ()
  ;; 最初のフレーム (t = 0 の場合) なら t
  (= (current-time-elapsed) 0))

(defun time-over-p ()
  (< (current-time-remaining) (count-remaining-dot)))

(defun current-time-elapsed ()
  (length *pacman-move-history*))

(defun current-time-remaining ()
  (- *pacman-timelimit* (current-time-elapsed)))


;;; 移動管理

;; ゲームの進行
;; 時刻 t = 0, 1, ..., T-1 において、自機と敵は隣り合う 4 方向のマスに移動することができます。
;; 敵は必ず移動しますが、自機はその場に留まることを選択することもできます。
;; 全機の行動は同時に行われ、時刻が 1 進められます。

;; 自機がドットのあるマスに進入すると、ドットを 1 つ回収することができます。
;; 一度ドットを回収したマスからは二度とドットを得ることはできません。
;; 敵がドットのあるマスに進入しても何も起こりません。

(defun pacman-move (direction)
  (when (game-over-p)
    (return-from pacman-move nil))
  (unless (can-move-p #\@ direction)
    (stage-message "壁です")
    (return-from pacman-move nil))
  (new-frame)
  (move-enemy)
  (move-player direction)
  (when (game-over-p)
    (unless *pacman-auto-play-p*
      ;; 遅いので自動探索中は記録しない
      (pushnew (pacman-answer) *pacman-game-over-pattern-list* :test #'string=))
    (stage-message (if (time-over-p) "time over!" "game over!"))
    (return-from pacman-move nil))
  ;; 時刻 t に衝突した場合、自機は時刻 t にドットを回収することはできません。
  (eat-dot)
  (when (game-clear-p)
    (pushnew (pacman-answer) *pacman-game-clear-pattern-list* :test #'string=)
    (stage-message "all your dot are belong to pacman!")
    (ed::kill-new (pacman-answer))
    (setf *kill-ring-newer-than-clipboard-p* t
          *clipboard-newer-than-kill-ring-p* nil)
    (message "kill-ringに回答をコピーしました")
    (return-from pacman-move nil))
  (render-stage))

(defun move-player (direction)
  (move-char #\@ direction)
  (save-move-history direction))

(defun move-char (char-designator direction)
  (multiple-value-bind (char next-pos pos)
      (char-next-pos char-designator direction)
    (when (not (wall-p next-pos))
      (setf (pacman-char-pos char) next-pos
            (pacman-char-prev-pos char) pos
            (pacman-char-direction char) direction
            )
      next-pos)))

;; 時刻 t = 0 においては、初期位置の 下、左、上、右 の順で最初に進入可能なマスの方向に移動します。
;; 時刻 t = 1, 2, ..., T-1 における移動方向は、現在位置の種別によって次のように決定されます。
;; - 行き止まりマスの場合
;;   - 唯一進入可能な隣接マスに移動します。
;; - 通路マスの場合
;;   - 時刻 t-1 に居たマス以外の進入可能な隣接するマスに移動します。
(defun move-enemy ()
  (dolist (enemy (pacman-enemy-char-list))
    (let ((pos (pacman-char-pos enemy)))
      (cond ((first-frame-p)              ; t = 0 の場合
             (move-first-space enemy (list :down :left :up :right)))
            ((stage-1-way-cell-p pos)     ; 行き止まりマスの場合
             (move-first-space enemy (list :down :left :up :right)))
            ((stage-2-way-cell-p pos)         ; 通路マスの場合
             (move-if #'(lambda (direction)
                          (multiple-value-bind (char next-pos pos)
                              (char-next-pos enemy direction)
                            (not (equal (pacman-char-prev-pos enemy) next-pos))))
                      enemy (list :up :right :down :left)))
            ((stage-intersection-p pos)       ; 交差点マスの場合
             (case (pacman-char-kind enemy)
               (#\V (move-enemy-on-intersection-V enemy))
               (#\H (move-enemy-on-intersection-H enemy))
               (#\L (move-enemy-on-intersection-L enemy))
               (#\R (move-enemy-on-intersection-R enemy))
               (#\J (move-enemy-on-intersection-J enemy))
               ))
            (t
             (error "Unknown cell")) ; bug
            ))))

;; - 交差点マスの場合
;;   - 敵 V
;;     - 敵から見た自機の相対位置を (dx, dy) と表すものとします。
;;     - 次のルールを上から順に適用し、最初に選ばれた方向に移動します。
;;       - dy ≠ 0 でかつ dy の符号方向にあるマスが進入可能であれば、その方向に移動します。
;;       - dx ≠ 0 でかつ dx の符号方向にあるマスが進入可能であれば、その方向に移動します。
;;       - 現在位置の 下、左、上、右 の順で最初に進入可能なマスの方向に移動する。
(defun move-enemy-on-intersection-V (enemy)
  (move-enemy-on-intersection-VH enemy t))

;;   - 敵 H
;;     - 敵 V とほぼ同じです。唯一異なるのは 、進行方向を決めるルールのうち、
;;       最初の二つのルールの適用順序が入れ替わるところです。
;;       すなわち、先に dx ≠ 0 のチェックを行ない、次に dy ≠ 0 のチェックを行います。
(defun move-enemy-on-intersection-H (enemy)
  (move-enemy-on-intersection-VH enemy nil))

(defun move-enemy-on-intersection-VH (enemy v)
  (multiple-value-bind (dx dy)
      (relative-position (find-char #\@) enemy)
    (labels ((move1 ()
               (let ((direction (if (minusp dy) :up :down)))
                 (when (and (not (zerop dy))
                            (can-move-p enemy direction))
                   (move-char enemy direction))))
             (move2 ()
               (let ((direction (if (minusp dx) :left :right)))
                 (when (and (not (zerop dx))
                            (can-move-p enemy direction))
                   (move-char enemy direction))))
             (move3 ()
               (move-first-space enemy (list :down :left :up :right))))
      (if v
          (or (move1) (move2) (move3))
        (or (move2) (move1) (move3))))))

;;   - 敵 L
;;     - 現在位置への進入方向から見て相対的に 左、前、右 の順で最初に進入可能なマスの方向に移動します。
(defun move-enemy-on-intersection-L (enemy)
  (move-first-space-relative enemy (list :left :up :right)))

;;   - 敵 R
;;     - 現在位置への進入方向から見て相対的に 右、前、左 の順で最初に進入可能なマスの方向に移動します。
(defun move-enemy-on-intersection-R (enemy)
  (move-first-space-relative enemy (list :right :up :left)))

;;   - 敵 J
;;     - 交差点マスに入るたびに、最初は敵Lの行動、次回は敵Rの行動、さらに次回はまた敵Lの行動、と繰り返します。
(defun move-enemy-on-intersection-J (enemy)
  (if (not (pacman-char-state enemy))
      (move-enemy-on-intersection-L enemy)
    (move-enemy-on-intersection-R enemy))
  (setf (pacman-char-state enemy) (not (pacman-char-state enemy))))


(defun move-if (fn enemy absolute-directions)
  (move-char enemy (find-if fn (find-moveable-directions enemy absolute-directions))))

(defun move-first-space (enemy absolute-directions)
  (move-char enemy (car (find-moveable-directions enemy absolute-directions))))

(defun find-moveable-directions (char &optional (absolute-directions (list :up :right :down :left)))
  (remove-if-not #'(lambda (direction)
                     (can-move-p char direction))
                 absolute-directions))

(defun move-first-space-relative (enemy relative-directions)
  (let ((absolute-directions
         (mapcar #'(lambda (rel-dir)
                     (direction-relative->absolute rel-dir (pacman-char-direction enemy)))
                 relative-directions)))
    (move-char enemy (car (find-moveable-directions enemy absolute-directions)))))

(defun direction-relative->absolute (relative-direction current-direction)
  (let* ((directions (list :up :right :down :left))
         (n (position current-direction directions))
         (pos (position relative-direction directions)))
    (nth pos (rotate-list-left directions n))))

(defun rotate-list-left (list n)
   (if (zerop n)
       list
     (rotate-list-left (append (rest list) (list (first list))) (- n 1))))



;;; ステージ管理

;; フィールド中の進入可能マスは、その 4 方向に隣り合うマスのうち進入可能なものの数に応じて、
;; 行き止まりマス、通路マス、交差点マスの 3 種類に分類されます。
;; 1 方向ある場合が行き止まりマス、2 方向ある場合が通路マス、3,4 方向ある場合が交差点マスとなります。

(defun stage-1-way-cell-p (pos)
  (= (stage-count-way pos) 1))

(defun stage-2-way-cell-p (pos)
  (= (stage-count-way pos) 2))

(defun stage-3-way-cell-p (pos)
  (= (stage-count-way pos) 3))

(defun stage-4-way-cell-p (pos)
  (= (stage-count-way pos) 4))

(defun stage-intersection-p (pos)
  (or (stage-3-way-cell-p pos)
      (stage-4-way-cell-p pos)))

(defun stage-count-way (pos)
  (let ((x (pos-x pos))
        (y (pos-y pos)))
    (count-if-not #'wall-p (list (make-pos (+ x 0) (+ y 1)) ; up
                                 (make-pos (+ x 1) (+ y 0)) ; right
                                 (make-pos (- x 1) (+ y 0)) ; left
                                 (make-pos (+ x 0) (- y 1)) ; down
                                 ))))

;; . はそのマスにドットが置かれていることを表します。
;; ドットでない進入可能なマス、すなわち自機と敵の初期位置および空きマスにはドットは置かれません。
(defun count-remaining-dot ()
  (count *pacman-stage-dot-char* *pacman-stage*))

(defun eat-dot ()
  (multiple-value-bind (_ pos)
      (find-char #\@)
    (set-stage-at pos #\SPC)))

(defun find-nearest-dot ()
  (car (sort-dot-by-distance (pacman-player-char)
                             (find-dot-list))))

(defun sort-dot-by-distance (player dot-list)
  (flet ((euclid-distance (p1 p2)
           (sqrt (apply '+ (mapcar #'(lambda (a b)
                                       (expt (- a b) 2))
                                   p1 p2)))))
    (when dot-list
      (let ((player-pos (pacman-char-pos player)))
        (sort dot-list
              #'(lambda (dot-a dot-b)
                  (< (euclid-distance dot-a player-pos)
                     (euclid-distance dot-b player-pos)))
              )))))

(defun find-dot-list ()
  (loop for cell across *pacman-stage* and i = 0 then (1+ i)
    when (char= cell *pacman-stage-dot-char*)
    collect (stage-index->pos i)))

;; # は壁を表します。そのマスには自機も敵も進入することができません。
;; 逆に壁以外のマスにはすべて進入可能です。
(defun can-move-p (char-designator direction)
  (multiple-value-bind (char next-pos pos)
      (char-next-pos char-designator direction)
    (not (wall-p next-pos))))

(defun wall-p (pos)
  (char= (stage-at pos) #\#))


;;; ステージの描画

(defparameter *stage-message* nil)
(defun stage-message (fmt &rest args)
  (setf *stage-message* (apply 'format nil fmt args))
  (render-stage)
  (setf *stage-message* nil))

;; 自機の動き方を表す文字列を出力してください。
;; 文字列は t = 0, 1, ..., T-1 における自機の行動を表す文字を連結したものです。
;;
;; h	左方向へ移動
;; j	下方向へ移動
;; k	上方向へ移動
;; l	右方向へ移動
;; .	その場に留まる

(defun render-stage ()
  (erase-buffer (selected-buffer))
  (let ((answer (pacman-answer)))
    (puts "~A" (stage-view))
    (puts "")
    (puts "remaining time: ~A, remaining dot ~A" (current-time-remaining) (count-remaining-dot))
    (puts "message: ~A" (or *stage-message* ""))
    (puts "current answer: ~A" answer)
    (when *pacman-auto-play-p*
      (puts "auto play count: ~A" *pacman-auto-play-count*)
      (puts "auto high score: ~S" *pacman-auto-play-high-score*))
    (save-excursion
      (puts "game clear answers:")
      (puts "~{  ~A~%~}" *pacman-game-clear-pattern-list*)
      (when *pacman-debug*
        (puts "")
        (puts "game over pattern:")
        (dolist (bad *pacman-game-over-pattern-list*)
          (when (and (<= (length answer) (length bad))
                     (string= answer bad :end2 (length answer)))
            (puts "  ~A~%" bad)))
        (dolist (sym *pacman-debug-symbols*)
          (let ((v (ignore-errors (symbol-value sym))))
            (puts "")
            (puts "~S:" sym)
            (cond ((null v)
                   (puts "  nil"))
                  ((listp v)
                   (puts "~{  ~S~%~}" v))
                  (t
                   (puts "  ~S" v)))))
        (puts "")))))

(defun pacman-answer ()
  (format nil "~{~A~}" *pacman-move-history*))

(defun stage-view ()
  (flet ((render-char (stage char)
           (set-stage-at (pacman-char-pos char)
                         (pacman-char-kind char)
                         stage)))
    (let ((view (copy-stage *pacman-stage*)))
      (dolist (char (pacman-enemy-char-list))
        (render-char view char))
      (render-char view (pacman-player-char))
      view)))

(defun puts (fmt &rest args)
  (let ((msg (apply 'format nil fmt args)))
    (insert msg)
    (when (or (string= msg "")
              (not (char= (char msg (- (length msg) 1)) #\LFD)))
      (insert #\LFD))))

(defparameter *direction-definition*
  '((:left  #\h -1  0)
    (:down  #\j  0  1)
    (:up    #\k  0 -1)
    (:right #\l  1  0)
    (:stay  #\.  0  0)
    ))

(defun direction-char>direction (direction-char)
  (let ((def (find direction-char *direction-definition* :key #'cadr)))
    (car def)))

(defun direction->direction-char (direction)
  (let ((def (find direction *direction-definition* :key #'car)))
    (cadr def)))

(defun direction->diff (direction)
  (let ((def (find direction *direction-definition* :key #'car)))
    (values-list (cddr def))))

(defun char-next-pos (char-designator direction)
  (multiple-value-bind (dx dy)
      (direction->diff direction)
    (multiple-value-bind (char pos)
        (find-char char-designator)
      (let ((x (pos-x pos))
            (y (pos-y pos)))
        (values char
                (make-pos (+ x dx) (+ y dy))
                (make-pos x y))))))

(defun find-char (char-designator)
  (let ((char (if (pacman-char-p char-designator)
                  char-designator
                (find-if #'(lambda (char)
                             (or (equal (pacman-char-id char) char-designator)
                                 (equal (pacman-char-kind char) char-designator)))
                         *pacman-char-list*))))
    (unless char
      (error "Char not found: ~S" char-designator))
    (values char (pacman-char-pos char))))

;; 各セルの位置は左上を (1, 1), 右下を (W, H) とする座標で表されます。
(defun relative-position (char1 char2)
  (relative-position-list (pacman-char-pos char1) (pacman-char-pos char2)))

(defun relative-position-list (pos1 pos2)
  (values-list (mapcar #'- pos1 pos2)))

(defun stage-at (pos &optional (stage *pacman-stage*))
  (elt stage (pos->stage-index pos)))

(defun set-stage-at (pos value &optional (stage *pacman-stage*))
  (setf (elt stage (pos->stage-index pos)) value))

(defun pos->stage-index (pos)
  (+ (* (pos-y pos) (1+ *pacman-stage-width*))
     (pos-x pos)))

(defun stage-index->pos (index)
  (multiple-value-bind (y x)
      (floor index (1+ *pacman-stage-width*))
    (make-pos x y)))


;;; utility

(defun copy-stage (stage)
  (copy-seq stage))

(defun copy-char-list (char-list)
  (mapcar #'copy-pacman-char char-list))


(provide "pacman")

;;; End

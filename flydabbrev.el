;;; flydabbrev.el --- on-the-fly dabbrev previews

;; Keywords: dabbrev on-the-fly convenience

;; Author   : Jakob Sievers <j@svirfneblin.org>
;; Created  : Early 2006
;; Modified : $Date$

;;; Commentary:

;; Flydabbrev displays the completion which dabbrev would insert if you were to
;; hit Meta-Slash while you type.
;;
;; Pros:
;;   o It's really simple
;; Cons
;;   o It defines advice
;;   o It doesn't work with all modes

;; See flydabbrev-mode's docstring for an example and some notes.
;; Flydabbrev is non-intrusive: Standard dabbrev commands still work
;; (there are, however, two pieces of advice which flydabbrev defines,
;; one for dabbrev-expand, and one for dabbrev-completion; they set
;; dabbrev-case-fold-search, dabbrev-case-replace, and
;; dabbrev-upcase-means-case-search to values which make sense for
;; flydabbrev).

;;; Code:

(require 'dabbrev)
(eval-when-compile
  (require 'cl))

(defconst flydabbrev-version "1.0")
(defconst flydabbrev-author  "j@svirfneblin.org")

;;;_. User options
;; You may want to look at `flydabbrev-triggers' which should contain all
;; single character insertion and deletion commands used by your favorite major
;; modes.

(defgroup flydabbrev nil
  "Flydabbrev configurables."
  :tag "On-the-fly dynamic abbreviations."
  :group 'abbrev
  :group 'convenience)

(defcustom flydabbrev-sit-time 0.1
  "*Flydabbrev sit-for's this many seconds before displaying a completion."
  :type  'number
  :group 'flydabbrev)

(defcustom flydabbrev-min-chars 3
  "*`dabbrev-expand' is first called for the current word on the
flydabbrev-min-chars'th character typed."
  :type  'number
  :group 'flydabbrev)

(defcustom flydabbrev-min-completion-length 3
  "*Only completions which add at least this many characters to the current
 word are displayed."
   :type  'number
   :group 'flydabbrev)

(defcustom flydabbrev-show-completion-list 3
  "*Number of future completions to include in a minibuffer message."
  :type  'number
  :group 'flydabbrev)

(defface flydabbrev-highlight-face
  `((((type tty) (class color))
     (:inverse-video t))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:underline t))
    (((class color) (background light))
     (:underline t))
    (t
     (:underline t)))
  "*Face used to highlight inserted completions."
       :group 'flydabbrev)

(defcustom flydabbrev-triggers
  '(self-insert-command
    delete-backward-char
    backward-delete-char-untabify
    c-electric-backspace
    paredit-backward-delete
    prolog-electric-delete)
  "*Commands which trigger the insertion of a completion."
  :group 'flydabbrev)

;;;_. Internal variables
;; These are used mostly to keep track of some internal state and in one case
;; (`flydabbrev--messaging') to control the behavior of `message' via advice
;; (see next section).

(defvar flydabbrev--messaging t
  "Used to control whether or not messages are sent to the minibuffer.")
(make-variable-buffer-local 'flydabbrev--messaging)

(defvar flydabbrev--overlay nil
  "Overlay used to highlight the current completion.")
(make-variable-buffer-local 'flydabbrev--overlay)

(defvar flydabbrev--currently-completing-p nil
  "Set when a completion has been inserted.")
(make-variable-buffer-local 'flydabbrev--currently-completing-p)

(defvar flydabbrev--cursor-position nil
  "Remembers the cursor position.")
(make-variable-buffer-local 'flydabbrev--cursor-position)

(defvar flydabbrev--begin-marker nil
  "Marks the beginning of an inserted completion.")
(make-variable-buffer-local 'flydabbrev--begin-marker)

(defvar flydabbrev--end-marker nil
  "Marks the end of an inserted completion.")
(make-variable-buffer-local 'flydabbrev--end-marker)

;;;_. Advice
;; Flydabbrev advises message to inhibit cluttering of the minibuffer, and the
;; two dabbrev procedures dabbrev-expand and dabbrev-completion to force
;; certain configuration variables to values which make sense for flydabbrev
;; (mostly these have to do with case conversion and searching).

(defadvice message (around msgoff activate)
  "Controls minibuffer messaging by wrapping `message' in a when-form.
Messaging can be turned on/off by setting flydabbrev--messaging."
  (if flydabbrev-mode
      (when flydabbrev--messaging
	ad-do-it)
    ad-do-it))

(defadvice dabbrev-expand (around expopts activate)
  "Wraps `dabbrev-expand' in a let-form which sets case related dabbrev options
to values appropriate for flydabbrev."
    (if flydabbrev-mode
	(let ((dabbrev-case-fold-search t)
	      (dabbrev-case-replace     t)
	      (dabbrev-upcase-means-case-search
               (if (flydabbrev--all-caps-p) nil t)))
	  ad-do-it)
      ad-do-it))

(defadvice dabbrev-completion (around complopts activate)
    "Wraps `dabbrev-completion' in a let-form which sets case related dabbrev
options to values appropriate for flydabbrev."
    (if flydabbrev-mode
	(let ((dabbrev-case-fold-search t)
	      (dabbrev-case-replace     t)
	      (dabbrev-upcase-means-case-search
               (if (flydabbrev--all-caps-p) nil t)))
	  ad-do-it)
      ad-do-it))

;;;_. Minor mode setup and initialization
(defvar flydabbrev-mode nil
  (concat "FLYDABBREV -- A minor mode for slow typists.\n"
"You are using: flydabbrev version " flydabbrev-version "\n"
"Please send bug reports & feature request to: " flydabbrev-author
"

NOTES
o `Flydabbrev--insert-cc' (flydabbrev's insertion primitive) is called by a
  function added to the `after-change-functions' Emacs hook.
o Completions which aren't accepted are removed by a function added to the
  `pre-command-hook' Emacs hook.
o To accept the current completion, hit whatever `dabbrev-expand' is bound to
  (M-/ by default).
o If `flydabbrev-min-completion-length' is > 0, it indicates the minimum number of
  characters a completion has to add to the current word for flydabbrev to
  display it.
o If `flydabbrev-sit-time' is > 0, flydabbrev `sit-for's that many seconds
  (default 0.1) before inserting a completion.
o Performance considerations:
  If you are experiencing poor performance...
   - Byte compile flydabbrev.el
   - Increase the value of `flydabbrev-min-chars'
   - Increase the value of `flydabbrev-sit-time'
"))
(make-variable-buffer-local 'flydabbrev-mode)

(defun flydabbrev-mode (&optional arg)
  "Flydabbrev minor mode."
  (interactive "P")
  ;; Set mode on/off
  (setq flydabbrev-mode
	(if (null arg)
	    (not flydabbrev-mode)
	  (> (prefix-numeric-argument) 0)))
  ;; Call on/off functions
  (if flydabbrev-mode
      (flydabbrev-on)
    (flydabbrev-off))
  ;; Force modeline redisplay
  (set-buffer-modified-p (buffer-modified-p)))

(defun flydabbrev-on ()
  "Do initialization for flydabbrev mode."
  ;; Initialize overlay.
  (setq flydabbrev--overlay (make-overlay 1 1))
  (overlay-put flydabbrev--overlay 'face 'flydabbrev-highlight-face)
  (delete-overlay flydabbrev--overlay)
  ;; Install hooks.
  (add-hook 'after-change-functions
            'flydabbrev--after-change-functions-function
            nil
            t)
  (add-hook 'pre-command-hook
            'flydabbrev--pre-command-hook-function
            nil
            t)
  ;; Update modeline.
  (if (not (assq 'flydabbrev-mode minor-mode-alist))
      (setq minor-mode-alist
	    (cons '(flydabbrev-mode " Flyd")
		  minor-mode-alist)))
  ;; Welcome message.
  (message (concat "Flydabbrev -- use "
                   (key-description (where-is-internal 'dabbrev-expand
                                                       nil
                                                       'non-ascii))
                   " to accept completions.")))

(defun flydabbrev-off ()
  "Do some clean up."
  ;; Remove hooks
  (remove-hook 'after-change-functions
               'flydabbrev--after-change-functions-function
               t)
  (remove-hook 'pre-command-hook
               'flydabbrev--pre-command-hook-function
               t)
  ;; Kill vars
  (if (overlayp flydabbrev--overlay)
      (delete-overlay flydabbrev--overlay))
  (setq flydabbrev--overlay                nil
        flydabbrev--currently-completing-p nil
        flydabbrev--messaging              t
        flydabbrev--cursor-position        nil
        flydabbrev--begin-marker           nil
        flydabbrev--end-marker             nil)
  ;; Goodbye message
  (message "Flydabbrev mode terminated."))

(defun turn-on-flydabbrev ()
  "Turn on `flydabbrev-mode' if not on already. For use in mode hooks."
  (unless flydabbrev-mode
    (flydabbrev-mode)))

;;;_. Utility procedures
;; Some helper predicates and procedures used in the next section.

(defun filter (pred lst)
  "Return the elements of LST for which PRED returns t."
  (let (result)
    (dolist (elt lst (reverse result))
      (when (funcall pred elt)
        (push elt result)))))

(defsubst flydabbrev--beginning-of-word ()
  "Go to just before the current sequence of non-whitespace characters."
  (skip-syntax-backward
   (concat "^ " (char-to-string (char-syntax ?\n)))))

(defun flydabbrev--word-qualifies-p ()
  "Returns t if there are at least `flydabbrev-min-chars' between (point)
and the last instance of whitespace/newline, else nil."
  (save-excursion
    (let ((old-point (point)))
      (flydabbrev--beginning-of-word)
      (if (>= (length (buffer-substring-no-properties (point) old-point))
              flydabbrev-min-chars)
          t
        nil))))

(defun flydabbrev--all-caps-p ()
  "Checks whether all characters before point are upper case or special."
  (save-excursion
    (let ((old-point (point)))
      (flydabbrev--beginning-of-word)
      (if (null (filter (lambda (c) (and (>= c ?a) (<= c ?z)))
                        (string-to-list
                         (buffer-substring-no-properties (point) old-point))))
          t
        nil))))

(defun flydabbrev--escape-message (s)
  "Escapes all occurences of '%' in the argument string (yielding '%%') so that
 message, etc. handle it correctly."
  (apply #'string
         (reduce #'append
                 (mapcar (lambda (c) (if (char-equal c ?\%) '(?\% ?\%) (list c)))
                         (string-to-list s)))))

;;;_. Primitives
;; Basic procedures for interacting with dabbrev, inserting and deleting
;; previously inserted completions, and resetting internal state.

(defun flydabbrev--get-completions ()
  "Calls dabbrev-expand to get the initial completion and the next
`flydabbrev-show-completion-list' future completions. Returns a list containing
two strings: The initial completion ready for insertion, and a string describing
future completions, to be echoed to the minibuffer."
  (let ((buffer-undo-list      t)
	(flydabbrev--messaging nil)
        (completions           nil)
        (i                     0)
        (initial-completion    "")
        (future-completions    "")
        (not-found             nil)
        (current-position      (point))
        (beginning-of-word     (save-excursion
                                 (progn (flydabbrev--beginning-of-word)
                                        (point)))))

    ;; Initial completion
    (dabbrev--reset-global-variables) ;XXX
    (condition-case nil
	(dabbrev-expand nil)
      (error
       (setq not-found t)))
    ;; Save it
    (unless not-found
      (setq initial-completion (buffer-substring-no-properties current-position
                                                               (point))))
    ;; Future completions
    (while (and (not not-found)
 		(< i flydabbrev-show-completion-list))
      ;; Call dabbrev
      (condition-case nil
	  (let ((last-command this-command))
	    (dabbrev-expand nil))
	(error
	 (setq not-found t)))
      ;; Copy completion
      (unless not-found
	(push (buffer-substring-no-properties beginning-of-word (point))
	      completions))
      (incf i))
    (dabbrev--reset-global-variables) ;XXX
    ;; Remove last completion
    (delete-region current-position (point))

    ;; Build string
    (dolist (elt completions)
      (setq future-completions (concat elt " " future-completions)))

    (list initial-completion
	  future-completions)))

(defun flydabbrev--insert-cc ()
  "Calls `flydabbrev--get-completions' and inserts the initial completion.
Updates `flydabbrev--overlay', the flydabbrev markers,
`flydabbrev--cursor-position' and `flydabbrev--currently-completing-p'."
  (let ((buffer-undo-list t)
        (current-position (point))
        (len              nil)
        (not-found        nil))
    ;; Get completions
    (destructuring-bind (initial-completion future-completions)
        (flydabbrev--get-completions)
      ;; If there is at least one completion, insert it.
      (if (string= initial-completion "")
          (setq not-found t)
        (insert initial-completion))
      ;; Markers, overlay, etc.
      (setq len (- (point) current-position))
      (move-overlay flydabbrev--overlay current-position (point))
      (setq flydabbrev--end-marker (point-marker))
      (goto-char current-position)
      (setq flydabbrev--begin-marker (point-marker))
      (setq flydabbrev--cursor-position current-position)
      ;; Echo next completions
      (unless (string= future-completions "")
        (message (flydabbrev--escape-message future-completions)))
      ;; Remove completions that are too short
      (when (and (not not-found)
                 (< len flydabbrev-min-completion-length))
        (delete-region (marker-position flydabbrev--begin-marker)
                       (marker-position flydabbrev--end-marker))
        (setq not-found t)
        (flydabbrev--reset))
      ;; Set flydabbrev--currently-completing-p
      (if not-found
          (setq flydabbrev--currently-completing-p nil)
        (setq flydabbrev--currently-completing-p t)))))

(defun flydabbrev--cancel-cc (caller)
  "Disables some hooks and undo then removes the current completion.
Checks integrity via `flydabbrev--cursor-position', sets
`flydabbrev--currently-completing-p'."
  (let ((buffer-undo-list t)
        (beg (marker-position flydabbrev--begin-marker))
        (end (marker-position flydabbrev--end-marker)))
    ;; Delete inserted completion
    (cond
     ;; Called by pre-command-hook-function, (point) shouldn't have changed.
     ((eq caller 'pre)
      (if (not (= (point) flydabbrev--cursor-position))
          (message "FLDBBRV ERR 003")
        (delete-region beg end)))
     ;; Called by after-change-hook-function, single char insertion:
     ;; (point) should be one more than old point.
     ((eq caller 'ins)
      (if (not (= (point) (+ flydabbrev--cursor-position 1)))
          (message "FLDBBRV ERR 004")
        (delete-region (+ beg 1) end)))
     ;; Called by after-change-hook-function, single char deletion:
     ;; (point) should be one less than old point.
     ((eq caller 'del)
      (if (and (not abbrev-mode) ; kludge
               (not (= (point) (- flydabbrev--cursor-position 1))))
          (message "FLDBBRV ERR 005")
        (delete-region beg end))))
    ;; Clear state
    (flydabbrev--reset)))

(defsubst flydabbrev--reset ()
  "Clear flags, overlay, markers."
  (progn
    (delete-overlay flydabbrev--overlay)
    (setq flydabbrev--currently-completing-p nil
          flydabbrev--cursor-position        nil
          flydabbrev--begin-marker           nil
          flydabbrev--end-marker             nil)))

;;;_. Hooks
;; The hook procedures are used to actually insert new completions and remove
;; old ones.

(defun flydabbrev--pre-command-hook-function ()
  "`Pre-command-hook' function. If the current command is a single keystroke
editing command, it is ignored. Every other command is executed after first
cancelling any previously inserted completions."
  (condition-case nil
      (cond
       ((memq this-command flydabbrev-triggers)
	;; IGNORE
	)
       (t
	(when flydabbrev--currently-completing-p
          (flydabbrev--cancel-cc 'pre))))
    (error (message "Error in pre-command-hook-function -- Flydabbrev"))))

(defun flydabbrev--after-change-functions-function (start end len)
  "Added to `after-change-functions'.
If the change that triggered it was caused by a single-character editing
command, flydabbrev--after-change-functions-function attempts to insert the
current completion by calling `dabbrev-expand'."
  (condition-case nil
      (when (member this-command flydabbrev-triggers)
        (cond
         ;; No prior completion
         ((and (not flydabbrev--currently-completing-p)
               (or (looking-at "\\>")
                   (looking-at "$"))
               (flydabbrev--word-qualifies-p)
               (sit-for flydabbrev-sit-time))
          (flydabbrev--insert-cc))
         ;; We are completing
         (flydabbrev--currently-completing-p
          (if (flydabbrev--word-qualifies-p)
              (progn
                (if (zerop len)
                    (flydabbrev--cancel-cc 'ins)
                  (flydabbrev--cancel-cc 'del))
                (when (sit-for flydabbrev-sit-time)
                  (flydabbrev--insert-cc)))
            (if (zerop len)
                (flydabbrev--cancel-cc 'ins) ; shouldn't happen
              (flydabbrev--cancel-cc 'del))))))
    (error (message "Error in after-change-functions-function -- FLD"))))

(provide 'flydabbrev)

;;; flydabbrev.el ends here

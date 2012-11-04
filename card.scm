;;; card --- query White Wolf's spoiler files by name
;;;
;;; `card' may be used in two ways:
;;; as a command-line replacement for monger.vekn.org and similar tools
;;; (it doesn't support querying on anything other than name yet, but
;;; that's practically guaranteed to be added Real Soon Now)
;;;
;;; $ ./card kine r
;;; Kine Resources Contested [Jyhad:C, VTES:C, CE:C/PTo2, Anarchs:PAB3,
;;;  BH:PM5, LoB:PG3, Third:PB6, KoT:C/PT4/PV6]
;;; Type: Political Action
;;; Allocate 4 points among two or more Methuselahs. Successful referendum
;;; means each Methuselah burns 1 pool for each point assigned.
;;;
;;; to grind deck files for posting to rec.games.trading-cards.jyhad
;;;
;;; $ ./card -f foo.d > foo.txt
;;;

;;; TODO: fix `, the' bug

(use
 srfi-1 srfi-13 regex format-modular procedure-decoration tool csv gdbm test)

(define *prefix* "/home/j/.card/")

;;;_. main()
(define (main)
  (define-option #\f "" file)

  (tool-main
   (command-line-arguments)
   (lambda (args)
     (let ((db (init)))
       (condition-case
        (if (not file)
            (if (null? args)
                (usage)
                (print-cards args db))
            (if (not (null? args))
                (usage)
                (print-file file db)))
        ((exn) (usage)))
       (gdbm-close db)))))

(define (usage)
  (die "usage: card name ...
       card -f file"))

(define (die . args)
  (apply format (cons (current-error-port) args))
  (newline (current-error-port))
  (exit 1))

;;;_. DB
(define *crypt-file*         (string-append *prefix* "vtescrypt.csv"))
(define *library-file*       (string-append *prefix* "vteslib.csv"))
(define *db-file*            (string-append *prefix* "cardlist.gdbm"))
(define *crypt-header-key*   "__crypt-header__")
(define *library-header-key* "__library-header__")

(define (init)
  (let ((db (if (file-exists? *db-file*)
                (gdbm-open *db-file*)
                (make-db))))
    (make-accessors db)
    db))

;; We use a single name-to-card table
(define (make-db)
  (let* ((crypt   (make-csv-reader (open-input-file *crypt-file*)))
         (library (make-csv-reader (open-input-file *library-file*)))
         (db      (gdbm-open *db-file*))
         (store-card
          (lambda (card)
            (gdbm-store db
                        (make-key card)
                        (pickle (map maybe-string->number card))
                        GDBM_INSERT))))
    (gdbm-store db *crypt-header-key*   (pickle (crypt))   GDBM_INSERT)
    (gdbm-store db *library-header-key* (pickle (library)) GDBM_INSERT)
    (csv-for-each store-card crypt)
    (csv-for-each store-card library)
    db))

;; Cards are lists of fields
(define (make-key card)
  (string-append
   (car card)
   (if (and (vamp? card) (field-set? (cadddr card))) ;XXX
       ": Advanced"
       "")))

(define (vamp? card)
  (or (field=? (cadr card) "Vampire")
      (field=? (cadr card) "Imbued")))

(define (card-project card fields)
  ;; returns FIELDS of CARD
  (reverse
   (fold (lambda (field acc) (cons (field card) acc))
         '()
         fields)))

(define (group-by field cards)
  ;; returns an alist of
  ;;  ( FIELD-value . subset-of-CARDS-with-that-value )
  ;; pairs. The list is sorted according to field<? on FIELD-value.
  (let ((collect-values
         (lambda (field cards)
           (sort (delete-duplicates (map field cards)) field<?))))
    (reverse
     (fold (lambda (field-value acc)
             (cons (cons field-value
                         (filter (lambda (card) (field=? (field card) field-value))
                                 cards))
                   acc))
           '()
           (collect-values field cards)))))

;; Fields are integers or strings
(define (maybe-string->number str)
  (let ((num (string->number str)))
    (if num num str)))

(define (field<? f1 f2)
  (if (integer? f1) (< f1 f2) (string<? f1 f2)))

(define (field=? f1 f2)
  (if (integer? f1) (= f1 f2) (string=? f1 f2)))

(define (field-length field)
  (if (integer? field) 2 (string-length field)))

(define (field-set? field)
  (if (integer? field) #t (not (string=? field ""))))

;; dbm(3)
(define (pickle sexp)
  (with-output-to-string (lambda () (write sexp))))

(define (unpickle str)
  (with-input-from-string str read))

;; Add card accessors to the top-level namespace
(define (make-accessors db)
  (let ((crypt-header   (car (fetch db *crypt-header-key*)))
        (library-header (car (fetch db *library-header-key*))))
    (parse-header crypt-header   'crypt)
    (parse-header library-header 'library)
    (define-accessors (delete-duplicates
                       (append crypt-header library-header)))))

(define (parse-header header tag)
  (for-each (match-lambda
             ((idx name)
              (put! tag (name->internal name) (lambda (card) (list-ref card idx)))))
   (zip (iota (length header)) header)))

(define (define-accessors fields)
  (for-each
   (lambda (field)
     (let ((sym (name->internal field)))
       (eval `(define (,sym card)
                (if (vamp? card)
                    ((get 'crypt   ',sym) card)
                    ((get 'library ',sym) card))))
       (eval `(set! (procedure-name ,sym) ,field))))
   fields))

(define (name->internal field-name)
  (string->symbol (string-substitute " " "-" (string-downcase field-name))))

(define-procedure-extender name procedure-name named-procedure?)

;; DB access
(define fetch
  (let ((keys '()))
    (lambda (db card-name)
      (if (null? keys)
          ;; More efficient variant of gdbm-fold
          (let loop ((key (gdbm-first-key db)) (acc '()))
            (if (not key)
                acc
                (begin (set! keys (cons key keys))
                       (loop (gdbm-next-key db key)
                             (if (substring-index-ci card-name key)
                                 (cons (unpickle (gdbm-fetch db key)) acc)
                                 acc)))))
          ;; Use cached list of keys
          (map (lambda (key) (unpickle (gdbm-fetch db key)))
               (filter (lambda (key) (substring-index-ci card-name key)) keys))))))

;;;_. Card lookup
(define (print-cards name db)
  ;; NAME is a list of name components, not a list of names
  (for-each print-card (fetch db (string-intersperse name))))

(define (print-card card)
  (format #t "~A [~{~A~#[~:;~% ~]~}]~%"
          (name card)
          (string-wrap (set card) (- 72 (string-length (name card)) 3)))
  (for-each (maybe-print-field card) (desired-fields card))
  (when (field-set? (card-text card))
    (for-each print (string-wrap (card-text card) 72))
    (newline)))

(define ((maybe-print-field card) field)
  (when (field-set? (field card))
    (format #t "~A: ~A~%"
            (procedure-name field) (field card))))

(define (desired-fields card)
  (if (vamp? card)
      (list clan group capacity disciplines)
      (list type clan pool-cost blood-cost conviction-cost burn-option)))

;; Wrap long lines
(define (string-wrap str chars-per-line)
  ;; This won't terminate if the input string contains a single word
  ;; with >= chars-per-line characters
  (append-map (lambda (str)
                (receive (chunk rest)
                    (string-wrap-once str chars-per-line)
                  (append (list chunk) (string-wrap rest chars-per-line))))
              (string-split str "\n")))

(define (string-wrap-once str chars-per-line)
  (if (<= (string-length str) chars-per-line)
      (values str "")
      (receive (line rest)
          (split-at (string->list str) chars-per-line)
        (receive (final-word line-up-to-final-blank)
            (break char-whitespace? (reverse line))
          (values
           (list->string
            (reverse (drop-while char-whitespace? line-up-to-final-blank)))
           (list->string
            (append (reverse final-word) rest)))))))

;;;_. Deck printer
(define (print-file file db)
  (print-deck (parse-file file db)))

;; Local conventions:

;; PARSE-FILE returns a deck
(define deck-name    first)
(define deck-version second)
(define deck-cards   third)

;; Cards get an extra field
(define quantity last)

(define (count cards)
  (sum (map quantity cards)))

;;;_ , File I/O and parsing
(define (parse-file file db)
  (let* ((lines   (drop-comments (drop-blank-lines (read-lines file))))
         (name    (first lines))
         (version (last lines))
         (entries (cdr (reverse (cdr (reverse lines)))))
         (cards   (entries->cards entries db)))
    (list name version cards))) ;the deck

(define (drop-blank-lines lines) (drop "[ ]*"   lines))
(define (drop-comments lines)    (drop "^// .*" lines))

(define (entries->cards entries db)
  (map (lambda (entry)
         (let ((parsed (string-match "(^[ ]*[0-9]+ )(.*)" entry)))
           (if (not parsed)
               (die "Bad line: ~A" entry)
               (entry->card (string->number (string-trim-both (cadr parsed)))
                            (string-trim-both (caddr parsed))
                            db))))
       entries))

(define (entry->card quantity name db)
  (let* ((name  (expand name))
         (cards (fetch db name)))
    (if (null? cards)
        (die "No such card: ~A" name)
        (append (best-match cards name) (list quantity))))) ;new field

(define (best-match cards input)
  ;; We use the shortest possible match of which the input is a prefix.
  (car (sort (filter (lambda (c) (string-prefix-ci? input (make-key c)))
                     cards)
             (lambda (c1 c2) (field<? (make-key c1) (make-key c2))))))

;;;_ , Print deck to stdout
(define (print-deck deck)
  (format #t "~A~%~%" (deck-name deck))

  (receive (crypt library)
      (partition vamp? (deck-cards deck))

    (let* ((crypt-stats (compute-crypt-statistics crypt))
           (lib-stats   (compute-library-statistics library)))

      (format
       #t
       "CRYPT [~A cards]~%~%Min draw: ~A~%Max draw: ~A~%Average capacity: ~,2F~%~%"
       (count crypt) (first crypt-stats) (second crypt-stats) (third crypt-stats))

      (for-each print (format-crypt-entries crypt (second lib-stats)))

      (format
       #t
       "~%~%LIBRARY [~A cards]~%~%Disciplines: ~{~{~A (~A)~}~#[~:;, ~]~}~%~%"
       (count library) (first lib-stats))

      (for-each print (format-library-entries library))))

  (format #t "( ~A )~%" (deck-version deck)))

(define (compute-crypt-statistics crypt)
  (let* ((capacities
          (lambda (crypt)
            (apply append
                   (map (lambda (card) (make-list (quantity card) (capacity card)))
                        crypt))))
         (caps (capacities crypt)))
    (list (sum (take (sort caps <) 4))      ;min draw
          (sum (take (sort caps >) 4))      ;max draw
          (/ (sum caps) (length caps)))))   ;average capacity

(define (compute-library-statistics library)
  (let* ((discipline-stats
          (map (match-lambda
                ((discipline . cards) (list (compact discipline) (count cards))))
               (group-by discipline library)))
         (disciplines-used
          (delete-duplicates
           (flatten (map (lambda (d) (map string-trim-both (string-split d "/|&")))
                         (map car discipline-stats))))))
    (list discipline-stats disciplines-used)))   ;# of cards used per discipline and a
                                                 ;list of disciplines used

;; Format crypt entries
(define (format-crypt-entries crypt disciplines-used)
  (let* ((vamps         (map (lambda (c) (card-project c
                                                  (crypt-entry-fields
                                                   disciplines-used)))
                             crypt))
         (field-lengths (max-field-lengths vamps))
         (fmt           (apply make-crypt-entry-format-string field-lengths)))
    (map (lambda (vamp) (format fmt vamp)) vamps)))

(define (crypt-entry-fields disciplines-used)
  (list quantity name capacity (used-disciplines disciplines-used) title
        abbreviated-clan group))

;; Hook disciplines field-selector: parse, then filter, then unparse
;; discipline spread so it only lists the disciplines we actually use. UGH!
(define ((used-disciplines disciplines-used) card)
  (string-intersperse
   (filter (lambda (d) (member (string-downcase d) disciplines-used))
           (string-split (disciplines card) " "))))

;; Hook clan field-selector: abbreviate clan name if possible
(define (abbreviated-clan vamp)
  (compact (clan vamp)))

(define (max-field-lengths vamps)
  (map (lambda (i) (apply max (map (lambda (v) (field-length (list-ref v i)))
                              vamps)))
       (iota (length (car vamps)))))

(define (make-crypt-entry-format-string m n c d t cl _g)
  (conc "~{~"m"@Ax ~"n"A  ~"c"@A  ~"(if (> d 0) (+ d 2) d)"A~"(if (> t 0) (+ t 2) t)"A~"cl"A:~A~}"))

;; Format library entries
(define (format-library-entries library)
  (map (match-lambda
        ((type . cards)
         (format "~A [~A]~%~{~{~2@Ax ~A~}~%~}"
                 type (count cards)
                 (map  (lambda (card) (list (quantity card) (name card)))
                       cards))))
       (group-by type library)))

;;;_ , Utilities
(define (sum lst)
  (apply + lst))

(define (drop regex lines)
  (filter (lambda (l) (not (string-match regex l))) lines))

;;;_ , Convenience
(define (expand str)
  (or (front->back str)
      (string-substitute* str *sverbba*)))

(define (compact str)
  (if (string=? str "")
      "none"
      (let ((str (string-substitute* str *abbrevs*)))
        (or (antitribu->! str) str))))

(define (rewrite re transformer)
  (o (lambda (groups) (and groups (transformer (map string-trim-both groups))))
     (lambda (str) (string-match re str))))

(define front->back
  (rewrite "The[ ]+(.*)"
           (match-lambda ((_ name) (string-append name ", The")))))

(define antitribu->!
  (rewrite "(.+) antitribu"
           (match-lambda ((_ clan) (string-append "!" clan)))))

(define *abbrevs*
  '(;; Clans
    ("Ahrimane"                    . "Cat")
    ("Blood Brother"               . "BB")
    ("Daughter of Cacophony"       . "DoC")
    ("Follower of Set"             . "Setite")
    ("Harbinger of Skulls"         . "HoS")
    ("Malkavian"                   . "Malk")
    ("Nosferatu"                   . "Nossie")
    ("True Brujah"                 . "Trujah")
    ;; Disciplines
    ("Abombwe"                     . "abo")
    ("Animalism"                   . "ani")
    ("Auspex"                      . "aus")
    ("Celerity"                    . "cel")
    ("Chimerstry"                  . "chi")
    ("Daimoinon"                   . "dai")
    ("Dementation"                 . "dem")
    ("Dominate"                    . "dom")
    ("Fortitude"                   . "for")
    ("Melpominee"                  . "mel")
    ("Mytherceria"                 . "myt")
    ("Necromancy"                  . "nec")
    ("Obeah"                       . "obe")
    ("Obfuscate"                   . "obf")
    ("Obtenebration"               . "obt")
    ("Potence"                     . "pot")
    ("Presence"                    . "pre")
    ("Protean"                     . "pro")
    ("Quietus"                     . "qui")
    ("Sanguinus"                   . "san")
    ("Serpentis"                   . "ser")
    ("Spiritus"                    . "spi")
    ("Temporis"                    . "tem")
    ("Thanatosis"                  . "thn") ;XXX
    ("Thaumaturgy"                 . "tha")
    ("Valeren"                     . "val")
    ("Vicissitude"                 . "vic")
    ("Visceratika"                 . "vis")
    ;; Virtues
    ("Vengeance"                   . "ven")
    ("Defense"                     . "def")
    ("Innocence"                   . "inn")
    ("Judgment"                    . "jud")
    ("Martyrdom"                   . "mar")
    ("Redemption"                  . "red")
    ("Vision"                      . "vis") ;XXX
    ;; Cards
    ("Bewitching Oration"          . "BO")
    ("Direct Intervention"         . "DI")
    ("Drawing out the Beast"       . "DotB")
    ("Free States Rant"            . "FSR")
    ("Hunting Ground"              . "HG")
    ("Immortal Grapple"            . "IG")
    ("Kine Resources Contested"    . "KRC")
    ("My Enemy's Enemy"            . "MEE")
    ("Minion Tap"                  . "MT")
    ("Powerbase"                   . "PB")
    ("Perfect Clarity"             . "PC")
    ("Praxis Seizure"              . "PS")
    ("Protect Thine Own"           . "PTO")
    ("Return to Innocence"         . "RtI")
    ("Sudden Reversal"             . "SR")
    ("Toreador Grand Ball"         . "TGB")
    ("Tension in the Ranks"        . "TitR")
    ("Temptation of Greater Power" . "ToGP")
    ("Taste of Vitae"              . "ToV")
    ("Torn Signpost"               . "TS")
    ("Telepathic Tracking"         . "TT")
    ("Telepathic Vote Counting"    . "TVC")
    ("Wake with Evening's Freshness" . "WwEF")
    ) )

(define (mapkey fun alist)
  (map (match-lambda ((car . cdr) (cons (fun car) cdr))) alist))

(define (regexify abbrev)
  (string-append "(^"abbrev"\\b)|(\\b"abbrev"$)"))

(define (reverse-pairs pairs)
  (map (compose (flip cons) car+cdr) pairs))

(define *sverbba* (mapkey regexify (reverse-pairs *abbrevs*)))

(eval-when (eval)
 (test "dem/tha/vic" (compact "Dementation/Thaumaturgy/Vicissitude"))
 (test "ani & for" (compact "Animalism & Fortitude"))
 (test "thn" (compact "Thanatosis"))
 (test "!Nossie" (compact "Nosferatu antitribu"))
 (test "Setite" (compact "Follower of Set"))
 (test "Praxis Seizure: Boston" (expand "PS: Boston"))
 (test "Anarch Free Press, The" (expand "The Anarch Free Press"))
 (test "Davis" (expand "Davis"))
 (test "Left for Dead" (expand "Left for Dead"))
 )

;; Do the business
(eval-when (load) (main))

;;;_. Emacs
;;; Local Variables:
;;; allout-layout: t
;;; End:

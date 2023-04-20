#+(or)
(progn
(declaim (optimize (speed 0) (space 0) (debug 3)))
(asdf:load-system "alexandria"))
; EOT, End Of Transmission, according to ASCII
; It's what SBCL uses for EOF
; Guess I'll try it out
(defconstant +eof+ (code-char 4))

(defun peek-chr (strm)
  (declare (type (or boolean stream) strm))
  (peek-char nil strm nil +eof+ nil))

(defun read-chr (strm)
  (declare (type (or boolean stream) strm))
  (read-char strm nil +eof+ nil))

(defun must-read-chr (strm)
  (declare (type (or boolean stream) strm))
  (let ((chr (read-chr strm)))
    (if (char= chr +eof+)
      (error "EOF reached during reading")
      chr)))

(defun number-start-p (chr)
  (declare (type character chr))
  (or
    (digit-char-p chr)
    (char= chr #\-)
    (char= chr #\.)))

(defun number-char-p (chr)
  (declare (type character chr))
  (or
    (number-start-p chr)
    (char= chr #\+)
    (char= chr #\E)
    (char= chr #\e)))


#+(or)
(equal
  (build-string '(#\c #\b #\a))
  "abc")
#+or
(equal (build-string nil) "")

(defun build-string (lst)
  (declare (type list lst))
  (let* ((size (length lst))
         (building (make-string size)))
    (loop for l in lst
          for j from (- size 1) downto 0
          do
          (setf (elt building j) l))
    building))

(defun extract-number (strm chr)
  (declare (type (or boolean stream) strm)
           (type character chr))
  (let ((building nil)
        (last-read chr))
    (loop while (number-char-p last-read)
          do
          (push last-read building)
          (read-chr strm)
          (setf last-read (peek-chr strm)))
    (read-from-string (build-string building) nil nil)))

(defun extract-quoted
  (strm chr quote-char)
  (declare (type (or boolean stream) strm)
           (type character chr)
           (type character quote-char))
  (declare (ignore chr))
  (must-read-chr strm)
  (let ((last-read (must-read-chr strm))
        (building nil))
    (loop while (char/= last-read quote-char)
          do
          (if (char= last-read #\\)
            (progn
              (setf last-read (must-read-chr strm))
              (cond last-read
                    ((char= last-read quote-char)
                     (push quote-char building))
                    ((char= last-read #\\)
                     (push last-read building))
                    ((char= last-read #\/)
                     (push last-read building))
                    ((char= last-read #\b)
                     (push #\Backspace building)
                     ((char= last-read #\f)
                      (push #\Page building))
                     ((char= last-read #\n)
                      (push #\Newline building))
                     ((char= last-read #\r)
                      (push #\Return building))
                     ((char= last-read #\t)
                      (push #\Tab building))
                     ((char= last-read #\u)
                      (push
                        (code-char
                          (let ((build-ordinal (make-string 6)))
                            (setf (elt  build-ordinal 0 #\#))
                            (setf (elt build-ordinal 1 #\X))
                            (setf (elt (build-ordinal 2 (must-read-chr strm))))
                            (setf (elt (build-ordinal 3 (must-read-chr strm))))
                            (setf (elt (build-ordinal 4 (must-read-chr strm))))
                            (setf (elt (build-ordinal 5 (must-read-chr strm))))
                            (read-from-string build-ordinal nil nil)))
                        building))
                     (t (error "Unknown escape char: ~A"
                               last-read)))))
            (push last-read building))
          (setf last-read (must-read-chr strm)))
    (build-string building)))

(defconstant +start-verbatim+ #\|)
(defconstant +start-prose+ #\>)
(defconstant +start-comment+ #\#)

(defun blankspace-p (chr)
  (declare (type character chr))
  (or
    (char= chr #\Tab)
    (char= chr #\Space)
    ))

(defun whitespace-p (chr)
  (declare (type character chr))
    (or
      (char= chr #\Newline)
      (char= chr #\Return)
      (char= chr #\Page)
      (blankspace-p chr)))

(defun sepchar-p (chr)
  (declare (type character chr))
  (or (whitespace-p chr)
      (char= chr #\,)
      (char= chr #\:)))

(defun guarded-sepchar-p (chr)
  (declare (type character chr))
    (unless (null chr)
      (sepchar-p chr)))

(defun guarded-blankspace-p (chr)
  (declare (type character chr))
    (unless (null chr)
      (blankspace-p chr)))

(defun extract-comment (strm chr)
  (declare (type (or boolean stream) strm)
           (type character chr))
  (loop with last-read = (read-chr strm)
        while (and
                (not (char= last-read +eof+))
                (not (char= last-read #\Newline)))
        do
        (setf last-read (read-chr strm))
        finally
        (return last-read)))

(defun extract-sep (strm chr pred)
  (declare (type (or boolean stream) strm)
           (type character chr)
           (type function pred))
  (loop with just-read = nil
        with next = chr
    do
    (cond
      ((char= next +start-comment+)
       (setf just-read (extract-comment strm next)))
      ((funcall pred next)
       (setf just-read (must-read-chr strm)))
      (t
        (return just-read)))
    (setf next (peek-chr strm))))

(defun extract-list-sep (strm chr)
  (declare (type (or boolean stream) strm)
           (type character chr))
  (extract-sep strm chr #'guarded-sepchar-p))

(defun must-extract-list-sep (strm chr)
  (declare (type (or boolean stream) strm)
           (type character chr))
  (let ((last-read (extract-sep strm chr #'guarded-sepchar-p)))
    (when (null last-read)
      (error "separator expected"))
    last-read))


(defun extract-blob-sep (strm chr)
  (declare (type (or boolean stream) strm)
           (type character chr))
  (extract-sep strm chr #'guarded-blankspace-p))
#|
(extract-multiline-blob t #\|)
|one
#and then
    # some of
        #
   | two
#this
|  three

(extract-multiline-blob t #\>)
>one
#and then
    # some of
        #
   > two
#this
>  three

|#

(defun extract-multiline-blob (strm chr)
  "
  Grabs the string after the multiline marker
  and grabs subsequent prefixed strings
  until a non-prefix character, EOF, or two new-lines in a row.
  "
  (declare (type (or boolean stream) strm)
           (type character chr))
  (loop named toplevel
        with last-read = nil
        with next = chr
        with building = nil
        while (and
                (char= next chr)
                (not (char= next #\^))
                (not (char= next +eof+)))
        do
        (setf last-read (read-chr strm))
        (setf next (peek-chr strm))
        (loop named getline
              while (and (not (char= next #\Newline))
                         (not (char= next +eof+)))
              do
              (setf last-read (read-chr strm))
              (push last-read building)
              (setf next (peek-chr strm)))
        (when (char= next +eof+)
          (return-from toplevel
                       (build-string building)))
        (setf last-read (read-chr strm))
        (if (char= chr +start-verbatim+)
          (push last-read building)
          (push #\Space building))
        (let ((sep-result
                (extract-blob-sep strm (peek-chr strm))))
          (when sep-result
            (setf last-read sep-result)))
        (setf next (peek-chr strm))
        finally (progn
                  (unless (or
                            (char= next +eof+)
                            (char= next #\^))
                    (error "Must end multiline blob with a caret"))
                  (when (char= next #\^)
                    (read-chr strm))
                  (return-from toplevel (build-string (cdr building))))))

(defun extract-quoted-blob (strm chr)
  (declare (type (or boolean stream) strm)
           (type character chr))
  (extract-quoted strm chr #\"))

#+(or)
(do
  (string-invertcase "")
  (string-invertcase "A")
  (string-invertcase "a")
  (string-invertcase " ")
  (string-invertcase "my heart's a stereo")
  (string-invertcase "My heart's a stereo"))

(defun string-invertcase
  (str)
  (declare (type string str))
  (let ((operable (remove-if-not #'both-case-p str)))
    (if (= (length operable) 0)
      str
      (let* ((first-upper-case (upper-case-p (elt operable 0)))
             (uneven-case
               (when (> (length operable) 1)
                      (reduce
                        (lambda (c v)
                          (or c v))
                        (map
                          'vector
                          (lambda (x)
                            (not (eql (upper-case-p x) first-upper-case)))
                          (subseq operable 1))))))
        (if uneven-case
          str
          (if first-upper-case
            (string-downcase str)
            (string-upcase str)))))))

;; Variable used to determine the case of a string
;; used for the name of a new symbol upon deserialization.
;;
;; Set to `(readtable-case *readtable*)` by default.
(defparameter *symbol-deserialize-case*
  (readtable-case *readtable*))

;; Variable used to determine the case of a string
;; generated from the name of a symbol upon serialization.
;; Can have the same values as the output of `readtable-case`:
;; `:upcase`, `:downcase`, `:preserve`, and `:invert`,
;; with the same effects.
;;
;; Unlike its cousin, I can't just give this thing a
;; default based on the readtable,
;;
;; and the printer has no notion of this. It just prints whatever case
;; was given originally. In effect, the printer is set to `:preserve` by
;; default.
;;
;; This is a default which is not useful in my case, I think.
;;
;; Therefore, by default, it will be set to `:downcase`, which addresses
;; a much more common need.
(defparameter *symbol-serialize-case*
  :downcase)

(defun
  symbol-string-prep (str case-guide)
  "
  Attempts to prepare a string for interning according to the normal
  ANSI rules for internment of literal symbols.
  "
  (declare (type string str)
           (type keyword case-guide))
  (cond ((eql case-guide :upcase) (string-upcase str))
        ((eql case-guide :downcase) (string-downcase str))
        ((eql case-guide :preserve) str)
        ((eql case-guide :invert) (string-invertcase str))
        (t (string-upcase str))))

(defun
  string-keyword (str &optional (case-guide *symbol-deserialize-case*))
  "
  Creates a keyword from a string.

  Creates a keyword following `(readtable-case *readtable*)` rules.

  If this package's var `*symbol-case*` is `:upcase`, the string is upcased.
  If it is `:downcase`, the string is downcased.
  If it is `:preserve`, the string's case is preserved.
  If it is `:invert`, the string's case is inverted as long as all caseable
  characters are already of uniform case.

  The variable `*symbol-case*` takes as its default the current value of
  `(readtable-case *readtable*)` at package load time.
  "
  (declare (type string str)
           (type keyword case-guide))
  (intern
    (symbol-string-prep str case-guide)
    :KEYWORD))

(defun symbol-string
  (sym &optional (case-guide *symbol-serialize-case*))
  (declare (type symbol sym)
           (type keyword case-guide))
  (cond ((eql sym 't) "true")
        ((eql sym 'nil) "null")
        (t (symbol-string-prep (symbol-name sym) case-guide))))

(defun bareword-start-p (chr)
  (declare (type character chr))
  (or
    (alpha-char-p chr)
    (char= chr #\_)
    (char= chr #\=)
    (char= chr #\>)
    (char= chr #\@)
    (char= chr #\$)
    (char= chr #\%)
    (char= chr #\&)
    (char= chr #\*)
    (char= chr #\+)
    (char= chr #\/)
    (char= chr #\=)))

(defun bareword-middle-p (chr)
  (declare (type character chr))
  (or
    (bareword-start-p chr)
    (digit-char-p chr)
    (char= chr #\<)
    (char= chr #\!)
    (char= chr #\?)
    (char= chr #\.)
    (char= chr #\-)))

(defparameter false nil)

(defun convert-to-property (final-string)
  (declare (type string final-string))
  (cond ((string= final-string "t")
                  t)
        ((string= final-string "nil")
         nil)
        ((string= final-string "true")
         t)
        ((string= final-string "false")
         'false)
        ((string= final-string "null")
         nil)
        (t (string-keyword final-string))))

(defun extract-bare-property (strm chr)
  (declare (type (or boolean stream) strm)
           (type character chr))
  (loop with building = nil
        with last-read = nil
        with next = chr
        while (bareword-middle-p next)
        do
        (setf last-read (read-chr strm))
        (push last-read building)
        (setf next (peek-chr strm))
        finally
        (return
          (convert-to-property (build-string building)))))

(defun extract-quoted-property (strm chr)
  (declare (type (or boolean stream) strm)
           (type character chr))
  (convert-to-property
    (extract-quoted strm chr #\')))

(defun extract-value (strm chr)
  (declare (type (or boolean stream) strm)
           (type character chr))
  (cond
        ((char= chr #\{)
         (extract-hash strm chr))
        ((char= chr #\[)
         (extract-array strm chr))
        ((char= chr #\")
         (extract-quoted-blob strm chr))
        ((char= chr #\')
         (extract-quoted-property strm chr))
        ((or
           (char= chr +start-verbatim+)
           (char= chr +start-prose+))
         (extract-multiline-blob strm chr))
        ((number-start-p chr)
         (extract-number strm chr))
        ((bareword-start-p chr)
         (extract-bare-property strm chr))
        (t (error "Invalid token starting with character `~A`"
                  chr))))

(defun extract-array (strm chr)
  (declare (type (or boolean stream) strm)
           (type character chr)
           (ignore chr))
  (read-chr strm)
  (loop with building = nil
        with found-sep = t
        with last-read = (extract-list-sep strm (peek-chr strm))
        with next = (peek-chr strm)
        while (and
                (not (char= next +eof+))
                (not (char= next #\])))
        do
        (unless found-sep
          (error
            "No separating whitespace found at character `~A`."
            next)
          )
        (push (extract-value strm next) building)
        (setf last-read (extract-list-sep strm (peek-chr strm)))
        (setf found-sep (guarded-sepchar-p last-read))
        (setf next (peek-chr strm))
        finally
        (progn (when (and
                (char= next #\])
                (not (char= next +eof+)))
                  (read-chr strm))
                  (return (reverse building)))))

(defun extract-hash (strm chr)
  (declare (type (or boolean stream) strm)
           (type character chr)
           (ignore chr))
  (read-chr strm)
  (loop with building = nil
        with found-sep = t
        with last-read = (extract-list-sep strm (peek-chr strm))
        with next = (peek-chr strm)
        while (and
                (not (char= next +eof+))
                (not (char= next #\})))
        do
        (unless found-sep
          (error
            "No separating whitespace found at character `~A`."
            next))
        (push (extract-value strm next) building)
        (setf last-read (extract-list-sep strm (peek-chr strm)))
        (setf found-sep (guarded-sepchar-p last-read))
        (setf next (peek-chr strm))
        (unless found-sep
          (error
            (concatenate
              'string
              "While looking for val in plist, failed to find separator "
              "whitespace at character `~A`")
            next))
        (push (extract-value strm next) building)
        (setf last-read (extract-list-sep strm (peek-chr strm)))
        (setf found-sep (guarded-sepchar-p last-read))
        (setf next (peek-chr strm))
        finally (when (char= next #\})
                  (read-chr strm))
                  (return
                  (alexandria:plist-hash-table
                  (reverse building)
                  :test #'equal))))
#|
(parse-from t)
# What now brown cow
{

   the-wind "bullseye"
   the-trees false
   the-sparrows his-eye
   poem
    # I don't know if you can hear me
      |His eyee
    # or if
    # you're even there
      |is on
    # I don't know if you can listen
      |The sparrow
      ^

    # to a gypsy's prayer

   this-should-still-work 15.0
   other
      |And I know
      |He's watching
      |Over me
      ^

   'force push' >I sing
                >because
                >I'm happy
                ^

   "i am mordac" true
   "I am web mistress ming" false
   "you are so wrong" null
    wendover [
            {
            so 1
            much -10
            gambling 100
            but 1000
            also -1000
            apparently 10000
            paramedics -10000
            and 1.01
            |this
            |that
            ^
            >and
            >the
            >other
            ^
            }
            {
            die in
            a fire
            }
            15
            ]
}


(parse-from t)
# do
# comments
# work



|#
(defun parse-from (strm)
  (declare (type (or boolean stream) strm))
  (let ((last-read (extract-list-sep strm (peek-chr strm)))
        (next (peek-chr strm)))
    (extract-value strm next)))



#|
(unprintable-p (code-char #x81))
|#
(defun unprintable-p (chr)
  (declare (type character chr))
  (and
    (not (whitespace-p chr))
    (let ((code (char-code chr)))
      (or
        (< code #x1f)
        (= code #x7f)
        (and
          (>= code #x80)
          (<= code #x9f))
        (and
          (>= code #xd800)
          (<= code #xdfff))
        (= code #xfeff)))))

(defun bmp-p (chr)
  (declare (type character chr))
  (< (char-code chr) #x10000))

(defun to-surrogates (chr)
  (declare (type character chr))
  (let* ((subject #x10E6d)

      (residue (- subject #x10000))
      (higher (prog1 (write (ash residue -10) :base 16) (terpri)))
      (lower (prog1 (write (logand #x003ff residue) :base 16) (terpri))))
      (list
        (+ #xD800 higher))
        (+ #xDc00 lower)))
#|
(inject-quoted
  t
  '(#\ude6d #\\ #\" #\c #\n #\Newline #\Tab #\Return #\c #\a #\b))
(inject-quoted t #\" "asdf
	\\\"blarg")
=>

* (inject-quoted
      t '(#\ude6d #\\ #\" #\c #\n #\Newline #\Tab #\Return #\c #\a #\b))
"\uDE6D\\\"cn\n\t\rcab"
(#\UDE6D #\\ #\" #\c #\n #\Newline #\Tab #\Return #\c #\a #\b)
* (inject-quoted t "asdf
                 ^V
                         \\\"blarg")
"asdf\n\u0016\n\t\\\"blarg"
"asdf
▬
        \\\"blarg"
*



|#

(defparameter *escape-characters*
  '((#\Newline . #\n)
(#\Page . #\f)
(#\Backspace . #\b)
(#\Return . #\r)
(#\Tab . #\t)
(#\\ . #\\)))

(defun inject-quoted (strm blob &optional (quote-char #\"))
  (declare (type (or boolean stream) strm)
           (type string blob)
           (type character quote-char))
  (write-char quote-char strm)
  (map nil (lambda (c)
             (let ((mapped-char (cdr (assoc c *escape-characters*))))
               (if mapped-char
                 (progn
                   (write-char #\\ strm)
                   (write-char mapped-char strm))
                 (cond
                   ((unprintable-p c)
                    (write-char #\\ strm)
                    (write-char #\u strm)
                    (format strm "~4,'0X" (char-code c)))
                   ((char= c quote-char)
                    (write-char #\\ strm)
                    (write-char quote-char strm))
                   (t (write-char c strm))))))
       blob)
  (write-char quote-char strm)
  blob)
#|

(subseq "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." 0 78)
(break-at-spaces 80 "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
(break-at-spaces 40
"http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_e.htm#element doof")
(break-at-spaces 40
                 "")
(break-up-blob 40
"http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_e.htm#element ")
(break-at-spaces 0 "")

|#
(defun blob-prose-break-spot (max-width blob)
  (declare (type (integer 0 1024) max-width)
           (type string blob))
  (let ((break-spot nil))
    (loop for c across blob
          for pos from 0 to (- (length blob) 1)
          do
          (when (and
                  (char= c #\Space)
                  (or
                    (null break-spot)
                    (< pos max-width)))
            (setf break-spot pos)
            )
            (when (and (>= pos max-width)
                       (not (null break-spot)))
              (return break-spot))
          finally
          (return break-spot))))

(defun blob-verbatim-break-spot (max-width blob)
  (declare (type (integer 0 1024) max-width)
           (type string blob)
           (ignore max-width))
  (position #\Newline blob))

(defun break-up-blob (max-width blob &optional (next-spot #'blob-break-spot))
  (declare (type (integer 0 1024) max-width)
           (type string blob)
           (type function next-spot))
  (if (<= (length blob) 0)
    blob
    (loop with consumed = (copy-seq blob)
          with chunks = nil
          for spot = (funcall next-spot max-width consumed)
          while (and
                  (> (length consumed) 0)
                  (not (null spot)))
          do
          (push (subseq consumed 0 spot) chunks)
          (setf consumed
                (if (> (length consumed) (+ 1 spot))
                  (subseq consumed (+ 1 spot))
                  (progn
                    ;; Demonstrate that there should be a trailing newline
                    (push "" chunks)
                    (copy-seq ""))))
          finally
          (progn
            (when (> (length consumed) 0)
              (push consumed chunks))
            (return (reverse chunks))))))

(defun inject-sep (strm indented-at)
  (declare (type (or boolean stream) strm)
           (type (or null (integer 0 1024)) indented-at))
  (if (null indented-at)
    (write-char #\Space strm)
    (progn
      (terpri strm)
      (loop for i from 0 to indented-at
            do
            (write-char #\Space strm)))))

(defun suggest-line-width
  (indented-at
    &key
    (break-min-width 30)
    (doc-width 80))
  (declare
    (type (or null (integer 0 1024)) indented-at)
    (type (integer 0 1024) break-min-width)
    (type (integer 0 1024) doc-width))
  (when (not (null indented-at))
    (min
      (max
        (- doc-width
           ;; Account for the prefix char
           (+ indented-at 1))
        break-min-width)
      doc-width)))

(defun determine-blob-form (blob line-width)
  (declare (type (string blob))
           (type (or null (integer 0 1024)) line-width))
  (if (null line-width)
    :quoted
    (cond
      ((> (count #\Newline blob) 1)
       :verbatim)
      ((> (length blob) line-width)
       :prose)
      (t
        :quoted))))

(defun inject-multiline-blob
  (strm blob indent line-width prefix-char next-spot)
  (declare (type (or boolean stream) strm)
           (type string blob)
           (type string indent)
           (type (integer 0 1024) line-width)
           (type character prefix-char)
           (type function next-spot))
        (loop named line-printer
              with spacious
              for str in (break-up-blob line-width
                                        blob
                                        next-spot)
              do
              (write-char prefix-char strm)
              (write-string str strm)
              (terpri strm)
              (write-string indent strm))
        (write-char #\^))

(defun inject-blob (strm blob indented-at
                         &key
                         line-width-args)
  (declare (type (or boolean stream) strm)
           (type (string blob))
           (type (or null (integer 0 1024)) indented-at)
           (type list line-width-args))
  (let* ((line-suggested-width (apply
                                 #'suggest-line-width
                                 (cons indented-at line-width-args)))
         (dispatch (determine-blob-form blob line-suggested-width)))
    (if (eql dispatch :quoted)
      (inject-quoted
        strm
        blob
        #\")
      (inject-multiline-blob
        strm
        blob
        (make-string indented-at
                     :initial-element #\Space)
        line-suggested-width
        (if (eql dispatch :verbatim)
          +start-verbatim+
          +start-prose+)
        (if (eql dispatch :verbatim)
                 #'blob-verbatim-break-spot
                 #'blob-prose-break-spot)))))

;; The only place where I punt to the printer
(defun inject-number (strm num)
  (declare (type (or boolean stream) strm)
           (type number num))
  (prin1 num strm))

(defun escapable-p (chr quote-char)
  (declare (type character chr)
           (type character quote-char))
  (or
    (find chr (map 'list #'car *escape-characters*))
    (unprintable-p chr)
    (char= chr quote-char)))

(defun inject-property-content (strm prop-content)
  (declare (type character chr)
           (type string prop-content))
  (if (> (count-if (lambda (x)
                     (or
                       (char= x #\Space)
                       (escapable-p x #\')))
                   prop-content) 0)
    (inject-quoted strm prop-content #\')
    (write-string prop-content strm)))
#|
(inject-property t :argyle)
=> [prints `argyle`]
(inject-property t 'terrifying)
=> [throws error, no symbols plz]
(inject-property t 15)
=> [throws error, no numbers plz]
(inject-property t 't)
=> [prints `true`]
(inject-property t :|a b c|)
=> [prints `'a b c'`]

|#
(defun inject-property (strm prop)
  (declare (type character chr)
           (type (or null boolean symbol) prop))
  (typecase prop
    (null (write-string "null" strm))
    (boolean (write-string "true" strm))
    (keyword (inject-property-content strm (symbol-string prop)))
    (symbol (cond ((eql prop 'false)
                   (write-string "false" strm))
                  ((eql prop 't)
                   (write-string "true" strm))
                  ((eql prop  'nil)
                   (write-string "null" strm))
                  (t (error "Writing symbols to PCL is undefined"))))
    (t (error "wrong type of thing given to inject property for thing `~A`."
              prop))))

;; Numbers are hard, punt on this
(defun inject-number (strm num)
  (prin1 num strm))

(defun inject-array (strm seq pretty-indent indented-at)
  (let ((array-indent (when (not (null pretty-indent))
                           (+ indented-at pretty-indent))))
    (write-char #\[ strm)
    (loop for v in seq
          do
          (inject-sep strm array-indent)
          (inject-value strm v pretty-indent array-indent)))
  (inject-sep strm indented-at)
  (write-char #\] strm))

(defun inject-object (strm object pretty-indent indented-at &optional &rest
                           line-width-args)
  (let* ((printable
           (stable-sort
             (alexandria:hash-table-alist object)
             #'string<
             :key
             (lambda (thing)
               (format nil "~A" (car thing)))))
         (object-indent (when (not (null pretty-indent))
                          (+ indented-at pretty-indent))))
    (write-char #\{ strm)
    (loop for (k . v) in printable
          do
          (inject-sep strm object-indent)
          (inject-value strm k pretty-indent object-indent)
          (if
              (and
                (typep v 'string)
                (not
                  (eql
                    :quoted
                    (determine-blob-form
                      v
                      (apply
                        #'suggest-line-width
                        (cons
                          (+ object-indent pretty-indent)
                          line-width-args))))))
(let ((blob-indent (+ object-indent pretty-indent)))
              (inject-sep strm blob-indent)
              (inject-blob strm v blob-indent))
            (progn
              (write-char #\Space strm)
              (inject-value strm v pretty-indent object-indent))))
    (inject-sep strm indented-at)
    (write-char #\} strm)))

(defun inject-value (strm val pretty-indent indented-at)
  (declare (type (or boolean stream) strm)
           (type (or null (integer 0 64)) pretty-indent)
           (type (or null (integer 0 1024) indented-at)))
  (typecase val
    ((or null boolean symbol) (inject-property strm val))
    (number (inject-number strm val))
    (string (inject-blob strm val indented-at))
    (hash-table (inject-object strm val pretty-indent indented-at))
    (sequence (inject-array strm val pretty-indent indented-at))))

(defun generate-to (strm val pretty-indent)
  (declare (type (or boolean stream) strm)
           (type (or null (integer 0 64)) pretty-indent))
  (inject-value strm val pretty-indent 0))


#+(or)
(progn
(declaim (optimize (speed 0) (space 0) (debug 3)))
(asdf:load-system "alexandria"))
; EOT, End Of Transmission, according to ASCII
; It's what SBCL uses for EOF
; Guess I'll try it out
(defconstant +eof+ (code-char 4))

(defun peek-chr (strm) (peek-char nil strm nil +eof+ nil))

(defun read-chr (strm)
  (read-char strm nil +eof+ nil))

(defun must-read-chr (strm)
  (let ((chr (read-chr strm)))
    (if (char= chr +eof+)
      (error "EOF reached during reading")
      chr)))

(defun number-start-p (chr)
  (or
    (digit-char-p chr)
    (char= chr #\-)
    (char= chr #\.)))

(defun number-char-p (chr)
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
  (let* ((size (length lst))
         (building (make-string size)))
    (loop for l in lst
          for j from (- size 1) downto 0
          do
          (setf (elt building j) l))
    building))

(defun extract-number (strm chr)
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
  (or
    (char= chr #\Tab)
    (char= chr #\Space)
    ))

(defun whitespace-p (chr)
    (or
      (char= chr #\Newline)
      (char= chr #\Return)
      (char= chr #\Page)
      (blankspace-p chr)))

(defun sepchar-p (chr)
  (or (whitespace-p chr)
      (char= chr #\,)
      (char= chr #\:)))

(defun guarded-sepchar-p (chr)
    (unless (null chr)
      (sepchar-p chr)))

(defun guarded-blankspace-p (chr)
    (unless (null chr)
      (blankspace-p chr)))

(defun extract-comment (strm chr)
  (loop with last-read = (read-chr strm)
        while (and
                (not (char= last-read +eof+))
                (not (char= last-read #\Newline)))
        do
        (setf last-read (read-chr strm))
        finally
        (return last-read)))

(defun extract-sep (strm chr pred)
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
  (extract-sep strm chr #'guarded-sepchar-p))

(defun must-extract-list-sep (strm chr)
  (let ((last-read (extract-sep strm chr #'guarded-sepchar-p)))
    (when (null last-read)
      (error "separator expected"))
    last-read))


(defun extract-blob-sep (strm chr)
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


(defvar *symbol-case*
  (readtable-case *readtable*))

(defun
  prep-symbol-string (str)
  (cond ((eql *symbol-case* :upcase) (string-upcase str))
        ((eql *symbol-case* :downcase) (string-downcase str))
        ((eql *symbol-case* :preserve) str)
        ((eql *symbol-case* :invert) (string-invertcase str))
        (t (string-upcase str))))

(defun
  string-keyword (str)
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
  (intern
    (prep-symbol-string str)
    :KEYWORD))

(defun bareword-start-p (chr)
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
  (convert-to-property
    (extract-quoted strm chr #\')))

(defun extract-value (strm chr)
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
  (declare (ignore chr))
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
  (declare (ignore chr))
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
  (let ((last-read (extract-list-sep strm (peek-chr strm)))
        (next (peek-chr strm)))
    (extract-value strm next)))



#|
(unprintable-p (code-char #x81))
|#
(defun unprintable-p (chr)
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
  (< (char-code chr) #x10000))

(defun to-surrogates (chr)
  (let* ((subject #x10E6d)

      (residue (- subject #x10000))
      (higher (prog1 (write (ash residue -10) :base 16) (terpri)))
      (lower (prog1 (write (logand #x003ff residue) :base 16) (terpri))))
      (list
        (+ #xD800 higher))
        (+ #xDc00 lower)))
#|
(inject-quoted
  t #\"
  '(#\ude6d #\\ #\" #\c #\n #\Newline #\Tab #\Return #\c #\a #\b))
(inject-quoted t #\" "asdf
	\\\"blarg")
=>

* (inject-quoted
      t #\"
        '(#\ude6d #\\ #\" #\c #\n #\Newline #\Tab #\Return #\c #\a #\b))
"\uDE6D\\\"cn\n\t\rcab"
(#\UDE6D #\\ #\" #\c #\n #\Newline #\Tab #\Return #\c #\a #\b)
* (inject-quoted t #\" "asdf
                 ^V
                         \\\"blarg")
"asdf\n\u0016\n\t\\\"blarg"
"asdf
▬
        \\\"blarg"
*



|#

(defun inject-quoted (strm quote-char blob)
  (write-char quote-char strm)
  (map nil (lambda (c)
             (cond
               ((char= c #\Newline)
                (write-char #\\ strm)
                (write-char #\n strm))
               ((char= c #\Page)
                (write-char #\\ strm)
                (write-char #\f strm))
               ((char= c #\Backspace)
                (write-char #\\ strm)
                (write-char #\b strm))
               ((char= c #\Return)
                (write-char #\\ strm)
                (write-char #\r strm))
               ((char= c #\Tab)
                (write-char #\\ strm)
                (write-char #\t strm))
               ((unprintable-p c)
                (write-char #\\ strm)
                (write-char #\u strm)
                (format strm "~4,'0X" (char-code c)))
               ((char= c #\\)
                (write-char #\\ strm)
                (write-char #\\ strm))
               ((char= c quote-char)
                (write-char #\\ strm)
                (write-char quote-char strm))
               (t
                 (write-char c strm))))
       blob)
  (write-char quote-char strm)
  blob)

(defun print-blob
  (blob &key stream pretty-indent)
  (cond
    ((find #\Newline blob) 



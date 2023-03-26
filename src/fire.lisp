
; EOT, End Of Transmission, according to ASCII
(defconstant +eof+ (code-char 4))

(defun peek-chr (strm) (peek-char nil strm nil +eof+ nil))

(defun read-chr (strm)
  (read-char strm nil +eof+ nil))

(defun must-read-chr (strm)
  (let ((chr (read-chr strm)))
    (if (char= chr +eof+)
      (error "EOF reached during reading")
      chr)))

(defun is-number-char-p (chr)
  (or
    (digit-char-p chr)
    (char= chr #\-)
    (char= chr #\+)
    (char= chr #\.)
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
    (loop while (is-number-char-p last-read)
          do
          (push last-read building)
          (read-chr strm)
          (setf last-read (peek-chr strm)))
    (read-from-string (build-string (print building)) nil nil)))

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
    (print (build-string building))))

(defconstant +start-verbatim+ #\|)
(defconstant +start-prose+ #\>)
(defconstant +start-comment+ #\#)

(defun whitespace-p (chr)
  (or
    (char= chr #\Newline)
    (char= chr #\Return)
    (char= chr #\Page)
    (char= chr #\Tab)
    (char= chr #\Space)
    (char= chr #\,)
    (char= chr #\:)))

(defun extract-comment (strm chr)
  (loop while (not (char= chr #\Newline))
        do
        (setf chr (read-chr strm)))
  nil)

(defun extract-sep (strm chr)
  (loop
    do
    (cond ((char= chr +start-comment+)
           (extract-comment strm chr))
          ((whitespace-p chr)
           (must-read-chr strm))
          (t 
            (return nil)))
    (setf chr (peek-chr strm))))


(defun extract-multiline-string (strm chr)
  (let ((last-read chr))
    (loop with building = nil
          while (and
                  (char= last-read chr)
                  (not (char= last-read +eof+)))
          do
          (setf last-read (read-chr strm))
          (loop while (and (not (char= last-read #\Newline))
                           (not (char= last-read +eof+)))
                do
                (push last-read building)
                (setf last-read (read-chr strm)))
          (if (char= chr +start-verbatim+)
            (push last-read building)
            (push #\Space building))
          (setf last-read (peek-chr strm))
          (extract-sep strm last-read)
          (setf last-read (peek-chr strm)))
          finally (progn
                    (unread-char last-read strm)
                    (build-string building))))

(defun 


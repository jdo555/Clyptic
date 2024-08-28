(defpackage clyptic
  (:use :cl)
  (:export ;; constants
           +alphabet+
           +alphabet-and-numbers+
           +alphabet-with-plus+
           +alphabet-without-j+
           +alphabet-without-j-and-v+
           +alphabet-without-q+
           ;; conditions
           odd-number-of-characters
           invalid-number-of-characters
           invalid-character-in-string
           duplicate-character-in-string
           invalid-subsequence
           invalid-number
           invalid-key
           invalid-argument
           ;; other functions
           make-hash-table-set-from-str
           make-hash-table-set-from-seq
           make-hash-table-set-from-list
           prep-alphabet
           prep-string
           prep-string-by-removal
           remove-characters-from-string
           get-random-letter
           filter-string-by-hash
           key-string
           shift-string
           extend-key
           make-char-pos-hash-table-from-str
           get-alphabet-based-index-list-from-str
           get-order-array-from-alphabet-based-index-list
           disrupt-text
           remove-disruption-from-text
           separate-repeating-characters
           distinguish-by-groups-of-two
           put-in-string
           ;; cipher functions
           adfgvx-cipher-simple
           adfgx-cipher-simple
           affine-cipher
           atbash-cipher
           autokey-cipher
           bifid-cipher
           caesar-cipher
           caesar-cipher-ascii
           columnar-transposition-cipher
           custom-n-by-m-cipher
             adfgvx-cipher
             adfgx-cipher
             polybius-cipher
           four-square-cipher
           one-time-pad-cipher
           playfair-cipher
           polybius-cipher-simple
           rail-fence-cipher
           scytale-cipher
           simple-substitution-cipher
           trifid-cipher
           trithemius-cipher
           two-square-cipher
           vigenere-cipher))
(in-package :clyptic)

;;; generic functions...

(defun make-hash-table-set-from-str (text)
  "Creates and returns a simple hash-table-based set from the string, TEXT."
  (let ((ht (make-hash-table)))
    (dotimes (i (length text))
      (let ((c (char text i)))
        (unless (gethash c ht)
          (setf (gethash c ht) t))))
    ht))

(defun make-hash-table-set-from-seq (seq &key (test 'eql))
  "Creates and returns a simple hash-table-based set from the sequence, SEQ."
  (let ((ht (make-hash-table :test test)))
    (dotimes (i (length seq))
      (let ((x (elt seq i)))
        (unless (gethash x ht)
          (setf (gethash x ht) t))))
    ht))

(defun make-hash-table-set-from-list (list &key (test 'eql))
  "Creates and returns a simple hash-table-based set from the list, LIST."
  (let ((ht (make-hash-table :test test)))
    (dolist (x list)
      (unless (gethash x ht)
        (setf (gethash x ht) t)))
    ht))

;;; globals...

(defparameter +alphabet+ "abcdefghijklmnopqrstuvwxyz")
(defparameter +alphabet-with-plus+ (concatenate 'string +alphabet+ "+"))
(defparameter +alphabet-without-j+ "abcdefghiklmnopqrstuvwxyz")
(defparameter +alphabet-without-q+ "abcdefghijklmnoprstuvwxyz")
(defparameter +alphabet-without-j-and-v+ "abcdefghiklmnopqrstuwxyz")
(defparameter +alphabet-and-numbers+ (concatenate 'string +alphabet+ "0123456789"))
(defparameter +primes-up-to-100+ (make-hash-table-set-from-list '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)))
(defparameter +list-of-whitespace-characters+ (list #\Space #\Return #\Tab))
(defparameter +list-of-punctuation-characters+ (list #\! #\- #\' #\" #\? #\. #\, #\; #\:))

;;; conditions...

(define-condition odd-number-of-characters (error)
  ((what :initarg :what :reader what)
   (why :initarg :why :reader why))
  (:report (lambda (condition stream) (format stream "String length: ~a~%~a~%~%" (what condition) (why condition)))))

(define-condition invalid-number-of-characters (error)
  ((what :initarg :what :reader what)
   (why :initarg :why :reader why))
  (:report (lambda (condition stream) (format stream "Invalid string: ~a~%~a~%~%" (what condition) (why condition)))))

(define-condition invalid-character-in-string (error)
  ((what :initarg :what :reader what)
   (why :initarg :why :reader why))
  (:report (lambda (condition stream) (format stream "Invalid character: ~a~%~a~%~%" (what condition) (why condition)))))

(define-condition duplicate-character-in-string (error)
  ((what :initarg :what :reader what)
   (why :initarg :why :reader why))
  (:report (lambda (condition stream) (format stream "Invalid string: ~a~%~a~%~%" (what condition) (why condition)))))

(define-condition invalid-subsequence (error)
  ((what :initarg :what :reader what)
   (why :initarg :why :reader why))
  (:report (lambda (condition stream) (format stream "Invalid subsequence: ~a~%~a~%~%" (what condition) (why condition)))))

(define-condition invalid-number (error)
  ((what :initarg :what :reader what)
   (why :initarg :why :reader why))
  (:report (lambda (condition stream) (format stream "Invalid number: ~a~%~a~%~%" (what condition) (why condition)))))

(define-condition invalid-key (error)
  ((what :initarg :what :reader what)
   (why :initarg :why :reader why))
  (:report (lambda (condition stream) (format stream "Invalid key: ~a~%~a~%~%" (what condition) (why condition)))))

(define-condition invalid-argument (error)
  ((what :initarg :what :reader what)
   (why :initarg :why :reader why))
  (:report (lambda (condition stream) (format stream "Invalid argument: ~a~%~a~%~%" (what condition) (why condition)))))

;;; macros...

(defmacro dostring ((var string) &body body)
  (let ((str-name (gensym))
        (str-len (gensym))
        (i (gensym)))
    `(let ((,str-name ,string))
       (let ((,str-len (length ,str-name)))
         (dotimes (,i ,str-len)
           (let ((,var (char ,str-name ,i)))
             ,@body))))))

;;; generic functions continued...

(defun prep-alphabet (alphabet &key (downcasing t))
  "Creates and returns a new string based on the string ALPHABET such that the new string only has the unique characters of the original string according to a left-to-right assessment where only first unique character is retained. When DOWNCASING is T, ALPHABET will be downcased before assessment; keep in mind that same characters are considered distinct when of different case."
  (let ((new-alpha (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (all-chars-ht (make-hash-table)))
    (dostring (c alphabet)
      (when downcasing
        (setq c (char-downcase c)))
      (let ((v (gethash c all-chars-ht)))
        (if v
            nil ;; !!! could throw error here or give warnings (based on currently non-existent keyed arguments or something...)
            (progn
              (setf (gethash c all-chars-ht) t)
              (vector-push-extend c new-alpha)))))
    new-alpha))

(defun prep-string (str &key (downcasing t) (replacing nil) (filtering-by-alpha nil) (error-on-filter nil))
  "Creates and returns a new string from STR by changing, replacing, or removing characters according to the provided arguments. STR must be a string. When DOWNCASING is T, STR and the two strings of REPLACING are downcased. REPLACING must be a CONS of two strings of equal length (such as (CONS \"ab\" \"xy\"), so that every 'a' will be replaced with 'x' and every 'b' will be replaced with 'y'). FILTERING-BY-ALPHA must be an alphabetic string like +ALPHABET+; note that this function does not alter the passed alphabet, nor does it confirm or deny its validity. Also, setting FILTERING-BY-ALPHA will require the CDR string of REPLACING to only use characters from alphabet. When ERROR-ON-FILTER is T, an error will be triggered when a character is found in STR that is not in FILTERING-BY-ALPHA string. Keep in mind that all downcasing occurs first, then replacing, and then finally the filtering. Note that this function exists primarily to optimize and standardize the string-preparation process as done in nearly all the ciphers."
  ;; preparing hash-table names (whose use is dependant upon arguments passed)
  (let ((replacing-ht nil)
        (alpha-ht nil))
    ;; preparing alphabet hash-table... (Note that by this point the alphabet string is assumed to be good, without duplicates or other issues)
    (when filtering-by-alpha
      (setq alpha-ht (make-hash-table-set-from-str filtering-by-alpha)))
    ;; preparing "replacing" data
    (when replacing
      (let ((error-msg (format nil "Malformed REPLACING-CHARS object!~%Should be a CONS of two strings where each string is of same length.~%Characters from string1 will be replaced by characters from string2 (e.g. with (~s . ~s), every A will be changed to X, and every B to Y.)" "ab" "xy")))
        (unless (consp replacing)
          (error 'invalid-argument :what replacing :why error-msg))
        (when (or (not (stringp (car replacing))) (not (stringp (cdr replacing))))
          (error 'invalid-argument :what replacing :why error-msg))
        (when (/= (length (car replacing)) (length (cdr replacing)))
          (error 'invalid-argument :what replacing :why error-msg))
        (setq replacing-ht (make-hash-table))
        (dotimes (i (length (car replacing)))
          (let ((c1 (char (car replacing) i))
                (c2 (char (cdr replacing) i)))
            (when downcasing ;; downcasing the "replacing" string characters...
              (setq c1 (char-downcase c1))
              (setq c2 (char-downcase c2)))
            (when (gethash c1 replacing-ht)
              (error 'invalid-argument :what replacing :why (format nil "CAR (the first string) of REPLACING-CHARS CONS must not contain duplicate characters!~%The character ~s is duplicated...." c1)))
            (when (and alpha-ht (not (gethash c2 alpha-ht))) ;; when filtering and c2 not in ALPHABET
              (error 'invalid-argument :what replacing :why (format nil "CDR (the second string) of REPLACING-CHARS CONS must only contain characters from ALPHABET!~%The character ~s is not in ALPHABET....~%ALPHABET: ~s" c2 filtering-by-alpha)))
            (setf (gethash c1 replacing-ht) c2)))))
    ;; preparing to loop...
    (let* ((str-len (length str))
           (result (if filtering-by-alpha (make-array 0 :element-type 'character :adjustable t :fill-pointer 0) (make-string str-len))))
      ;; iterating over string changing each character according to set parameters
      (dotimes (i str-len)
        (let ((c (char str i)))
          (when downcasing
            (setq c (char-downcase c)))
          (when replacing
            (let ((new-c (gethash c replacing-ht)))
              (when new-c
                (setq c new-c))))
          (if filtering-by-alpha
              (let ((letter-found-p (gethash c alpha-ht)))
                (cond
                  (letter-found-p ;; when letter is in alphabet...
                   (vector-push-extend c result))
                  (error-on-filter ;; when letter is NOT in alphabet and "error" flag is set
                   (error 'invalid-character-in-string :what c :why (format nil "PREP-STRING is currently set to trigger an error on filtering, and the character ~s is not in the passed alphabet!~%" c)))))
              (setf (char result i) c))))
      result)))

(defun prep-string-by-removal (text &key (downcase-text t) (remove-whitespace t) (remove-punctuation t) (list-of-other-chars-to-remove nil))
  "Creates and returns a new string that is the result of removing characters from TEXT, or otherwise changing characters, according to passed arguments. When DOWNCASE-TEXT is T, all characters are considered in lower-case, even characters in the list LIST-OF-OTHER-CHARS-TO-REMOVE. When REMOVE-WHITESPACE is T, all characters in the list +LIST-OF-WHITESPACE-CHARACTERS+ will be removed. When REMOVE-PUNCTUATION is T, all characters in the list +LIST-OF-PUNCTUATION-CHARACTERS+ will be removed. When LIST-OF-OTHER-CHARS-TO-REMOVE is non-NIL, it must be a list of characters, and all characters in the list will not be included in returned string value."
  (let ((text-len (length text))
        (new-text (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (set-of-chars-to-remove (make-hash-table)))
    ;; preparing hash-table that holds all characters to be removed...
    (when list-of-other-chars-to-remove
      (dolist (c list-of-other-chars-to-remove)
        (setf (gethash (if downcase-text (char-downcase c) c) set-of-chars-to-remove) t)))
    (when remove-whitespace
      (dolist (c +list-of-whitespace-characters+)
        (setf (gethash c set-of-chars-to-remove) t)))
    (when remove-punctuation
      (dolist (c +list-of-punctuation-characters+)
        (setf (gethash c set-of-chars-to-remove) t)))
    ;; using the hash-table data to create the new restricted text
    (dotimes (i text-len)
      (let ((c (elt text i)))
        (when downcase-text
          (setq c (char-downcase c)))
        (let ((found (gethash c set-of-chars-to-remove)))
          (unless found
            (vector-push-extend c new-text)))))
    new-text))

(defun remove-characters-from-string (text list-of-chars-to-remove)
  "Creates and returns a string that is like TEXT but without any of the characters from LIST-OF-CHARS-TO-REMOVE."
  (let ((new-string (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (set-of-chars-to-remove (make-hash-table-set-from-list list-of-chars-to-remove)))
    (dostring (c text)
      (let ((found (gethash c set-of-chars-to-remove)))
        (unless found
          (vector-push-extend c new-string))))
    new-string))

;; !!! this is a very simple function that could be improved upon
(defun get-random-letter (alphabet)
  "Returns a single random letter from ALPHABET (by a random index)."
  (char alphabet (random (length alphabet))))

(defun filter-string-by-hash (text ht)
  "Creates and returns a new string from the passed TEXT such that only the characters that are present in hash-table, HT, are retained. It is recommended to use one of the MAKE-HASH-TABLE-SET-FROM-* functions to create a valid HT argument."
  (let ((s (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (dotimes (i (length text))
      (let ((c (char text i)))
        (when (gethash c ht)
          (vector-push-extend c s))))
    s))

;; !!! this function could be made much more efficient, although that seems somewhat unnecessary due to the fact that the resulting key can never be larger than the associated alphabet, which generally should not be very big
(defun key-string (text key &key (restricting-alphabet nil))
  "Creates and returns a new string that is the result of TEXT having been keyed by KEY. TEXT must be a string, and is usually an alphabet. KEY must be a string. When RESTRICTING-ALPHABET is a string, TEXT and KEY will be filtered according to the letters available in RESTRICTING-ALPHABET. Note that a keyed string is a concatenation of KEY without duplicates and TEXT both without characters from KEY and without its own duplicates, thus yielding a string with no duplicates at all (and that cannot be larger than the associated alphabet). As a simple example, \"abcde\" keyed by \"be\" yields \"beacd\"."
  (let ((text (filter-string-by-hash text (make-hash-table-set-from-str (remove-duplicates text))))
        (key (filter-string-by-hash key (make-hash-table-set-from-str (remove-duplicates key))))
        (keyed-string nil))
    ;; removing disallowed letters from both text and key
    (when restricting-alphabet
      (let ((ht (make-hash-table-set-from-str restricting-alphabet)))
        (setq text (filter-string-by-hash text ht))
        (setq key (filter-string-by-hash key ht))))
    ;; adding unique key characters to list
    (dotimes (i (length key))
      (setf keyed-string (adjoin (char key i) keyed-string)))
    ;; adding unique text characters to list
    (dotimes (i (length text))
      (setf keyed-string (adjoin (char text i) keyed-string)))
    (coerce (reverse keyed-string) 'string)))

(defun shift-string (string-to-be-shifted shift)
  "Creates and returns a new string that is the result of shifting the characters of STRING-TO-BE-SHIFTED left or right according to the value of SHIFT. STRING-TO-BE-SHIFTED must be a string. SHIFT must be an integer; note that very large SHIFT values are modded accordingly. As a simple example, a SHIFT of 1 on \"hello\" yields \"ohell\", and a SHIFT of -1 on \"hello\" yields \"elloh\"."
  (let ((str-len (length string-to-be-shifted)))
    (if (= 0 str-len)
        string-to-be-shifted ;; return passed string if it is empty...
        (let ((shift (mod shift str-len))
              (s string-to-be-shifted))
          (concatenate 'string (subseq s (- str-len shift)) (subseq s 0 (- str-len shift)))))))

(defun extend-key (key specified-length)
  "Creates and returns a new string that is the result of extending the string KEY by repetition until it is of length SPECIFIED-LENGTH. KEY must be a string. SPECIFIED-LENGTH must be an integer."
  (let ((key-len (length key)))
    (let ((extended-key (make-array specified-length :element-type 'character)))
      (dotimes (i specified-length)
        (setf (char extended-key i) (char key (mod i key-len))))
      extended-key)))

(defun make-char-pos-hash-table-from-str (text)
  "Creates and returns a hash-table from TEXT such that each key in the hash-table is a character that points to the unique integer value that relates to its index in TEXT. Thus, TEXT must be a string with only unique characters."
  (unless (= (length text) (length (remove-duplicates text)))
    (error 'duplicate-character-in-string :what text :why "The function make-char-pos-hash-table-from-str must receive a string with unique characters!"))
  (let ((ht (make-hash-table)))
    (dotimes (i (length text))
      (let ((c (char text i)))
        (setf (gethash c ht) i)))
    ht))

(defun get-alphabet-based-index-list-from-str (text &key (alphabet +alphabet+))
  "Creates and returns a list that holds all unique indexes of TEXT such that when considered in list-wise order each index yields its related character such that the characters are yielded up in alphabetical order. Note that when there are duplicate characters in TEXT, left-most characters are considered first in alphabetical order. TEXT must be a string. ALPHABET should be an alphabet-string, but note that although this function does ensure that every character in TEXT can be found in ALPHABET, it simply assumes (and does NOT check) that the alphabet itself is valid. As an example usage, the string \"apple\" yields the list '(0 4 3 1 2) because \"aelpp\" is the alphabetical re-ordering of TEXT, with #\a at index 0, #\e at 4, #\l at 3, #\p at 1, and other #\p at 2."
  (let ((alpha-ht (make-char-pos-hash-table-from-str alphabet))
        (ht (make-hash-table)) ;; each entry is a list of the related character's index in text
        (arr (make-array (length text)))) ;; is like original string but is an array where each element is the letter's index in alphabet
    ;; setting up ht and arr as described above...
    (dotimes (i (length text))
      (let* ((c (char text i))
             (alphabetic-index (gethash c alpha-ht)))
        (unless alphabetic-index
          (error 'invalid-character-in-string :what c :why (format nil "The character, ~s (in ~s), is not present in the passed alphabet, ~s." c text alphabet)))
        (push i (gethash alphabetic-index ht))
        (setf (elt arr i) alphabetic-index)))
    (sort arr #'(lambda (x y) (> x y))) ;; in descending order since, later, values will be pushed onto a list
    (let ((abil nil)) ;; abil --> alphabet-based-index-list
      ;; looking up, in order, each element of arr in the ht hash-table and pushing that value onto a new list (abil)
      (dotimes (i (length arr))
        (push (pop (gethash (elt arr i) ht)) abil))
      abil)))

(defun get-order-array-from-alphabet-based-index-list (passed-list)
  "This is a specialized function that is meant to receive a value returned from GET-ALPHABET-BASED-INDEX-LIST-FROM-STR as PASSED-LIST to create an array from it. The returned array corresponds to the original text such that each number in the array indicates that the character at the same index in original text is in the number's position (zero-based) in terms of relative alphabetical positioning. To try to clarify by example, when the PASSED-LIST is '(0 4 3 1 2) for the original-text \"apple\", this results in the array #(0 3 4 2 1) because #\a is in position 0 by relative alphabetical positioning, first #\p is in position 3, second #\p is in position 4, #\l is in position 2, and #\e is in position 1; but, notice that this function does not receive the original-text, and works more abstractly by simply tracking indexes in order (01234): thus, 0 is found at index 0 in PASSED-LIST, 1 at index 3, 2 at index 4, 3 at index 2, and 4 at index 1 (yielding 03421). Note that this function does not check PASSED-LIST in any way."
  (let ((arr (make-array (length passed-list) :element-type 'integer :adjustable nil))
        (i 0))
    (dolist (x passed-list)
      (setf (aref arr x) i)
      (incf i))
    arr))

(defun disrupt-text (text key disruption-string &key (alphabet +alphabet+))
  "Creates and returns a new string from TEXT such that it is disrupted according to the provided arguments; disruption, in this case, means that characters are inserted into the text at certain intervals based on the provided KEY. TEXT must be a string. KEY must be a string. DISRUPTION-STRING must be a string, and it is the characters from this string that are at random inserted into the TEXT. ALPHABET must be an alphabetic string; note that ALPHABET, in this function, has no effect on TEXT or DISRUPTION-STRING, and is used only to restrict KEY such that if KEY has characters not in ALPHABET an error will be triggered. Note that the disruption intervals are based on the \"order-array\" of the key; thus, to give an example, the TEXT \"hellothere\" when disrupted by the KEY \"abc\" (giving order-array #(0 1 2)) with DISRUPTION-STRING \"X\" yields \"XhXelXXlXotXXhXerXXe\", because, by order-array, 0 spaces are stepped before insertion, then 1 space, then 2 spaces, after which the 012 pattern continues until end of string. Finally, note that the return value of this function can be reverted back to its undisrupted state via the related function REMOVE-DISRUPTION-FROM-TEXT."
  (when (<= (length key) 1)
    (error 'invalid-key :what (format nil "~s" key) :why "The function disrupt-text requires that the passed key be 2 or more characters in length."))
  (let ((text-len (length text))
        (order-array (get-order-array-from-alphabet-based-index-list (get-alphabet-based-index-list-from-str key :alphabet alphabet)))
        (disrupted-text (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (do ((counter 0 (1+ counter)) ;; steps up to disruption-step and is then reset...
         (arr-len (length order-array))
         (arr-i 0)
         (text-i 0)) ;; changes only when not disrupting the text
        ((>= text-i text-len))
      (let ((disruption-step (aref order-array arr-i)))
        (if (= counter disruption-step)
            ;; adding a disrupting character to the disrupted text, while resetting the counter and getting next step count
            (let ((disrupting-character (get-random-letter disruption-string)))
              (vector-push-extend disrupting-character disrupted-text)
              (setq counter -1)
              (setq arr-i (mod (1+ arr-i) arr-len)))
            ;; adding the next character to the disrupted text, while incrementing the text index
            (let ((current-char (char text text-i)))
              (vector-push-extend current-char disrupted-text)
              (incf text-i)))))
    disrupted-text))

(defun remove-disruption-from-text (disrupted-text key &key (alphabet +alphabet+))
  "Creates and returns a new string from DISRUPTED-TEXT such that all disruption has been removed, assuming the correct KEY was passed. DISRUPTED-TEXT must be a string created by the function DISRUPT-TEXT. KEY must be a string. ALPHABET must be an alphabetic string; note that every character in KEY must be in ALPHABET or else an error will be triggered. See the related function DISRUPT-TEXT for more details."
  (when (<= (length key) 1)
    (error 'invalid-key :what (format nil "~s" key) :why "The function remove-disruption-from-text requires that the passed key be 2 or more characters in length."))
  (let ((dt-len (length disrupted-text))
        (order-array (get-order-array-from-alphabet-based-index-list (get-alphabet-based-index-list-from-str key :alphabet alphabet)))
        (cleared-text (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (do ((counter 0 (1+ counter)) ;; steps up to disruption-step and is then reset...
         (dt-i 0 (1+ dt-i)) ;; increments every iteration (unlike disruption function)
         (arr-len (length order-array))
         (arr-i 0))
        ((>= dt-i dt-len))
      (let ((disruption-step (aref order-array arr-i)))
        (if (= counter disruption-step)
            ;; ignoring the disruption character, while resetting the counter and getting the next step count
            (progn
              (setq counter -1)
              (setq arr-i (mod (1+ arr-i) arr-len)))
            ;; getting the original character from the disrupted text
            (let ((current-char (char disrupted-text dt-i)))
              (vector-push-extend current-char cleared-text)))))
    cleared-text))

(defun separate-repeating-characters (text added-char &optional secondary-char)
  "Creates and returns a new string from TEXT wherein all repeating characters have been separated by another character. TEXT must be a string. ADDED-CHAR must be a character. SECONDARY-CHAR, if provided, must be a character that is distinct from ADDED-CHAR; note that the secondary character must be provided when repeating ADDED-CHARs already exist within TEXT."
  (when (and secondary-char (char= added-char secondary-char))
    (error 'invalid-argument :what secondary-char :why "ADDED-CHAR and SECONDARY-CHAR must be unique characters in relation to each other!"))
  (let ((text-len (length text))
        (s (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (when (> text-len 0)
      (vector-push-extend (char text 0) s)
      (do ((i 1 (1+ i)))
          ((>= i text-len))
        (when (char= (char s (1- (length s))) (char text i)) ;; when next character is the same as last...
          ;; add a separating character...
          (if (char= (char text i) added-char)
              (if secondary-char
                  (vector-push-extend secondary-char s)
                  (error 'invalid-argument :what secondary-char :why "SECONDARY-CHAR must be set for this TEXT, because ADDED-CHAR alone will not result it complete separation."))
              (vector-push-extend added-char s)))
        (vector-push-extend (char text i) s)))
    s))

(defun distinguish-by-groups-of-two (text added-char secondary-char &key (make-string-even t))
  "Creates and returns a new string from TEXT wherein every consecutive grouping of 2 characters necessarily consists of distinct characters such that where two of the same character would occupy one group, the second character of group is displaced into the next group by ADDED-CHAR or SECONDARY-CHAR so as to ensure aforementioned distinctness. TEXT must be a string. ADDED-CHAR must be a character. SECONDARY-CHAR must be a character that is distinct from ADDED-CHAR. Note that ADDED-CHAR is the prioritized character in term of choosing which character to add into text, with SECONDARY-CHAR only ever being chosen when ADDED-CHAR fails to cause distinction. When MAKE-STRING-EVEN is T, if the resulting string length is odd, one more character will be added to end (either ADDED-CHAR or SECONDARY-CHAR, as appropriate) to give the string an even length."
  (when (char= added-char secondary-char)
    (error 'invalid-argument :what secondary-char :why "ADDED-CHAR and SECONDARY-CHAR must be unique characters in relation to each other!"))
  (let ((text-len (length text))
        (result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (do ((i 0) ;; note that i is incremented within body and is only incremented when a character from TEXT is appended to RESULT
         (first-of-group t (not first-of-group))) ;; FIRST-OF-GROUP is always T or NIL, alternating each iteration of loop
        ((>= i text-len))
      (let ((c (char text i)))
        (if first-of-group
            (progn ;; extend RESULT with character from TEXT
              (vector-push-extend c result)
              (incf i))
            (progn ;; extend RESULT with non-duplicate character which may or may not be from TEXT
              (let ((last-c (char result (1- (fill-pointer result))))) ;; the last character added to RESULT (and necessarily the first of a distinct group)
                (if (char/= c last-c) ;; if not a duplicate
                    (progn ;; extend RESULT with character from TEXT
                      (vector-push-extend c result)
                      (incf i))
                    (if (char= last-c added-char)
                        (vector-push-extend secondary-char result)
                        (vector-push-extend added-char result))))))))
    (when (and make-string-even (oddp (fill-pointer result))) ;; ! note that the below LET and IF sections are taken directly from the above loop
      (let ((last-c (char result (1- (fill-pointer result)))))
        (if (char= last-c added-char)
            (vector-push-extend secondary-char result)
            (vector-push-extend added-char result))))
    result))

(defun put-in-string (added-str receiving-str &optional (start-index 0))
  "Modifies the string RECEIVING-STR in-place and with its length unaltered such that its current contents are overwritten with the contents of ADDED-STR from left-to-right starting at index START-INDEX; note that although RECEIVING-STR in changed in-place it is still returned. ADDED-STR must be a string. RECEIVING-STR must be a string, and should be an actual variable. START-INDEX must be a non-negative integer, but note that passing an index that is invalid due to being too large will not cause an error and will simply result in no changes being made to RECEIVING-STR."
  (let ((str-len (length receiving-str))
        (cur-index start-index))
    (when (< start-index 0)
      (error 'invalid-argument :what start-index :why "START-INDEX must be greater than or equal to zero!"))
    (when (< start-index str-len)
      (dostring (c added-str)
        (setf (char receiving-str cur-index) c)
        (incf cur-index)
        (when (>= cur-index str-len)
          (return)))))
  receiving-str)

;;; ciphers...

(defun encrypt-adfgvx-cipher-simple (text)
  (let ((letters "adfgvx")
        (ht (make-hash-table))
        (encrypted (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))) ;; using an adjustable array because text may contain invalid characters
    ;; creating the hash-table that represents the grid
    (let ((i 0))
      (dotimes (d 6)
        (dotimes (r 6)
          (setf (gethash (char +alphabet-and-numbers+ i) ht) (make-array 2 :element-type 'character :initial-contents (list (char letters d) (char letters r))))
          (incf i))))
    ;; creating the encrypted string, ignoring all characters that do not exist in the hash-table
    (dotimes (i (length text))
      (let* ((c (char-downcase (char text i))) ;; downcasing
             (two-char-str (gethash c ht)))
        (when two-char-str ;; filtering
          (vector-push-extend (char two-char-str 0) encrypted)
          (vector-push-extend (char two-char-str 1) encrypted))))
    encrypted))

(defun decrypt-adfgvx-cipher-simple (text)
  (let ((letters "adfgvx")
        (ht (make-hash-table :test 'equal)) ;; keys are strings of size 2, such as "ag", whereas values are characters, such as #\d
        (decrypted nil) ;; will be set after length is checked
        (text-len (length text)))
    (when (oddp text-len)
      (error 'odd-number-of-characters :what text-len :why "adfgvx decryption only works when there is an even number of characters!"))
    ;; making the result string now that the text-len is validated
    (setq decrypted (make-string (/ text-len 2)))
    ;; checking the validity of passed text, by checking each character against the letter possibilities
    (let ((letter-hash-table (make-hash-table-set-from-str letters)))
      (dotimes (i text-len)
        (let ((c (char text i)))
          (unless (gethash c letter-hash-table)
            (error 'invalid-character-in-string :what c :why "adfgvx decryption only works with the lower-case letters a, d, f, g, v, and x!")))))
    ;; simulating the grid by filling out a hash-table with all possible letter-based coordinate combinations
    (let ((i 0))
      (dotimes (d 6)
        (dotimes (r 6)
          (setf (gethash (make-array 2 :element-type 'character :initial-contents (list (char letters d) (char letters r))) ht) (char +alphabet-and-numbers+ i))
          (incf i))))
    ;; decrypting the text two letters at a time, using the hash-table just created
    (do ((i 0 (1+ i))
         (start 0 (+ start 2)))
        ((>= start text-len))
      (setf (char decrypted i) (gethash (subseq text start (+ start 2)) ht)))
    decrypted))

(defun adfgvx-cipher-simple (text &key (encrypt t))
  "Using a 6x6 grid where each row/column is labelled ADFGVX (in that order) and where the grid consists of the full English alphabet and the number 0-9, each character from the original string is found within the grid and its coordinates become a segment of the 'encrypted' string. Thus, 'b' becomes 'ad' because of its (0 . 1) coordinate. Note that passed text is downcased, and all non-alphanumeric characters are lost."
  (if encrypt
      (encrypt-adfgvx-cipher-simple text)
      (decrypt-adfgvx-cipher-simple text)))

(defun encrypt-adfgx-cipher-simple (text)
  (let ((letters "adfgx")
        (ht (make-hash-table))
        (encrypted (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))) ;; using an adjustable array because text may contain invalid characters
    ;; creating the hash-table that represents the grid
    (let ((i 0))
      (dotimes (d 5)
        (dotimes (r 5)
          (setf (gethash (char +alphabet-without-j+ i) ht) (make-array 2 :element-type 'character :initial-contents (list (char letters d) (char letters r))))
          (incf i))))
    ;; adding #\j to alphabet such that every #\j will be interpreted as an #\i
    (setf (gethash #\j ht) (gethash #\i ht))
    ;; creating the encrypted string, ignoring all characters that do not exist in the hash-table
    (dotimes (i (length text))
      (let* ((c (char-downcase (char text i))) ;; downcasing
             (two-char-str (gethash c ht)))
        (when two-char-str ;; filtering...
          (vector-push-extend (char two-char-str 0) encrypted)
          (vector-push-extend (char two-char-str 1) encrypted))))
    (coerce encrypted 'string)))

(defun decrypt-adfgx-cipher-simple (text)
  (let ((letters "adfgx")
        (ht (make-hash-table :test 'equal)) ;; keys are strings of size 2, such as "ag", whereas values are characters, such as #\d
        (decrypted nil) ;; will be set after length is checked
        (text-len (length text)))
    (when (oddp text-len)
      (error 'odd-number-of-characters :what text-len :why "adfgx decryption only works when there is an even number of characters!"))
    ;; making the result string now that the text-len is validated
    (setq decrypted (make-string (/ text-len 2)))
    ;; checking the validity of passed text, by checking each character against the letter possibilities
    (let ((letter-hash-table (make-hash-table-set-from-str letters)))
      (dotimes (i text-len)
        (let ((c (char text i)))
          (unless (gethash c letter-hash-table)
            (error 'invalid-character-in-string :what c :why "adfgx decryption only works with the lower-case letters a, d, f, g, and x!")))))
    ;; simulating the grid by filling out a hash-table with all possible letter-based coordinate combinations
    (let ((i 0))
      (dotimes (d 5)
        (dotimes (r 5)
          (setf (gethash (make-array 2 :element-type 'character :initial-contents (list (char letters d) (char letters r))) ht) (char +alphabet-without-j+ i))
          (incf i))))
    ;; decrypting the text two letters at a time, using the hash-table just created
    (do ((i 0 (1+ i))
         (start 0 (+ start 2)))
        ((>= start text-len))
      (setf (char decrypted i) (gethash (subseq text start (+ start 2)) ht)))
    decrypted))

(defun adfgx-cipher-simple (text &key (encrypt t))
  "Using a 5x5 grid where each row/column is labelled ADFGX (in that order) and where the grid consists of the English alphabet but without 'j', each character from the original string is found within the grid and its coordinates become a segment of the 'encrypted' string. Thus, 'b' becomes 'ad' because of its (0 . 1) coordinate. Note that passed text is downcased, and all characters not in the used alphabet are lost."
  (if encrypt
      (encrypt-adfgx-cipher-simple text)
      (decrypt-adfgx-cipher-simple text)))

;; !!! could be optimized so that a hash-table gives the new character directly (meaning there would be an encrypting and decryption hash-table...), instead of by making the transformation for every character in the text
(defun affine-cipher (text &key (prime-v 3) (added-v 0) (alphabet +alphabet+) (encrypt t))
  "Changes the passed text by applying a mathematical mutation (using prime values) to each character's position in the used alphabet to get a new index to be used with the same alphabet (guaranteeing, by the use of primes, a 1-to-1 character transformation). ADDED-V is the value added onto the final index, thereby effecting a shift. Note that passed text is downcased and all characters not in the alphabet are lost."
  (setq alphabet (prep-alphabet alphabet))
  (when encrypt
    (setq text (prep-string text :filtering-by-alpha alphabet)))
  (let ((alpha-len (length alphabet))
        (text-len (length text)))
    ;; making ADDED-V more manageable in case it is very large positively or negatively
    (setq added-v (mod added-v alpha-len))
    (unless (gethash prime-v +primes-up-to-100+)
      (error 'invalid-number :what prime-v :why "Affine cipher only works with a prime value of less than 100!"))
    (unless (= 1 (gcd prime-v alpha-len))
      (error 'invalid-number :what prime-v :why "Affine cipher's prime value must be coprime with length of alphabet!"))
    (let ((result (make-string text-len))
          (ht (make-char-pos-hash-table-from-str alphabet)))
      ;; when decrypting, must change the value of prime-v... !!!!!!!!!! this part can be optimized...
      (unless encrypt
        (dotimes (x alpha-len)
          (when (= 1 (mod (* prime-v x) alpha-len))
            (setq prime-v x)
            (return))))
      ;; creating RESULT text...
      (dotimes (i text-len) ;; for each character in text...
        (let ((char-index (gethash (char text i) ht)) ;; find character position in alphabet
              (alpha-index nil)) ;; is set immediately below based on ENCRYPT...
          (unless char-index
            (error 'invalid-character-in-string :what (char text i) :why (format nil "The character ~s is not present in ALPHABET, ~s." (char text i) alphabet)))
          (if encrypt ;; find a new alphabetic index after mathematical mutation of char-index
              (setq alpha-index (mod (+ (* prime-v char-index) added-v) alpha-len))
              (setq alpha-index (mod (* prime-v (- char-index added-v)) alpha-len)))
          (setf (char result i) (char alphabet alpha-index)))) ;; add the new character to result string
      result)))

(defun atbash-cipher (text &key (alphabet +alphabet+) (encrypt t))
  "This cipher effectively reverse-transposes an alphabet with itself so that every letter becomes the one on the symmetrically opposite end (eg. 'a' becomes 'z' and 'b' becomes 'y'). Note that encryption and decryption follow the same exact process except that decryption (as usual) does not downcase or filter. Note that every character will be downcased, and all characters not in ALPHABET are lost."
  (setq alphabet (prep-alphabet alphabet))
  (when encrypt
    (setq text (prep-string text :filtering-by-alpha alphabet)))
  (let ((ht (make-hash-table))
        (text-len (length text)))
    ;; setting up ht...
    (map nil #'(lambda (k v) (setf (gethash k ht) v)) alphabet (reverse alphabet))
    ;; preparing result
    (let ((result (make-string text-len)))
      (dotimes (i text-len)
        (let* ((c (char text i))
               (new-c (gethash c ht)))
          (if new-c
              (setf (char result i) new-c)
              (error 'invalid-character-in-string :what c :why (format nil "The character ~s is not present in ALPHABET, ~s." c alphabet)))))
      result)))

(defun autokey-cipher (text primer &key (alphabet +alphabet+) (encrypt t))
  "Encrypts text by (conceptually) laying out the TEXT and a key into aligned rows, with the key being PRIMER plus TEXT such that key is of same length as TEXT; each vertical column of two letters is then used to get a new character according to the summed (and modded) index of the TEXT character and KEY character in ALPHABET, thus simulating (in English at least) a character lookup in the tabula recta. Note that all characters not in ALPHABET are lost."
  (setq alphabet (prep-alphabet alphabet))
  (when encrypt
    (setq text (prep-string text :filtering-by-alpha alphabet)))
  (setq primer (prep-string primer :filtering-by-alpha alphabet))
  (when (= 0 (length primer))
    (error 'invalid-key :what primer :why "Autokey cipher primer must have at least one character from used alphabet!"))
  (let ((ht (make-hash-table))
        (alpha-len (length alphabet))
        (text-len (length text))
        (primer-len (length primer)))
    ;; preparing the hash-table that gives index of each character according to its position in alphabet
    (dotimes (i alpha-len)
      (setf (gethash (char alphabet i) ht) i))
    ;; preparing key and other data...
    (let ((key (if encrypt ;; in both cases, the final key is primer+text up to length of text; note that for decryption, the key must be recreated during decryption process
                   (subseq (concatenate 'string primer text) 0 text-len) ;; !!! could be optimized...
                   (let ((key-str (make-string text-len)))
                     (put-in-string primer key-str))))
          (result (make-string text-len))
          (result-index 0))
      ;; iterating through both text and key, getting the index of each character and then combining them to get a new index from which a new character is gotten...
      (dotimes (i text-len)
        (let ((text-i (gethash (char text i) ht)) ;; text-i may be NIL for decryption because TEXT was not prepped... this is addressed below...
              (key-i (gethash (char key i) ht)))
          (if encrypt
              (setf (char result result-index) (char alphabet (mod (+ text-i key-i) alpha-len)))
              (progn ;; else, decrypting...
                (if (not text-i)
                    (error 'invalid-character-in-string :what (char text i) :why "Autokey cipher decryption detected a character not included in the used alphabet!")
                    (let ((decrypted-c (char alphabet (mod (+ (- alpha-len key-i) text-i) alpha-len)))
                          (decryption-key-index (+ primer-len i)))
                      (setf (char result result-index) decrypted-c)
                      (when (< decryption-key-index text-len)
                        ;; extending decryption key with decrypted text, but only when key is not as long as text...
                        (setf (char key decryption-key-index) decrypted-c))))))
          (incf result-index)))
      result)))

;; !!!!! currently all or part of KEY can be lost to filtering, without warning or error!
(defun bifid-cipher (text &key (shift 0) (key "") (grid-size 5) (alphabet +alphabet-without-j+) (replacing-chars (cons "j" "i")) (encrypt t))
  "This cipher uses a two-step grid-lookup process where each row/column is labelled (automatically) with the same unique characters from ALPHABET (and is read down then right). With default arguments, this cipher uses the standard alphabet without J, on a 5x5 (or GRID-SIZE by GRID-SIZE) grid. (Notice that the grid is necessarily a square [to always allow for reverse-lookups], and that ALPHABET must be exactly of length GRID-SIZE squared.) The ordering of ALPHABET on the grid is shifted and then keyed, and then each character of the text is looked up, giving two coordinates; these coordinates are then (conceptually) stacked with each new pair being placed to the right of the previous pair; afterwards this two row string is read left-to-right top-to-bottom such that every 2 indexes are used to lookup another value using the same grid; the result of this second lookup is the encrypted string. Note that all characters not in the ALPHABET are lost; additionally, with default arguments (because of REPLACING-CHARS) every J will become an I."
  (setq alphabet (prep-alphabet alphabet))
  (when encrypt
    (setq text (prep-string text :replacing replacing-chars :filtering-by-alpha alphabet)))
  (setq key (prep-string key :replacing replacing-chars :filtering-by-alpha alphabet))
  (when (< grid-size 1)
    (error 'invalid-argument :what grid-size :why "GRID-SIZE must be greater than or equal to 1!"))
  (let ((row-col-chars nil) ;; ! note that this is set below after the next error check...
        (grid-alpha (key-string (shift-string alphabet shift) key))
        (char-str-ht (make-hash-table))
        (str-char-ht (make-hash-table :test 'equal))
        (text-len (length text))
        (alphabet-len (length alphabet)))
    (unless (= alphabet-len (* grid-size grid-size))
      (error 'invalid-number-of-characters :what alphabet :why (format nil "Bifid cipher must receive an alphabet that has length exactly equal to GRID-SIZE (~a) squared, or ~a; the passed alphabet has length of ~a!" grid-size (* grid-size grid-size) alphabet-len)))
    ;; setting ROW-COL-CHARS according to the first few unique characters of ALPHABET
    (setq row-col-chars (subseq alphabet 0 grid-size))
    ;; setting up the hash-tables that simulate the grid-lookup process...
    (let ((i 0))
      (dotimes (d grid-size)
        (dotimes (r grid-size)
          (let ((temp-str (make-array 2 :element-type 'character :initial-contents (list (char row-col-chars d) (char row-col-chars r)))))
            (setf (gethash (char grid-alpha i) char-str-ht) temp-str)
            (setf (gethash temp-str str-char-ht) (char grid-alpha i)))
          (incf i))))
    (if encrypt
        ;; ENCRYPTING
        ;; preparing the "index rows", but instead of having distinct rows, a single string is used such that its first half is row-1 and its second half is row-2
        ;; ! this way I do not need to concatenate two strings later...
        (let ((index-string (make-string (* 2 text-len))))
          ;; do first index lookup, simulating the "stacking" of indexes by using INDEX-STRING
          (dotimes (i text-len)
            (let* ((c (char text i))
                   (two-char-str (gethash c char-str-ht)))
              (setf (char index-string i) (char two-char-str 0))
              (setf (char index-string (+ i text-len)) (char two-char-str 1))))
          ;; do second index lookup by stepping through the new string that is created from the arrays
          (let ((encrypted (make-string text-len)))
            (do ((start 0 (+ start 2))
                 (encrypted-index 0 (1+ encrypted-index))
                 (index-string-len (length index-string)))
                ((>= start index-string-len))
              (setf (char encrypted encrypted-index) (gethash (subseq index-string start (+ start 2)) str-char-ht)))
            encrypted))
        ;; DECRYPTING
        (let ((decrypted (make-string text-len))
              (index-string (make-string (* 2 text-len))))
          ;; change each character back to the repeatedly paired indexes (such that INDEX-STRING is filled in order of its indexes [e.g. 0,1,2,3...])
          (dotimes (i text-len)
            (let* ((c (char text i))
                   (two-char-str (gethash c char-str-ht)))
              (unless two-char-str
                (error 'invalid-character-in-string :what c :why (format nil "Bifid cipher decryption found invalid letter in original text: ~s" text)))
              (setf (char index-string (* i 2)) (char two-char-str 0))
              (setf (char index-string (1+ (* i 2))) (char two-char-str 1))))
          ;; split the string to make (conceptually) two rows and read each of the "stacked" coordinates as lookup-coordinates to get a character
          (dotimes (i text-len)
            (setf (char decrypted i) (gethash (make-array 2 :element-type 'character :initial-contents (list (elt index-string i) (elt index-string (+ i text-len)))) str-char-ht)))
          decrypted))))

(defun caesar-cipher (text shift &key (alphabet +alphabet+) (encrypt t))
  "Shifts the characters in TEXT according to the passed ALPHABET. For example, a shift of 1 on 'ab' gives 'bc'; whereas a shift of -2 on the same gives 'yz'. Note that passed text is downcased and all characters not in ALPHABET are lost."
  (setq alphabet (prep-alphabet alphabet))
  (if encrypt
      (setq text (prep-string text :filtering-by-alpha alphabet))
      (setq shift (- shift))) ;; reversing the shift here for decryption
  (let ((alpha-len (length alphabet))
        (text-len (length text))
        (ht (make-hash-table)))
    ;; adjusting the shift, in case it is larger than alphabet
    (setq shift (mod shift alpha-len))
    (let ((result (make-string text-len)))
      ;; setting up ht, which, when given a character, will return the new character (based on shift)
      (do ((i 0 (1+ i))
           (j shift (mod (1+ j) alpha-len)))
          ((>= i alpha-len))
        (setf (gethash (char alphabet i) ht) (char alphabet j)))
      ;; shifting each character in text
      (dotimes (i text-len)
        (let* ((c (char text i))
               (shifted-c (gethash c ht)))
          (unless shifted-c
            (error 'invalid-character-in-string :what c :why (format nil "Caesar cipher decryption found invalid letter in original text: ~s" text)))
          (setf (char result i) shifted-c)))
      result)))

(defun caesar-cipher-ascii (text shift &key (encrypt t))
  "Shifts the characters of a text string according to the ASCII value of each character. Note that this cipher only shifts ASCII alphabetic values, while ignoring and retaining all other characters (including numbers)."
  ;; adjusting the shift, in case it is larger than alphabet
  (setq shift (mod shift 26))
  (unless encrypt
    (setq shift (- shift))) ;; reversing the shift for decryption
  (let ((s (copy-seq text)))
    (dotimes (i (length s))
      (let ((c (char-code (char s i)))
            (dl (char-code #\a))
            (du (char-code #\A)))
        (when (alpha-char-p (char s i))
          (if (lower-case-p (char s i))
              (setf (char s i) (code-char (+ (mod (+ (- c dl) shift) 26) dl)))
              (setf (char s i) (code-char (+ (mod (+ (- c du) shift) 26) du)))))))
    s))

;; ! note that the approach taken by the below code does not actually create a grid or any intermediate data, but, rather, directly creates the result from the text by appropriately stepping across indexes
(defun encrypt-columnar-transposition-cipher (text key &key (disruption-key nil) (irregular t) (alphabet +alphabet+))
  (setq alphabet (prep-alphabet alphabet))
  (setq text (prep-string text :filtering-by-alpha alphabet))
  (setq key (prep-string key :filtering-by-alpha alphabet))
  (when disruption-key
    (setq disruption-key (prep-string disruption-key :filtering-by-alpha alphabet))
    (setq text (disrupt-text text disruption-key alphabet :alphabet alphabet))) ;; disrupting with random characters from alphabet
  (let ((key-len (length key))
        (text-len (length text)))
    (unless (>= key-len 2)
      (error 'invalid-key :what key :why "KEY must have length of 2 or greater (after filtering)!"))
    ;; add random characters to string to allow for regular rows if option was specified...
    (unless irregular
      (let ((added-num (mod text-len key-len)))
        (when (> added-num 0)
          (setq added-num (- key-len added-num))
          (let ((added (make-array added-num :element-type 'character :adjustable nil))) ;; this is the string of random characters that makes the text "regular"
            (dotimes (x added-num)
              (setf (aref added x) (get-random-letter alphabet)))
            (setq text (concatenate 'string text added))
            (setq text-len (length text))))))
    (let ((width key-len)
          (index-list (get-alphabet-based-index-list-from-str key :alphabet alphabet))
          (encrypted (make-string text-len))
          (encrypted-index -1)) ;; starts at -1 and is incremented at each usage
      ;; choose starting index (according to the order of index-list), then repeatedly step by WIDTH until at end of text; then choose next start and repeat
      (dolist (i index-list)
        (do ((index i (+ index width)))
            ((>= index text-len))
          (setf (elt encrypted (incf encrypted-index)) (char text index))))
      encrypted)))

;; ! note that the key IRREGULAR is not necessary for the decrypting function
(defun decrypt-columnar-transposition-cipher (text key &key (disruption-key nil) (alphabet +alphabet+))
  (setq alphabet (prep-alphabet alphabet))
  (setq key (prep-string key :filtering-by-alpha alphabet))
  (when disruption-key
    (setq disruption-key (prep-string disruption-key :filtering-by-alpha alphabet)))
  (let ((text-len (length text))
        (key-len (length key)))
    (unless (>= key-len 2)
      (error 'invalid-key :what key :why "KEY must have length of 2 or greater (after filtering)!"))
    (let ((index-list (get-alphabet-based-index-list-from-str key :alphabet alphabet))
          (row-count (ceiling (/ text-len key-len)))
          (full-columns (mod text-len key-len)) ;; this and the below variable, through their usage below, negate the need for knowing beforehand whether the grid is regular
          (empty-columns nil))
      ;; adjust full-columns value when necessary
      (when (= 0 full-columns)
        (setq full-columns key-len))
      ;; adjust empty-columns value based on full-columns value
      (setq empty-columns (- key-len full-columns))
      (let ((index 0)
            (decrypted (make-string (- (* key-len row-count) empty-columns))))
        ;; grouping full text into the columnar strings by taking a subsequence of TEXT and saving it into SUBSTRING (all according to the ordering of index-list)...
        (dolist (i index-list)
          (let* ((end-index (if (< i full-columns) (+ index row-count) (+ index (1- row-count))))
                 (substring (subseq text index end-index))) ;; this is the columnar text
            ;; ... next, iterate through each SUBSTRING writing each character step-wise into the DECRYPTED string (such that the [conceptual] grid is now read by row)
            (dotimes (ci (- end-index index))
              (let ((c (char substring ci)))
                (setf (char decrypted (+ i (* key-len ci))) c)))
            (setq index end-index)))
        (if disruption-key
            (remove-disruption-from-text decrypted disruption-key :alphabet alphabet)
            decrypted)))))

(defun columnar-transposition-cipher (text key &key (disruption-key nil) (irregular t) (encrypt t) (alphabet +alphabet+))
  "Receives passed TEXT and (conceptually) writes it out in sequential rows according to the length of KEY, and then reads down the columns in an order dictated by the alphabetical order of KEY to create the reorganized 'encrypted' text. This cipher has built-in disruption, which adds random characters to the text based on the DISRUPTION-KEY and ALPHABET (these random characters are subsequently removed by the decryption process). When IRREGULAR is nil, then random characters are added to end of string to make for a complete rectangular shape of the rows; the IRREGULAR argument is unnecessary for decryption (and note that decryption does not remove the added random characters). Note that TEXT is downcased and all characters not in ALPHABET are lost."
  (if encrypt
      (encrypt-columnar-transposition-cipher text key :disruption-key disruption-key :irregular irregular :alphabet alphabet)
      (decrypt-columnar-transposition-cipher text key :disruption-key disruption-key :alphabet alphabet)))

(defun custom-n-by-m-cipher (text vert-str hori-str &key (shift 0) (key "") (alphabet nil) (replacing-chars nil) (encrypt t))
  "This cipher uses a (conceptual) grid that contains a shifted and then keyed ALPHABET with labelled rows and columns. The column values are taken from VERT-STR and the row values are taken from HORI-STR. To encrypt, each character from TEXT is found on the grid and is replaced by its two character-based coordinates. Note that passed TEXT is downcased, and all characters (in both TEXT and KEY) not found in ALPHABET are lost. Using REPLACING-CHARS can help to reduce some character loss by creating a character overlap in the text; for example, to make it so that 'i' and 'j' are both 'i' here are sample arguments ->  :alphabet +alphabet-without-j+ :replacing-chars (cons \"j\" \"i\"); such substitution can only be used with encryption."
  (unless alphabet
    (error 'invalid-argument :what alphabet :why "Custom n by m cipher must receive an alphabet!"))
  (setq alphabet (prep-alphabet alphabet))
  (when encrypt
    (setq text (prep-string text :replacing replacing-chars :filtering-by-alpha alphabet)))
  (setq key (prep-string key :replacing replacing-chars :filtering-by-alpha alphabet))
  (let ((vert-len (length vert-str))
        (hori-len (length hori-str))
        (text-len (length text))
        (alphabet-len (length alphabet)))
    (unless encrypt
      (when (oddp text-len)
        (error 'odd-number-of-characters :what text-len :why (format nil "Custom n by m cipher decryption only works when there is an even number of characters!~%The TEXT string: ~s." text))))
    (unless (= vert-len (length (remove-duplicates vert-str)))
      (error 'duplicate-character-in-string :what vert-str :why "Custom n by m cipher's vertical string must have no duplicate characters!"))
    (unless (= hori-len (length (remove-duplicates hori-str)))
      (error 'duplicate-character-in-string :what hori-str :why "Custom n by m cipher's horizontal string must have no duplicate characters!"))
    (unless (= alphabet-len (* vert-len hori-len))
      (error 'invalid-number-of-characters :what alphabet :why (format nil "Custom n by m cipher must receive an alphabet that has length equal to VERT-STR (~a) times HORI-STR (~a), or ~a; the passed alphabet has length of ~a!" vert-len hori-len (* vert-len hori-len) alphabet-len)))
    (let ((alpha (key-string (shift-string alphabet shift) key)))
      (if encrypt
          (progn ;; ENCRYPTING
            (let ((ht (make-hash-table))
                  (encrypted (make-string (* 2 text-len))))
              ;; simulating the grid by filling out a hash-table with all possible character-based coordinate combinations
              (let ((i 0))
                (dotimes (d vert-len)
                  (dotimes (r hori-len)
                    (setf (gethash (char alpha i) ht) (cons (char vert-str d) (char hori-str r)))
                    (incf i))))
              ;; encrypting the text using the hash-table just created, changing each original character to two coord-characters in the same order
              (let ((str-index -1)) ;; is incremented with each new character addition...
                (dotimes (i text-len)
                  (let* ((c (char text i))
                         (coord-characters (gethash c ht)))
                    (setf (elt encrypted (incf str-index)) (car coord-characters))
                    (setf (elt encrypted (incf str-index)) (cdr coord-characters)))))
              encrypted))
          (progn ;; DECRYPTING
            (let ((ht (make-hash-table :test 'equal))
                  (decrypted (make-string (/ text-len 2))))
              ;; simulating the grid by filling out a hash-table with all possible character-based coordinate combinations
              (let ((i 0))
                (dotimes (d vert-len)
                  (dotimes (r hori-len)
                    (setf (gethash (make-array 2 :element-type 'character :initial-contents (list (char vert-str d) (char hori-str r))) ht) (char alpha i))
                    (incf i))))
              ;; decrypting the text using the hash-table just created, changing each two-character coord-str into the appropriate character while keeping same order
              (do ((i 0 (1+ i))
                   (start 0 (+ start 2)))
                  ((>= start text-len))
                (let* ((sub-string (subseq text start (+ start 2)))
                       (new-char (gethash sub-string ht)))
                  (if new-char
                      (setf (char decrypted i) new-char)
                      (error 'invalid-subsequence :what sub-string :why (format nil "Custom n by m cipher decryption found invalid substring in original string.~%Only characters from VERT-STR or HORI-STR (in appropriate combination) are allowed!~%VERT-STR: ~s~%HORI-STR: ~s~%" vert-str hori-str)))))
              decrypted))))))

;;; ciphers based on CUSTOM-N-BY-M-CIPHER

(defun adfgvx-cipher (text &key (shift 0) (key "") (alphabet +alphabet-and-numbers+) (replacing-chars nil) (encrypt t))
  "Using a 6x6 grid where each row/column is labelled ADFGVX (in that order) and where the grid consists of the full English alphabet and the numbers 0-9, each character from the original string is found within the grid and its coordinates become a segment of the 'encrypted' string. Thus, 'b' becomes 'ad' because of its (0 . 1) coordinate. Note that passed text is downcased, and all non-alphanumeric characters are lost."
  (custom-n-by-m-cipher text "adfgvx" "adfgvx" :shift shift :key key :alphabet alphabet :replacing-chars replacing-chars :encrypt encrypt))

(defun adfgx-cipher (text &key (shift 0) (key "") (alphabet +alphabet-without-j+) (replacing-chars (cons "j" "i")) (encrypt t))
  "Using a 5x5 grid where each row/column is labelled ADFGX (in that order) and where the grid consists of the English alphabet but without 'j', each character from the original string is found within the grid and its coordinates become a segment of the 'encrypted' string. Thus, 'b' becomes 'ad' because of its (0 . 1) coordinate. Note that passed text is downcased, and all characters not in the used alphabet are lost."
  (custom-n-by-m-cipher text "adfgx" "adfgx" :shift shift :key key :alphabet alphabet :replacing-chars replacing-chars :encrypt encrypt))

(defun polybius-cipher (text &key (shift 0) (key "") (alphabet +alphabet-without-j+) (replacing-chars (cons "j" "i")) (encrypt t))
  "Using a 5x5 grid where each row/column is labelled 12345 (in that order) and where the grid consists of the English alphabet but without 'j', each character from the original string is found within the grid and its coordinates become a segment of the final 'encrypted' string. Thus, 'b' becomes '12' because of its (0 . 1) coordinate. Note that passed text is downcased, and all characters not in the used alphabet are lost."
  (custom-n-by-m-cipher text "12345" "12345" :shift shift :key key :alphabet alphabet :replacing-chars replacing-chars :encrypt encrypt))

;;; ciphers continued...

(defun four-square-cipher (text &key (shift1 0) (key1 "") (shift2 0) (key2 "") (grid-height 5) (grid-width 5) (alphabet +alphabet-without-j+) (replacing-chars (cons "j" "i")) (encrypt t))
  "Encrypts TEXT through the use of four alphabetic grids, where the top-left and bottom-right grids are straightforwardly derived from ALPHABET, the top-right is derived from ALPHABET shifted by SHIFT1 and keyed by KEY1, and the bottom-left is derived from ALPHABET but shifted by SHIFT2 and keyed by KEY2. Each two-character group of TEXT is respectively looked up in the top-left and then bottom-right grids so as to form a (conceptual) rectangle whose corners lie in the four grids; the character at the opposite corner horizontally is the character in the encrypted string. Decryption simply reverses this lookup process. Use REPLACING-CHARS to create a character overlap (such as making every J an I) in order to make an alphabet fit the grid. Note that ALPHABET must be of length equal to GRID-HEIGHT times GRID-WIDTH, and all characters not in ALPHABET are lost."
  (setq alphabet (prep-alphabet alphabet))
  (when encrypt
    (setq text (prep-string text :replacing replacing-chars :filtering-by-alpha alphabet)))
  (setq key1 (prep-string key1 :replacing replacing-chars :filtering-by-alpha alphabet))
  (setq key2 (prep-string key2 :replacing replacing-chars :filtering-by-alpha alphabet))
  (let ((alpha-len (length alphabet))
        (text-len (length text)))
    (when (/= alpha-len (* grid-height grid-width))
      (error 'invalid-number-of-characters :what alphabet :why (format nil "ALPHABET must be of length GRID-HEIGHT (~d) times GRID-WIDTH (~d), meaning ~d, but is of length ~d!~%" grid-height grid-width (* grid-height grid-width) alpha-len)))
    (when (oddp text-len)
      (error 'odd-number-of-characters :what text :why "Four-square cipher only works when there is an even number of characters!"))
    (let ((alpha1 (key-string (shift-string alphabet shift1) key1))
          (alpha2 (key-string (shift-string alphabet shift2) key2))
          (result (make-string text-len)))
      (if encrypt
          (progn ;; ENCRYPTING
            ;; note that for encrypting the lookup is done from the basic alphabet, hence there is only one hash-table HT and two distinct grids for alpha1 and alpha2 for retrieving the encrypted value
            (let ((ht (make-hash-table)) ;; simulates the "top-left" and "bottom-right" alphabet grids... also, note that this hash-table holds CONSes of coords
                  (grid1 (make-array `(,grid-height ,grid-width))) ;; the "top-right" key grid
                  (grid2 (make-array `(,grid-height ,grid-width)))) ;; the "bottom-left" key grid
              ;; preparing the hash-table and grids...
              (let ((i 0))
                (dotimes (d grid-height)
                  (dotimes (r grid-width)
                    (setf (gethash (char alphabet i) ht) (cons d r))
                    (setf (aref grid1 d r) (char alpha1 i))
                    (setf (aref grid2 d r) (char alpha2 i))
                    (incf i))))
              ;; encrypting TEXT two characters at a time...
              (do ((i 0 (+ i 2)))
                  ((>= i text-len))
                (let ((coords1 (gethash (char text i) ht))
                      (coords2 (gethash (char text (1+ i)) ht)))
                  (setf (char result i) (aref grid1 (car coords1) (cdr coords2)))
                  (setf (char result (1+ i)) (aref grid2 (car coords2) (cdr coords1)))))))
          (progn ;; DECRYPTING
            ;; note that for decrypting the lookup is done from ALPHA1 and ALPHA2, hence there are two hash-tables HT1 and HT2 and one grid for the basic alphabet for retrieving the decrypted value
            (let ((ht1 (make-hash-table)) ;; simulates the "top-right" key grid... also, note that this hash-table holds CONSes of coords
                  (ht2 (make-hash-table)) ;; simulates the "bottom-left" key grid... also, note that this hash-table holds CONSes of coords
                  (grid (make-array `(,grid-height ,grid-width)))) ;; represents the "top-left" and "bottom-right" alphabet grids
              ;; preparing the hash-tables and the grid...
              (let ((i 0))
                (dotimes (d grid-height)
                  (dotimes (r grid-width)
                    (setf (gethash (char alpha1 i) ht1) (cons d r))
                    (setf (gethash (char alpha2 i) ht2) (cons d r))
                    (setf (aref grid d r) (char alphabet i))
                    (incf i))))
              ;; decrypting TEXT two characters at a time...
              (do ((i 0 (+ i 2)))
                  ((>= i text-len))
                (let ((hash1 (gethash (char text i) ht1))
                      (hash2 (gethash (char text (1+ i)) ht2)))
                  (unless hash1
                    (error 'invalid-character-in-string :what (char text i) :why "During decryption, all characters in TEXT must be present in ALPHABET!"))
                  (unless hash2
                    (error 'invalid-character-in-string :what (char text (1+ i)) :why "During decryption, all characters in TEXT must be present in ALPHABET!"))
                  (let ((coords1 hash1)
                        (coords2 hash2))
                    (setf (char result i) (aref grid (car coords1) (cdr coords2)))
                    (setf (char result (1+ i)) (aref grid (car coords2) (cdr coords1)))))))))
      result)))

(defun one-time-pad-cipher (text key-seq &key (numerical-seq t) (alphabet +alphabet+) (encrypt t))
  "Encrypts TEXT by (conceptually) laying out TEXT and KEY-SEQ (whose length must be greater than or equal to that of TEXT) into aligned rows where each vertical column is used to get a new character; if NUMERCIAL-SEQ is T then the key is a sequence of numbers, and the related number is added to the index of the text character in ALPHABET to get a new index that is used to look up a new character in ALPHABET; but when NUMERICAL-SEQ is NIL then KEY-SEQ must be a string, and each vertical column of two letters is then used to get a new character according to the summed (and modded) index of the TEXT character and KEY-SEQ character in ALPHABET. Note that all characters not in ALPHABET are lost."
  (setq alphabet (prep-alphabet alphabet))
  (when encrypt
    (setq text (prep-string text :filtering-by-alpha alphabet)))
  ;; verifying KEY-SEQ is valid (so far) according to passed value of NUMERICAL-SEQ, and throwing error if it is not; also filtering KEY-SEQ if it is a string
  (cond
    (numerical-seq ;; when KEY-SEQ should be an array or list...
     (when (listp key-seq)
       (setq key-seq (coerce key-seq 'vector)))
     (when (or (not (arrayp key-seq)) (stringp key-seq))
       (error 'invalid-argument :what key-seq :why "When NUMERICAL-SEQ is T then, KEY-SEQ must be an array (or list, but a list is coerced to an array), and cannot be a string!")))
    (t ;; when NUMERICAL-SEQ is nil, meaning KEY-SEQ should be a string
     (if (stringp key-seq)
         (setq key-seq (prep-string key-seq :filtering-by-alpha alphabet))
         (error 'invalid-argument :what key-seq :why "When NUMERICAL-SEQ is NIL, then KEY-SEQ must be a string!"))))
  (let ((text-len (length text))
        (key-seq-len (length key-seq)))
    (when (< key-seq-len text-len)
      (error 'invalid-argument :what (list :text text :key-seq key-seq) :why (format nil "ONE-TIME-PAD-CIPHER requires that KEY-SEQ length be greater than or equal to TEXT length!~%Note that this cipher filters TEXT by alphabet before encrypting...~%TEXT length: ~a~%KEY-SEQ length: ~a~%" text-len key-seq-len)))
    (let ((ht (make-hash-table))
          (alpha-len (length alphabet))
          (result (make-string text-len))
          (result-index 0))
      ;; preparing the hash-table that gives index of each character according to its position in alphabet
      (dotimes (i alpha-len)
        (setf (gethash (char alphabet i) ht) i))
      ;; iterating through both text and key, getting indexes and then combining them to get a new index from which a new character is gotten...
      (dotimes (i text-len)
        (let ((text-i (gethash (char text i) ht))
              (key-i (if numerical-seq (elt key-seq i) (gethash (char key-seq i) ht))))
          (unless text-i
            (error 'invalid-character-in-string :what (char text i) :why "Decryption requires that TEXT consists exclusively of characters from ALPHABET!"))
          (if encrypt
              (setf (char result result-index) (char alphabet (mod (+ text-i key-i) alpha-len)))
              (setf (char result result-index) (char alphabet (mod (+ (- alpha-len key-i) text-i) alpha-len))))
          (incf result-index)))
      result)))

;; ! could also add a feature to allow for sequential duplicates to simply go though un-enciphered (for the sake mostly of decrypting the work of other ciphers...)
(defun playfair-cipher (text &key (shift 0) (key "") (grid-height 5) (grid-width 5) (alphabet +alphabet-without-j+) (replacing-chars (cons "j" "i")) (primary-null-char #\q) (secondary-null-char #\z) (make-string-even t) (encrypt t))
  "Encrypts TEXT through the use of a grid whose contents is ALPHABET, shifted and keyed. Each two-character group of TEXT (in order) is looked up in the grid such that a (conceptual) rectangle is formed between the two found characters; if these two grouped characters are identical, then PRIMARY-NULL-CHAR displaces the second character (pushing it to the next grouping), and if the two characters are same as PRIMARY-NULL-CHAR then SECONDARY-NULL-CHAR is used instead; if the formed \"rectangle\" has both height and width to it, then each letter is replaced with the letter at the opposite row-wise corner; but, if the \"rectangle\" is tall (existing in a single column) then each letter is replaced by the letter immediately below it (with rotation); finally, if the \"rectangle\" is wide (existing in a single row) then each letter is replaced by the letter immediately to the right of it (again with rotation). Setting MAKE-STRING-EVEN to T causes one of the \"null\" characters to be added at end when the modified TEXT is of odd length. Use REPLACING-CHARS to create a character overlap (such as making every J and I an I) in order to make an alphabet fit the grid. Note that ALPHABET must be of length (* GRID-HEIGHT GRID-WIDTH), and all characters not in ALPHABET are lost."
  (setq alphabet (prep-alphabet alphabet))
  (setq key (prep-string key :replacing replacing-chars :filtering-by-alpha alphabet))
  (when encrypt
    (setq text (prep-string text :replacing replacing-chars :filtering-by-alpha alphabet)))
  (let ((alpha-ht (make-hash-table-set-from-str alphabet))
        (alpha-len (length alphabet)))
    (when (/= alpha-len (* grid-height grid-width))
      (error 'invalid-number-of-characters :what alphabet :why (format nil "ALPHABET must be of length GRID-HEIGHT (~d) times GRID-WIDTH (~d), meaning ~d, but is of length ~d!~%" grid-height grid-width (* grid-height grid-width) alpha-len)))
    (when encrypt
      (unless (gethash primary-null-char alpha-ht)
        (error 'invalid-argument :what primary-null-char :why "PRIMARY-NULL-CHAR must be in ALPHABET!"))
      (unless (gethash secondary-null-char alpha-ht)
        (error 'invalid-argument :what secondary-null-char :why "SECONDARY-NULL-CHAR must be in ALPHABET!"))
      (when (char= primary-null-char secondary-null-char) ;; ! note that the function DISTINGUISH-BY-GROUPS-OF-TWO actually performs this check as well...
        (error 'invalid-argument :what primary-null-char :why "PRIMARY-NULL-CHAR and SECONDARY-NULL-CHAR must be distinct characters!"))
      (setq text (distinguish-by-groups-of-two text primary-null-char secondary-null-char :make-string-even make-string-even)))
    (let ((alpha (key-string (shift-string alphabet shift) key))
          (ht (make-hash-table)) ;; gives grid coordinates for each alphabetic character
          (grid (make-array `(,grid-height ,grid-width) :element-type 'character))
          (text-len (length text)))
      (when (oddp text-len)
        (error 'odd-number-of-characters :what text :why (format nil "Playfair cipher only works when there is an even number of characters!~%Character count after prepping: ~d~%" text-len)))
      ;; preparing the hash-table and the grid...
      (let ((i 0))
        (dotimes (d grid-height)
          (dotimes (r grid-width)
            (setf (gethash (char alpha i) ht) (cons d r))
            (setf (aref grid d r) (char alpha i))
            (incf i))))
      (let ((result (make-string text-len)))
        ;; analyzing two characters at a time to perform the encryption/decryption process
        (do ((i 0 (+ i 2))
             (result-i -1)) ;; starts at -1 and is incremented at each use
            ((>= i text-len))
          (let ((coords1 (gethash (char text i) ht))
                (coords2 (gethash (char text (1+ i)) ht))
                (f (if encrypt #'1+ #'1-))) ;; if encrypting move down or right as appropriate (when characters in single line); otherwise, up or left
            (cond
              ((= (car coords1) (car coords2)) ;; when characters in same row
               (setf (char result (incf result-i)) (aref grid (car coords1) (mod (funcall f (cdr coords1)) grid-width)))
               (setf (char result (incf result-i)) (aref grid (car coords2) (mod (funcall f (cdr coords2)) grid-width))))
              ((= (cdr coords1) (cdr coords2)) ;; when characters in same column
               (setf (char result (incf result-i)) (aref grid (mod (funcall f (car coords1)) grid-height) (cdr coords1)))
               (setf (char result (incf result-i)) (aref grid (mod (funcall f (car coords2)) grid-height) (cdr coords2))))
              (t ;; otherwise...
               (setf (char result (incf result-i)) (aref grid (car coords1) (cdr coords2)))
               (setf (char result (incf result-i)) (aref grid (car coords2) (cdr coords1)))))))
        result))))

(defun encrypt-polybius-cipher-simple (text)
  (let ((ht (make-hash-table))
        (encrypted (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    ;; preparing the hash-table which simulates the grid lookup...
    (do ((d 1 (1+ d))
         (i 0))
        ((>= d 6))
      (do ((r 1 (1+ r)))
          ((>= r 6))
        (setf (gethash (char +alphabet-without-j+ i) ht) (make-array 2 :element-type 'character :initial-contents (list (digit-char d) (digit-char r))))
        (incf i)))
    ;; making it so that the character #\i is read as #\j from text...
    (setf (gethash #\j ht) (gethash #\i ht))
    ;; for each character, writing its two-character-coordinate to the encrypted string...
    (dotimes (i (length text))
      (let* ((c (char-downcase (char text i))) ;; downcasing here
             (two-char-str (gethash c ht)))
        (when two-char-str ;; filtering here...
          (vector-push-extend (char two-char-str 0) encrypted)
          (vector-push-extend (char two-char-str 1) encrypted))))
    encrypted))

(defun decrypt-polybius-cipher-simple (text)
  (let ((ht (make-hash-table :test 'equal))
        (text-len (length text))) 
    (when (oddp text-len)
      (error 'odd-number-of-characters :what text-len :why "Polybius square decryption only works when there is an even number of characters!"))
    (let ((decrypted (make-array (/ text-len 2) :element-type 'character)))
      ;; create the hash-table that simulates the grid lookup...
      (do ((d 1 (1+ d))
           (i 0))
          ((>= d 6))
        (do ((r 1 (1+ r)))
            ((>= r 6))
          (setf (gethash (make-array 2 :element-type 'character :initial-contents (list (digit-char d) (digit-char r))) ht) (char +alphabet-without-j+ i))
          (incf i)))
      ;; step through text to translate each character-based coordinate pair to its appropriate character...
      (do ((result-index 0 (1+ result-index))
           (i 0 (+ 2 i)))
          ((>= i text-len))
        (let* ((two-char-str (subseq text i (+ i 2)))
               (hash-value (gethash two-char-str ht)))
          (unless hash-value
            (error 'invalid-subsequence :what two-char-str :why "Polybius square decryption only works with the characters 1, 2, 3, 4, and 5!"))
          (setf (char decrypted result-index) hash-value)))
      decrypted)))

(defun polybius-cipher-simple (text &key (encrypt t))
  "Using a 5x5 grid where each row/column is labelled 12345 (in that order) and where the grid consists of the English alphabet but without 'j', each character from the original string is found within the grid and its coordinates become a segment of the final 'encrypted' string. Thus, 'b' becomes '12' because of its (0 . 1) coordinate. Note that passed text is downcased, and all characters not in the used alphabet are lost."
  (if encrypt
      (encrypt-polybius-cipher-simple text)
      (decrypt-polybius-cipher-simple text)))

;; !!! currently creates an intermediary dataset (although somewhat minimally); could be optimized to skip that...
(defun encrypt-rail-fence-cipher (text &key (fence-length 3) (downcase-text t) (remove-whitespace t) (remove-punctuation t) (list-of-other-chars-to-remove nil))
  (when (< fence-length 2)
    (error 'invalid-number :what fence-length :why "Rail fence cipher only works with a fence-length of greater than or equal to 2!"))
  (setq text (prep-string-by-removal text :downcase-text downcase-text :remove-whitespace remove-whitespace :remove-punctuation remove-punctuation :list-of-other-chars-to-remove list-of-other-chars-to-remove))
  (let ((text-len (length text)))
    (if (>= fence-length text-len)
        text ;; simply return TEXT (actually a copy) when FENCE-LENGTH is greater than or equal to TEXT-LEN
        (let ((fence-rows (make-array fence-length)) ;; an array of adjustable arrays where each array represents a single row of characters in the fence
              (increasing t) ;; controls the zigzag motion by signalling appropriate index change...
              (index 0))
          ;; setting up FENCE-ROWS further...
          (dotimes (i fence-length)
            (setf (elt fence-rows i) (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
          ;; filling out FENCE-ROWS in the zigzag pattern (simulating writing out characters zigzag-like, going FENCE-LENGTH down-right and then FENCE-LENGTH up-right, etc.)
          (dotimes (i text-len)
            (vector-push-extend (char text i) (elt fence-rows index))
            (if increasing
                (progn ;; going down-right
                  (incf index)
                  (when (>= index fence-length) ;; changing direction
                    (decf index 2) ;; correcting over-incrementation
                    (setq increasing nil)))
                (progn ;; going up-right
                  (decf index)
                  (when (< index 0) ;; changing direction
                    (incf index 2) ;; correcting over-decrementation
                    (setq increasing t)))))
          ;; read horizontally across the zigzag pattern (by rows) to get the encrypted text...
          (let ((encrypted (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
            (dotimes (i fence-length)
              (dotimes (j (length (aref fence-rows i)))
                (vector-push-extend (char (aref fence-rows i) j) encrypted)))
            encrypted)))))

;; ! note that the decrypting function does not accept the many text altering arguments
(defun decrypt-rail-fence-cipher (text &key (fence-length 3))
  (when (< fence-length 2)
    (error 'invalid-number :what fence-length :why "Rail fence cipher only works with a fence-length of greater than or equal to 2!"))
  (let ((text-len (length text)))
    (if (>= fence-length text-len)
        (copy-seq text) ;; simply return a copy of TEXT when FENCE-LENGTH is greater or equal to TEXT-LEN
        (let ((max-step (* 2 (1- fence-length))) ;; the largest step from character to character (as seen on top row of zigzag)
              (decrypted (make-array text-len :element-type 'character)))
          ;; iterates through TEXT (in TEXT's original order), placing each character into DECRYPTED at the appropriate index (through the use of various step variables)
          ;; the below loop iterates through the indexes of the first down-right line of the zigzag such that the later loop can repeatedly step the appropriate lengths...
          (do ((start-decrypted-i 0 (1+ start-decrypted-i))
               (text-index 0)
               ;; due to placement of characters row-wise within zigzag, the step value from char to next char will alternate; hence the need for two step variables...
               (step1 max-step (- step1 2)) ;; adjusting by 2 because each step up or down zigzag causes the steps across rows to differ by 2
               (step2 max-step (+ step2 2)))
              ((>= start-decrypted-i fence-length))
            ;; correcting step variables for when they go under 2 or over MAX-STEP
            (when (< step1 2)
              (setq step1 max-step))
            (when (> step2 max-step)
              (setq step2 2))
            ;; placing character into DECRYPTED, and afterwards incrementing TEXT-INDEX...
            (setf (char decrypted start-decrypted-i) (char text text-index))
            (incf text-index)
            ;; based on starting point set by last loop, place each TEXT character into DECRYPTED according to the alternating steps
            (let ((steps (vector step1 step2)))
              (do* ((steps-i 0 (mod (1+ steps-i) 2)) ;; an alternating 0 or 1 index for STEPS
                    (steps-value (aref steps steps-i) (aref steps steps-i)) ;; the alternating step
                    (current-decrypted-i (+ start-decrypted-i steps-value) (+ current-decrypted-i steps-value)))
                   ((>= current-decrypted-i text-len))
                ;; placing character into DECRYPTED, and afterwards incrementing TEXT-INDEX...
                (setf (char decrypted current-decrypted-i) (char text text-index))
                (incf text-index))))
          decrypted))))

(defun rail-fence-cipher (text &key (fence-length 3) (downcase-text t) (remove-whitespace t) (remove-punctuation t) (list-of-other-chars-to-remove nil) (encrypt t))
  "Encrypts text by writing out the text in a zigzag pattern, such that each zigzag line segment (corner-to-corner) has FENCE-LENGTH characters in it; this zigzag of characters is then read across by rows to get the encrypted text. Text is downcased by default before encryption; set DOWNCASE-TEXT to NIL to prevent downcasing. Whitespace and punctuation are also removed by default. Use LIST-OF-OTHER-CHARS-TO-REMOVE to remove any other unwanted characters."
  (if encrypt
      (encrypt-rail-fence-cipher text :fence-length fence-length :downcase-text downcase-text :remove-whitespace remove-whitespace :remove-punctuation remove-punctuation :list-of-other-chars-to-remove list-of-other-chars-to-remove)
      (decrypt-rail-fence-cipher text :fence-length fence-length)))

;; ! note that the decrypting functionality does not use any of the text-altering arguments
;; !!! currently creates an intermediary dataset (SCYTALE) when encrypting or decrypting; could be optimized to skip that...
(defun scytale-cipher (text &key (radius 3) (downcase-text t) (remove-whitespace t) (remove-punctuation t) (list-of-other-chars-to-remove nil) (encrypt t))
  "Encrypts text by writing text horizontally across a (conceptual) cylinder of a specified radius, and then reading the text vertically, left-to-right, to create the encrypted text; decryption follows a reversal of this process. Text is downcased by default before encryption; set DOWNCASE-TEXT to NIL to prevent downcasing. Whitespace and punctuation are also removed by default. Use LIST-OF-OTHER-CHARS-TO-REMOVE to remove any other unwanted characters."
  (when (< radius 2)
    (error 'invalid-number :what radius :why "Scytale cipher only works with a RADIUS of greater than or equal to 2!"))
  (when encrypt (setq text (prep-string-by-removal text :downcase-text downcase-text :remove-whitespace remove-whitespace :remove-punctuation remove-punctuation :list-of-other-chars-to-remove list-of-other-chars-to-remove)))
  (let ((text-len (length text)))
    (when (<= text-len radius)
      (return-from scytale-cipher (if encrypt text (copy-seq text)))) ;; return a copy of TEXT when TEXT length is too small to be affected by RADIUS
    (let* ((height radius)
           (width (ceiling (/ text-len height)))
           (temp (mod text-len width))
           (last-row-size (if (zerop temp) width temp))
           (empty-rows (floor (/ (- (* height width) text-len) width)))
           (result (make-array text-len :element-type 'character))
           (scytale (make-array `(,height ,width) :element-type 'character)))
      (if encrypt
          (progn ;; ENCRYPTING
            ;; writing the characters horizontally across the "scytale", row by row
            (let ((index 0))
              ;; fill out the complete upper rows and the final partial row (if any)
              (dotimes (d (- height empty-rows))
                (dotimes (r width)
                  (setf (aref scytale d r) (char text index))
                  (incf index)
                  (when (>= index text-len)
                    (return)))))
            ;; reading the characters off the "scytale" vertically to get the encrypted string, skipping any Null characters...
            (let ((index 0))
              (dotimes (r width)
                (dotimes (d height)
                  (let ((c (aref scytale d r)))
                    (when (char/= c #\Nul)
                      (setf (char result index) c)
                      (incf index)))))))
          (progn ;; DECRYPTING
            ;; writing the characters vertically down the "scytale", column by column
            (let ((index 0))
              (dotimes (r width)
                (dotimes (d (- height empty-rows))
                  (when (and (= d (1- (- height empty-rows))) (>= r last-row-size))
                    (return)) ;; don't write to last partial row if r-index is >= last-row-size
                  (setf (aref scytale d r) (char text index))
                  (incf index))))
            ;; reading the characters off the "scytale" horizontally to get the decrypted string...
            (let ((index 0))
              (dotimes (d (- height empty-rows))
                (dotimes (r width)
                  (let ((c (aref scytale d r)))
                    (unless (char= c #\Nul)
                      (setf (char result index) c)
                      (incf index))))))))
      result)))

(defun simple-substitution-cipher (text alpha1 alpha2 &key (filter t) (downcasing t) (encrypt t))
  "Encrypts TEXT by changing every character in TEXT from a character in one alphabet (ALPHA1) to a character of corresponding index in another alphabet (ALPHA2). Note that the passed alphabets must be of exactly the same size. By default, (when encrypting only) this function filters TEXT according to the contents of ALPHA1; to disable this, set FILTER to NIL, thereby allowing unencrypted text to fall through. Note that DOWNCASING controls the case of TEXT and ALPHA1 and ALPHA2."
  (setq alpha1 (prep-alphabet alpha1 :downcasing downcasing))
  (setq alpha2 (prep-alphabet alpha2 :downcasing downcasing))
  (when (and encrypt (or downcasing filter))
    (setq text (prep-string text :downcasing downcasing :filtering-by-alpha (if filter alpha1 nil))))
  (let ((alpha1-len (length alpha1))
        (alpha2-len (length alpha2))
        (text-len (length text))
        (ht (make-hash-table)))
    (when (/= alpha1-len alpha2-len)
      (error 'invalid-argument :what (list :alpha1 alpha1 :alpha2 alpha2) :why (format nil "ALPHA1 and ALPHA2 must be of exactly the same length!~%ALPHA1 length: ~d~%ALPHA2 length: ~d~%" alpha1-len alpha2-len)))
    ;; preparing the hash table
    (dotimes (i alpha1-len)
      (let ((c1 (char alpha1 i))
            (c2 (char alpha2 i)))
        (if encrypt
            (setf (gethash c1 ht) c2)
            (setf (gethash c2 ht) c1))))
    ;; performing substitution on TEXT...
    (let ((result (make-string text-len)))
      (dotimes (i text-len)
        (let* ((c (char text i))
               (lookup (gethash c ht)))
          (if (not lookup)
              (if filter
                  (error 'invalid-character-in-string :what c :why "Simple substitution cipher decryption found a character not in ALPHA2 while filtering is on!")
                  (setf (char result i) c))
              (setf (char result i) lookup))))
      result)))

(defun trifid-cipher (text &key (shift 0) (key "") (grouping 5) (grid-count-height-width 3) (replacing-chars nil) (alphabet +alphabet-with-plus+) (encrypt t))
  "Encrypts TEXT by creating x x-by-x grids where x is GRID-COUNT-HEIGHT-WIDTH, and then writing a shifted and then keyed ALPHABET across all the grids, filling each grid before adding characters to the next (following a left-to-right row-by-row top-to-bottom pattern); thus, every letter is given a 3-character coordinate (by grid-index, height-index, then width-index); then, every letter of TEXT is translated to its 3-character coordinate and (conceptually) written vertically, or stacked, with each new \"stack\" being placed to the right of the previous until there are stacks of count GROUPING; next, these stacks are read horizontally and each 3-character sequence is again looked up in the grids to get an encrypted character; after a full group is enciphered another group is created and the process continues until all text has been affected; the last group may be incomplete but is still encrypted by the same process. Note that indexing is handled automatically for the grid-lookup such that GRID-COUNT-HEIGHT-WIDTH may not exceed the length of the internally used indexing-alphabet, +ALPHABET-AND-NUMBERS+. ALPHABET must have the exact length of GRID-COUNT-HEIGHT-WIDTH to the power of 3. TEXT is downcased and all characters not in ALPHABET are lost."
  (setq alphabet (prep-alphabet alphabet))
  (when encrypt
    (setq text (prep-string text :replacing replacing-chars :filtering-by-alpha alphabet)))
  (setq key (prep-string key :replacing replacing-chars :filtering-by-alpha alphabet))
  (let* ((text-len (length text))
         (alpha-len (length alphabet))
         (indexing-alphabet +alphabet-and-numbers+) ;; is used to set GRID-ROW-COL-CHARS below ;; ! this alphabet is WAY larger than necessary for general use...
         (indexing-alphabet-length (length indexing-alphabet)))
    (when (or (< grouping 2) (not (integerp grouping)))
      (error 'invalid-number :what grouping :why "GROUPING must be an integer of value 2 or greater!"))
    (when (> grouping text-len)
      (setq grouping text-len))
    (when (or (<= grid-count-height-width 0) (not (integerp grid-count-height-width)))
      (error 'invalid-number :what grid-count-height-width :why "GRID-COUNT-HEIGHT-WIDTH must be an integer of value 1 or greater!"))
    (when (> grid-count-height-width indexing-alphabet-length)
      (error 'invalid-number :what grid-count-height-width :why (format nil "GRID-COUNT-HEIGHT-WIDTH may not exceed the value of ~d!" indexing-alphabet-length)))
    (when (/= alpha-len (expt grid-count-height-width 3))
      (error 'invalid-number-of-characters :what alphabet :why (format nil "ALPHABET must be of length GRID-COUNT-HEIGHT-WIDTH (~d) to the power of 3, meaning ~d, but is of length ~d!~%" grid-count-height-width (expt grid-count-height-width 3) alpha-len)))
    (let ((alpha (key-string (shift-string alphabet shift) key))
          (char-str-ht (make-hash-table))
          (str-char-ht (make-hash-table :test 'equal))
          (group-string-len (* 3 grouping))
          (grid-row-col-chars (subseq indexing-alphabet 0 grid-count-height-width)))
      ;; creating the hash-tables that simulate the grid lookup for each character and the reverse lookup for each coordinate-string
      (let ((i 0))
        (dotimes (gc grid-count-height-width)
          (dotimes (d grid-count-height-width)
            (dotimes (r grid-count-height-width)
              (let ((temp-str (make-array 3 :element-type 'character :initial-contents (list (char grid-row-col-chars gc) (char grid-row-col-chars d) (char grid-row-col-chars r)))))
                (setf (gethash (char alpha i) char-str-ht) temp-str)
                (setf (gethash temp-str str-char-ht) (char alpha i)))
              (incf i)))))
      (let ((result (make-string text-len))
            (result-index -1) ;; is incremented at each use
            (group-count 0) ;; used to count characters added to GROUP-STRING, but is also used to keep track of insertion index...
            (group-string (make-string group-string-len))) ;; 3-character sequences will be written into this string in steps so that it can be read iteratively to collect the secondary 3-character sequences ;; note that GROUP-STRING is simply overwritten with different group information
        (if encrypt
            (progn ;; THEN, ENCRYPTING...
              ;; iterating through text such that text is processed in groups, where each group is saved temporarily in GROUP-STRING
              (dostring (c text)
                (let ((3-char-string (gethash c char-str-ht)))
                  ;; writing to group-string in steps, thus simulating the "vertical stacks" but in 1-dimensional string format...
                  (setf (char group-string group-count) (char 3-char-string 0))
                  (setf (char group-string (+ grouping group-count)) (char 3-char-string 1))
                  (setf (char group-string (+ (* 2 grouping) group-count)) (char 3-char-string 2)))
                (incf group-count)
                (when (>= group-count grouping)
                  ;; step across GROUP-STRING by 3s grabbing each 3-character string and translating it back into a single character, saving it in RESULT
                  (do ((start 0 (+ start 3)))
                      ((>= start group-string-len))
                    (setf (char result (incf result-index)) (gethash (subseq group-string start (+ start 3)) str-char-ht)))
                  (setq group-count 0)))
              ;; when there is an incomplete grouping... 
              (when (> group-count 0)
                ;; iterate across string "rows", collecting GROUP-COUNT characters per "row"
                (do ((i 0 (1+ i)) ;; used as index for GROUP-STRING
                     (3-char-str (make-string 3))
                     (added 0) ;; used as index for 3-CHAR-STR and to count characters added to same string...
                     (end-indexes (list group-count (+ grouping group-count))) ;; a list-based set when helps to indicate when to stop reading a "row"
                     (final-index (+ (* 2 grouping) group-count)))
                    ((>= i final-index))
                  (if (member i end-indexes)
                      ;; THEN, go to next "row"
                      (setq i (- (+ i grouping) group-count 1)) ;; subtracting 1 because i increments each iteration...
                      ;; ELSE, add character to string
                      (progn
                        (setf (char 3-char-str added) (char group-string i))
                        (incf added)
                        (when (= 3 added) ;; when 3-CHAR-STR is filled out
                          (setf (char result (incf result-index)) (gethash 3-char-str str-char-ht))
                          (setq added 0)))))))
            (progn ;; ELSE, DECRYPTING...
              ;; begin decryption process, getting each character in TEXT and changing it to a 3-character sequence...
              (dostring (c1 text)
                (let ((3-char-str (gethash c1 char-str-ht)))
                  (unless 3-char-str
                    (error 'invalid-character-in-string :what c1 :why "Trifid cipher decryption detected an invalid character in TEXT."))
                  ;; ... and then writing each of these 3 characters horizontally top-to-bottom across GROUP-STRING
                  (dostring (c2 3-char-str)
                    (setf (char group-string group-count) c2)
                    (incf group-count))
                  ;; when one group is complete (meaning GROUP-STRING is filled out)
                  (when (>= group-count group-string-len)
                    (setq group-count 0) ;; reset the count/index...
                    (let ((3-char-str (make-string 3))) ;; creating a new binding for 3-CHAR-STR to prevent reference errors...
                      ;; reading vertically downward through the completed group to get the decrypted character, and placing it in RESULT
                      (dotimes (i grouping)
                        ;; reusing 3-CHAR-STR to hold a new collection of 3 characters
                        (setf (char 3-char-str 0) (char group-string i))
                        (setf (char 3-char-str 1) (char group-string (+ i grouping)))
                        (setf (char 3-char-str 2) (char group-string (+ i (* 2 grouping))))
                        ;; saving new character to RESULT
                        (setf (char result (incf result-index)) (gethash 3-char-str str-char-ht)))))))
              ;; when there is still an incomplete group...
              (when (> group-count 0)
                ;; collect the remaining incomplete group's characters into their appropriate vertical strings and then translate them to decrypted characters...
                (let ((string-count (/ group-count 3)) ;; how many strings to be collected from incomplete group
                      (3-char-str (make-string 3)))
                  (dotimes (start-i string-count)
                    (do ((iterations 0 (1+ iterations))
                         (index 0 (1+ index)) ;; this is index for 3-CHAR-STR
                         (adj 0 (+ adj string-count))) ;; adjustment to start-i
                        ((>= iterations 3))
                      (setf (char 3-char-str index) (char group-string (+ start-i adj))))
                    (setf (char result (incf result-index)) (gethash 3-char-str str-char-ht)))))))
        result))))

(defun trithemius-cipher (text &key (alphabet +alphabet+) (initial-offset 0) (replacing-chars nil) (encrypt t))
  "Encrypts TEXT by looking at each character of TEXT in sequential order, changing each character according to sum of two things: its index in TEXT, and its index in an alphabet that is shifted such that the first character of TEXT is the first character of the alphabet; the index resulting from this addition is then used on the shifted alphabet to get the encrypted character. Thus 'bd' becomes 'be' because 'b' is at TEXT-index 0 and alphabet-index 0 (yielding 0 + 0 = 0) and 'd' is at TEXT-index 1 and alphabet-index 2 (yielding 1 + 2 = 3). INITIAL-OFFSET is used in the addition process described above to effect a consistent change on the entire process, thereby allowing for a greater variety of results and also the prevention of the transparency of the first character of TEXT. Note that this cipher is equivalent to a VIGENERE-CIPHER that is keyed by an ALPHABET that is shifted by a negative INITIAL-OFFSET."
  (setq alphabet (prep-alphabet alphabet))
  (when encrypt
    (setq text (prep-string text :replacing replacing-chars :filtering-by-alpha alphabet)))
  (unless (integerp initial-offset)
    (error 'invalid-argument :what initial-offset :why "INITIAL-OFFSET must be an integer!"))
  (let ((text-len (length text))
        (alpha-len (length alphabet))
        (index-to-char-ht (make-hash-table))
        (char-to-index-ht (make-hash-table))) ;; this is simply a reverse lookup table for the previous hash table...
    (let ((result (make-string text-len)))
      (when (> text-len 0)
        ;; finding the index of the first text character within the alphabet, and adding INITIAL-OFFSET
        (let ((alpha-index (position (char text 0) alphabet)))
          ;; preparing the hash table that is used for character transformation, such that every index from 0 to (1- ALPHA-LEN) is associated with a character
          ;; this hash table preparation gives a direct lookup for the 0th index; thus, later lookups need to be adjusted according to current position
          (dotimes (x alpha-len)
            (let* ((i (mod (+ x alpha-index) alpha-len))
                   (c (char alphabet i)))
              (setf (gethash i index-to-char-ht) c)
              (setf (gethash c char-to-index-ht) i))))
        ;; enciphering TEXT by getting each character's position alphabet according to first character, and then adjusting that position by index in text (with mod) to perform new lookup
        (let ((f (if encrypt #'+ #'-)))
          (dotimes (i text-len)
            (let ((temp-index (gethash (char text i) char-to-index-ht)))
              (unless temp-index
                (error 'invalid-character-in-string :what (char text i) :why "When decrypting, every character in TEXT must be present in ALPHABET!"))
              (let ((c (gethash (mod (funcall f temp-index i initial-offset) alpha-len) index-to-char-ht)))
                (setf (char result i) c))))))
      result)))

(defun two-square-cipher (text &key (shift1 0) (key1 "") (shift2 0) (key2 "") (read-vertically t) (obscure-with-reverse-read nil) (grid-height 5) (grid-width 5) (replacing-chars (cons "j" "i")) (alphabet +alphabet-without-j+) (encrypt t) (troubleshooting nil))
  "Encrypts TEXT by creating two grids of ALPHABET where each grid is shifted and then keyed (by one of the two keys); then each two-character group of text (in order) is looked up in the grids such that the first character is found in first grid and second character is found in second grid; a rectangle is then formed (conceptually) between the two found characters (according to the alignment of the grids [vertical or horizontal]) and then the characters found at the corners (opposing corner on same grid for each character) of this rectangle are kept as the encrypted text. Encryption and decryption follow this same process. Setting READ-VERTICALLY to nil will make the grid alignment horizontal. When a line instead of a rectangle is formed during the lookup, the grid alignment can be reversed to help prevent character transparencies; set OBSCURE-WITH-REVERSE-READ to T to do this. Note that ALPHABET must be of length (* GRID-HEIGHT GRID-WIDTH), and all characters not in ALPHABET are lost."
  (setq alphabet (prep-alphabet alphabet))
  (setq key1 (prep-string key1 :replacing replacing-chars :filtering-by-alpha alphabet))
  (setq key2 (prep-string key2 :replacing replacing-chars :filtering-by-alpha alphabet))
  (when encrypt
    (setq text (prep-string text :replacing replacing-chars :filtering-by-alpha alphabet)))
  (let ((alpha1 (key-string (shift-string alphabet shift1) key1)) ;; alphabet for grid-1 (which is top or left)
        (alpha2 (key-string (shift-string alphabet shift2) key2)) ;; alphabet for grid-2 (which is bottom or right)
        (ht1 (make-hash-table)) ;; represents grid-1 (see previous two lines...)
        (ht2 (make-hash-table)) ;; represents grid-2
        (alpha-len (length alphabet))
        (text-len (length text)))
    (when (/= alpha-len (* grid-height grid-width))
      (error 'invalid-number-of-characters :what alphabet :why (format nil "ALPHABET must be of length GRID-HEIGHT (~d) times GRID-WIDTH (~d), meaning ~d, but is of length ~d!~%" grid-height grid-width (* grid-height grid-width) alpha-len)))
    (when (oddp text-len)
      (error 'odd-number-of-characters :what text-len :why (format nil "Two-square cipher only works when TEXT has an even number of characters!~%TEXT after clean-up: ~s~%Please manually add a character to your text (and ensure that the chosen character is part of the passed ALPHABET).~%" text)))
    ;; preparing the hash-tables that simulate the two-grid lookup...
    (let ((i 0))
      (dotimes (d grid-height)
        (dotimes (r grid-width)
          (setf (gethash (char alpha1 i) ht1) (cons d r))
          (setf (gethash (char alpha2 i) ht2) (cons d r))
          (incf i))))
    ;; TEST OUTPUT...
    (when troubleshooting
      (let ((l (list alpha1 alpha2)))
        (dotimes (i 2)
          (dotimes (j (* grid-height grid-width))
            (when (= (mod j grid-width) 0)
              (format t "~%"))
            (format t "~a " (elt (nth i l) j)))
          (format t "~%")))
      (format t "~%~a~%~%" text))
    ;; the below do loop steps through text collecting two characters at a time, performs the "grid lookup", and then puts the corner characters into RESULT
    (let ((result (make-string text-len))
          (result-index -1)) ;; starts at -1 because it is incremented within the #'char function at each #'setf
      (do ((i 0 (+ i 2)))
          ((>= i text-len))
        (let ((coords1 (gethash (char text i) ht1))
              (coords2 (gethash (char text (1+ i)) ht2)))
          (unless coords1
            (error 'invalid-character-in-string :what (char text i) :why "During decryption, all characters in TEXT must be present in ALPHABET!"))
          (unless coords2
            (error 'invalid-character-in-string :what (char text (1+ i)) :why "During decryption, all characters in TEXT must be present in ALPHABET!"))
          (if read-vertically
              (if (and obscure-with-reverse-read (= (cdr coords1) (cdr coords2))) ;; if a line (instead of a rectangle) is formed with the looked up characters
                  (progn ;; horizontal
                    (setf (char result (incf result-index)) (char alpha1 (+ (* grid-width (car coords2)) (cdr coords1))))
                    (setf (char result (incf result-index)) (char alpha2 (+ (* grid-width (car coords1)) (cdr coords2)))))
                  (progn ;; vertical
                    (setf (char result (incf result-index)) (char alpha1 (+ (* grid-width (car coords1)) (cdr coords2))))
                    (setf (char result (incf result-index)) (char alpha2 (+ (* grid-width (car coords2)) (cdr coords1))))))
              (if (and obscure-with-reverse-read (= (car coords1) (car coords2))) ;; again, if a line (instead of a rectangle) is formed with the looked up characters
                  (progn ;; vertical
                    (setf (char result (incf result-index)) (char alpha1 (+ (* grid-width (car coords1)) (cdr coords2))))
                    (setf (char result (incf result-index)) (char alpha2 (+ (* grid-width (car coords2)) (cdr coords1)))))
                  (progn ;; horizontal
                    (setf (char result (incf result-index)) (char alpha1 (+ (* grid-width (car coords2)) (cdr coords1))))
                    (setf (char result (incf result-index)) (char alpha2 (+ (* grid-width (car coords1)) (cdr coords2)))))))))
      result)))

(defun vigenere-cipher (text key &key (alphabet +alphabet+) (encrypt t))
  "Encrypts text by (conceptually) laying out the TEXT and the KEY (which is repeated to the length of TEXT) into aligned rows; each vertical column of two letters is then used to get a new character according to the summed (and modded) index of the TEXT character and KEY character in ALPHABET, thus simulating (in English at least) a character lookup in the tabula recta. Note that all characters not in ALPHABET are lost."
  (setq alphabet (prep-alphabet alphabet))
  (when encrypt
    (setq text (prep-string text :filtering-by-alpha alphabet)))
  (setq key (prep-string key :filtering-by-alpha alphabet))
  (when (= 0 (length key))
    (error 'invalid-key :what key :why "Vigenere cipher key must have at least one character from used alphabet!"))
  (let ((ht (make-hash-table))
        (alpha-len (length alphabet))
        (text-len (length text)))
    ;; preparing the hash-table that gives index of each character according to its position in alphabet
    (dotimes (i alpha-len)
      (setf (gethash (char alphabet i) ht) i))
    ;; extending key and preparing other data...
    (let ((key (extend-key key text-len))
          (result (make-string text-len))
          (result-index 0))
      ;; iterating through both text and key (which are now equal length), getting the index of each character and then combining them to get a new index from which a new character is gotten...
      (dotimes (i text-len)
        (let ((text-i (gethash (char text i) ht)) ;; text-i may be NIL for decryption because TEXT was not prepped... this is addressed below...
              (key-i (gethash (char key i) ht)))
          (if encrypt
              (setf (char result result-index) (char alphabet (mod (+ text-i key-i) alpha-len)))
              (if text-i
                  (setf (char result result-index) (char alphabet (mod (+ (- alpha-len key-i) text-i) alpha-len)))
                  (error 'invalid-character-in-string :what (char text i) :why "Vigenere cipher decryption detected a character not included in the used alphabet!")))
          (incf result-index)))
      result)))


(defpackage :clyptic/tests/general
  (:use :cl :clyptic :rove))
(in-package :clyptic/tests/general)

(deftest alphabets
  (testing "all alphabet constants have original string-literals..."
    (ok (string= "abcdefghijklmnopqrstuvwxyz" +alphabet+))
    (ok (string= "abcdefghiklmnopqrstuvwxyz" +alphabet-without-j+))
    (ok (string= "abcdefghijklmnoprstuvwxyz" +alphabet-without-q+))
    (ok (string= "abcdefghiklmnopqrstuwxyz" +alphabet-without-j-and-v+))
    (ok (string= "abcdefghijklmnopqrstuvwxyz+" +alphabet-with-plus+))
    (ok (string= "abcdefghijklmnopqrstuvwxyz0123456789" +alphabet-and-numbers+))))

(deftest make-hash-table-set-from-str
  (let ((result (make-hash-table-set-from-str "ab")))
    (testing "*** basics (input of \"ab\") *** output is hash-table"
      (ok (hash-table-p result)))
    (testing "hash-table has correct length"
      (ok (= 2 (hash-table-count result))))
    (testing "hash-table has exact contents"
      (ok (gethash #\a result))
      (ok (gethash #\b result)))
    (testing "hash-table uses T for all values"
      (ok (eq T (gethash #\a result)))
      (ok (eq T (gethash #\b result)))))
  (let ((result (make-hash-table-set-from-str "aa")))
    (testing "*** handles duplicates (input of \"aa\") *** hash-table has correct length"
      (ok (= 1 (hash-table-count result))))
    (testing "hash-table has exact contents"
      (ok (gethash #\a result))))
  (let ((result (make-hash-table-set-from-str "aA")))
    (testing "*** handles cases (input of \"aA\") *** hash-table has correct length"
      (ok (= 2 (hash-table-count result))))
    (testing "hash-table has exact contents"
      (ok (gethash #\a result))
      (ok (gethash #\A result)))))

(deftest make-hash-table-set-from-seq
  (let ((result (make-hash-table-set-from-seq (vector #\a #\b))))
    (testing "*** basic tests (input like #(a b)) *** output is hash-table"
      (ok (hash-table-p result)))
    (testing "hash-table has correct length"
      (ok (= 2 (hash-table-count result))))
    (testing "hash-table has exact contents"
      (ok (gethash #\a result))
      (ok (gethash #\b result)))
    (testing "hash-table uses T for all values"
      (ok (eq T (gethash #\a result)))
      (ok (eq T (gethash #\b result)))))
  (let ((result (make-hash-table-set-from-seq (vector #\a #\a))))
    (testing "*** handles duplicates (input like #(a a)) *** hash-table has correct length"
      (ok (= 1 (hash-table-count result))))
    (testing "hash-table has exact contents"
      (ok (gethash #\a result))))
  (let ((result (make-hash-table-set-from-seq (vector #\a #\A))))
    (testing "*** handles case (input like #(a A)) *** hash-table has correct length"
      (ok (= 2 (hash-table-count result))))
    (testing "hash-table has exact contents"
      (ok (gethash #\a result))
      (ok (gethash #\A result)))))

(deftest make-hash-table-set-from-list
  (let ((result (make-hash-table-set-from-list (list #\a #\b))))
    (testing "*** basic tests (input like '(a b)) *** output is hash-table"
      (ok (hash-table-p result)))
    (testing "hash-table has correct length"
      (ok (= 2 (hash-table-count result))))
    (testing "hash-table has exact contents"
      (ok (gethash #\a result))
      (ok (gethash #\b result)))
    (testing "hash-table uses T for all values"
      (ok (eq T (gethash #\a result)))
      (ok (eq T (gethash #\b result)))))
  (let ((result (make-hash-table-set-from-list (list #\a #\a))))
    (testing "*** handles duplicates (input like '(a a)) *** hash-table has correct length"
      (ok (= 1 (hash-table-count result))))
    (testing "hash-table has exact contents"
      (ok (gethash #\a result))))
  (let ((result (make-hash-table-set-from-list (list #\a #\A))))
    (testing "*** handles case (input like '(a A)) *** hash-table has correct length"
      (ok (= 2 (hash-table-count result))))
    (testing "hash-table has exact contents"
      (ok (gethash #\a result))
      (ok (gethash #\A result)))))

(deftest prep-alphabet
  (testing "duplicates are removed"
    (ok (string= "abc" (prep-alphabet "abbc"))))
  (testing "downcasing functionality works"
    (ok (string= "abc" (prep-alphabet "ABC")))
    (ok (string= "AbC" (prep-alphabet "AbC" :downcasing nil))))
  (testing "duplicates are removed in accordance with downcasing functionality"
    (ok (string= "abc" (prep-alphabet "abBc")))
    (ok (string= "abBc" (prep-alphabet "abBc" :downcasing nil))))
  (testing "duplicates are removed such that only first instance is kept"
    (ok (string= "abc" (prep-alphabet "abcbb")))
    (ok (string= "abc" (prep-alphabet "abcB")))
    (ok (string= "abcB" (prep-alphabet "abcaB" :downcasing nil))))
  (testing "makes no in-place changes"
    (let ((abbc "abbc"))
      (ok (string/= abbc (prep-alphabet abbc)))
      (ok (string= abbc "abbc")))))

(deftest prep-string
  (testing "testing downcasing..."
    (ok (string= "abc" (prep-string "Abc")))
    (ok (string= "Abc" (prep-string "Abc" :downcasing nil))))
  (testing "testing replacing (with and without downcasing)..."
    (ok (string= "az" (prep-string "ab" :replacing (cons "b" "z"))))
    (ok (string= "az" (prep-string "aB" :replacing (cons "b" "z"))))
    (ok (string= "az" (prep-string "ab" :replacing (cons "B" "z"))))
    (ok (string= "az" (prep-string "ab" :replacing (cons "b" "Z"))))
    (ok (string= "az" (prep-string "ab" :replacing (cons "B" "Z"))))
    (ok (string= "az" (prep-string "aB" :replacing (cons "B" "Z"))))
    (ok (string= "ab" (prep-string "aB" :replacing (cons "c" "Z"))))
    (ok (string= "aZ" (prep-string "aB" :downcasing nil :replacing (cons "B" "Z"))))
    (ok (string= "ab" (prep-string "ab" :downcasing nil :replacing (cons "B" "Z"))))
    (ok (string= "aB" (prep-string "aB" :downcasing nil :replacing (cons "b" "z"))))
    (ok (string= "az" (prep-string "aB" :downcasing nil :replacing (cons "B" "z"))))
    (ok (string= "xyc" (prep-string "abc" :replacing (cons "ab" "xy"))))
    (ok (string= "bcdebcde" (prep-string "abcdabcd" :replacing (cons "abcd" "bcde")))))
  (testing "REPLACING must be a CONS"
    (ok (signals (prep-string "ab" :replacing "wrong") 'invalid-argument)))
  (testing "REPLACING must be a CONS where each element is a string"
    (ok (signals (prep-string "ab" :replacing (cons 1 "x")) 'invalid-argument))
    (ok (signals (prep-string "ab" :replacing (cons "a" 2)) 'invalid-argument)))
  (testing "REPLACING must be a CONS where each element is a string of exactly the same length"
    (ok (signals (prep-string "ab" :replacing (cons "abc" "x")) 'invalid-argument))
    (ok (signals (prep-string "ab" :replacing (cons "a" "xyz")) 'invalid-argument)))
  (testing "The CAR string of REPLACING must have no duplicates"
    (ok (signals (prep-string "ab" :replacing (cons "aba" "xyz")) 'invalid-argument)))
  (testing "The CDR string of REPLACING must use only characters from ALPHABET when filtering"
    (ok (signals (prep-string "abc" :replacing (cons "abc" "def") :filtering-by-alpha "abcde") 'invalid-argument))
    (ok (signals (prep-string "ab" :replacing (cons "b" "z") :filtering-by-alpha "ab") 'invalid-argument)))
  (testing "testing filtering..."
    (ok (string= "a" (prep-string "ab" :filtering-by-alpha "a")))
    (ok (string= "" (prep-string "ab" :filtering-by-alpha "A"))))
  (testing "testing error-on-filter"
    (ok (signals (prep-string "ab" :filtering-by-alpha "a" :error-on-filter t) 'invalid-character-in-string))))

(deftest prep-string-by-removal
  (testing "removes whitespace from TEXT"
    (ok (string= "abcd" (prep-string-by-removal (coerce (list #\a #\space #\b #\tab #\c #\return #\d) 'string)))))
  (testing "can leave whitespace in TEXT"
    (ok (string= " s p a c i n g " (prep-string-by-removal " s p a c i n g " :remove-whitespace nil))))
  (testing "removes punctuation from TEXT"
    (ok (string= "hellothere" (prep-string-by-removal (coerce (list #\h #\! #\e #\- #\l #\' #\l #\" #\o #\? #\t #\. #\h #\, #\e #\; #\r #\: #\e) 'string)))))
  (testing "can leave punctuation in TEXT"
    (ok (string= "h!e-l'l\"o?t.h,e;r:e" (prep-string-by-removal (coerce (list #\h #\! #\e #\- #\l #\' #\l #\" #\o #\? #\t #\. #\h #\, #\e #\; #\r #\: #\e) 'string) :remove-punctuation nil))))
  (testing "removes custom characters from TEXT"
    (ok (string= "ad" (prep-string-by-removal "abcd" :list-of-other-chars-to-remove (list #\b #\c)))))
  (testing "downcases TEXT"
    (ok (string= "abc" (prep-string-by-removal "ABC"))))
  (testing "also downcases LIST-OF-OTHER-CHARS-TO-REMOVE when downcasing"
    (ok (string= "ad" (prep-string-by-removal "abcd" :list-of-other-chars-to-remove (list #\B #\C)))))
  (testing "does not downcase LIST-OF-OTHER-CHARS-TO-REMOVE when not downcasing"
    (ok (string= "abcd" (prep-string-by-removal "abcd" :downcase-text nil :list-of-other-chars-to-remove (list #\B #\C))))))

(deftest remove-characters-from-string
  (testing "works as expected"
    (ok (string= "abc" (remove-characters-from-string "aBbcd" (list #\B #\d))))
    (ok (string= "abc" (remove-characters-from-string "abc" (list #\x #\y #\z))))
    (ok (string= "abc" (remove-characters-from-string "abc" nil)))
    (ok (string= "" (remove-characters-from-string "" nil)))))

(deftest get-random-letter
  (let ((works t)
        (alpha "abcd"))
    (dotimes (i 20)
      (let ((result (get-random-letter alpha)))
        (if (and (characterp result) (find result alpha))
            nil ;; it works
            (setq works nil))))
    (if works
        (pass "It works...")
        (fail "It failed!"))))

(deftest filter-string-by-hash
  (testing "basic tests..."
    (let ((ht (make-hash-table)))
      (setf (gethash #\b ht) t)
      (setf (gethash #\c ht) t)
      (ok (string= "bc" (filter-string-by-hash "abcd" ht)))
      (ok (string= "c" (filter-string-by-hash "aBcd" ht))))))

(deftest key-string
  (testing "basics"
    (ok (string= "zab" (key-string "ab" "z")))
    (ok (string= "catbd" (key-string "abcd" "cat"))))
  (testing "duplicates are removed from text and key"
    (ok (string= "xab" (key-string "aabbbbb" "x")))
    (ok (string= "xab" (key-string "ab" "xxxxxxxx")))
    (ok (string= "xab" (key-string "aaabbbb" "xxxxx"))))
  (testing "restricting alphabet filters both text and key"
    (ok (string= "qab" (key-string "abc" "q" :restricting-alphabet "abq")))
    (ok (string= "qab" (key-string "ab" "qr" :restricting-alphabet "abq")))
    (ok (string= "qab" (key-string "abcd" "qrst" :restricting-alphabet "abefpq")))))

(deftest shift-string
  (testing "STRING-TO-BE-SHIFTED can be an empty string"
    (ok (string= "" (shift-string "" 0)))
    (ok (string= "" (shift-string "" 5))))
  (testing "no shift"
    (ok (string= "abc" (shift-string "abc" 0))))
  (testing "positive"
    (ok (string= "cab" (shift-string "abc" 1)))
    (ok (string= "bca" (shift-string "abc" 2))))
  (testing "negative"
    (ok (string= "bca" (shift-string "abc" -1)))
    (ok (string= "cab" (shift-string "abc" -2))))
  (testing "full rotation"
    (ok (string= "abc" (shift-string "abc" 3)))
    (ok (string= "abc" (shift-string "abc" -3))))
  (testing "large shift values"
    (ok (string= "abc" (shift-string "abc" 999)))
    (ok (string= "cab" (shift-string "abc" 1000)))
    (ok (string= "abc" (shift-string "abc" -999)))
    (ok (string= "bca" (shift-string "abc" -1000)))))

(deftest extend-key
  (testing "no change"
    (ok (string= "abc" (extend-key "abc" 3))))
  (testing "extension"
    (ok (string= "abcabca" (extend-key "abc" 7)))
    (ok (string= "abcabc" (extend-key "abc" 6))))
  (testing "shrinkage"
    (ok (string= "ab" (extend-key "abc" 2)))
    (ok (string= "" (extend-key "abc" 0)))))

(deftest make-char-pos-hash-table-from-str
  (testing "works as expected (input of \"aBc\")"
    (let ((result (make-char-pos-hash-table-from-str "aBc")))
      (ok (= 3 (hash-table-count result)))
      (ok (= 0 (gethash #\a result)))
      (ok (= 1 (gethash #\B result)))
      (ok (= 2 (gethash #\c result)))))
  (testing "duplicate characters are not allowed"
    (ok (signals (make-char-pos-hash-table-from-str "test") 'duplicate-character-in-string))))

(deftest get-alphabet-based-index-list-from-str
  (testing "returns list"
    (ok (listp (get-alphabet-based-index-list-from-str "abc"))))
  (testing "works as expected"
    (ok (equal (list 0 4 3 1 2) (get-alphabet-based-index-list-from-str "apple")))
    (ok (equal (list 2 1 0 3) (get-alphabet-based-index-list-from-str "vnax")))
    (ok (equal (list 0 5 1 2 4 3) (get-alphabet-based-index-list-from-str "abbcba")))
    (ok (equal (list 1 2 0) (get-alphabet-based-index-list-from-str "zxy"))))
  (testing "passed text must all exist within alphabet"
    (ok (signals (get-alphabet-based-index-list-from-str "abc9") 'invalid-character-in-string))
    (ok (signals (get-alphabet-based-index-list-from-str "ab" :alphabet "a") 'invalid-character-in-string))))

(deftest get-order-array-from-alphabet-based-index-list
  (testing "returns an array"
    (ok (arrayp (get-order-array-from-alphabet-based-index-list (list 0 4 3 1 2)))))
  (testing "works as expected"
    (ok (equalp (vector 0 3 4 2 1) (get-order-array-from-alphabet-based-index-list (list 0 4 3 1 2))))
    (ok (equalp (vector 2 1 0 3) (get-order-array-from-alphabet-based-index-list (list 2 1 0 3))))
    (ok (equalp (vector 2 0 1) (get-order-array-from-alphabet-based-index-list (list 1 2 0))))
    (ok (equalp (vector 0 5 1 2 4 3) (get-order-array-from-alphabet-based-index-list (list 0 2 3 5 4 1))))))

(deftest disrupt-text
  (testing "a string is returned"
    (ok (stringp (disrupt-text "hi" "ab" "B"))))
  (testing "requires key of certain size (2 or greater)"
    (ok (signals (disrupt-text "hi" "a" "B") 'invalid-key))
    (ok (signals (disrupt-text "hi" "" "B") 'invalid-key)))
  (testing "works as expected"
    (ok (string= "BhBBi" (disrupt-text "hi" "ab" "B")))
    (ok (string= "BhBelBBlBotBBhBerBBe" (disrupt-text "hellothere" "abc" "B")))
    (ok (string= "thaiaasisaamaoaareianvaoaalveadtaeaast" (disrupt-text "thisisamoreinvolvedtest" "vnax" "a"))))
  (testing "randomly adds characters from disruption-string (POOR TEST!)"
    (let ((result (disrupt-text "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "ab" "BC")))
      (ok (find #\B result))
      (ok (find #\C result))))
  (testing "requires that every character in key be within alphabet"
    (ok (signals (disrupt-text "a" "a9" "abc") 'invalid-character-in-string))))

(deftest remove-disruption-from-text
  (testing "a string is returned"
    (ok (stringp (remove-disruption-from-text "Aa" "ab"))))
  (testing "an empty string passed as TEXT does not cause an error"
    (ok (string= "" (remove-disruption-from-text "" "abc"))))
  (testing "basic functionality"
    (ok (string= "a" (remove-disruption-from-text "Aa" "ab")))
    (ok (string= "hi" (remove-disruption-from-text "BhBBi" "ab")))
    (ok (string= "hi" (remove-disruption-from-text "zhxni" "ab")))
    (ok (string= "hellothere" (remove-disruption-from-text "BhBelBBlBotBBhBerBBe" "abc")))
    (ok (string= "thisisamoreinvolvedtest" (remove-disruption-from-text "thaiaasisaamaoaareianvaoaalveadtaeaast" "vnax"))))
  (testing "requires key of certain size (2 or greater)"
    (ok (signals (remove-disruption-from-text "Aa" "") 'invalid-key))
    (ok (signals (remove-disruption-from-text "Aa" "a") 'invalid-key)))
  (testing "requires that every character in key be within alphabet"
    (ok (signals (remove-disruption-from-text "Aa" "a9") 'invalid-character-in-string))))

(deftest separate-repeating-characters
  (testing "a string is returned"
    (ok (stringp (separate-repeating-characters "a" #\b))))
  (testing "can receive an empty string as TEXT"
    (ok (string= "" (separate-repeating-characters "" #\b))))
  (testing "works as expected"
    (ok (string= "a" (separate-repeating-characters "a" #\b)))
    (ok (string= "aba" (separate-repeating-characters "aa" #\b)))
    (ok (string= "ababcb" (separate-repeating-characters "aabb" #\b #\c)))
    (ok (string= "lit@tle ap@ple" (separate-repeating-characters "little apple" #\@))))
  (testing "signals error when SECONDARY-CHAR is the same as ADDED-CHAR"
    (ok (signals (separate-repeating-characters "aabb" #\b #\b) 'invalid-argument)))
  (testing "signals error when SECONDARY-CHAR is needed but not provided"
    (ok (signals (separate-repeating-characters "aabb" #\b) 'invalid-argument))))

(deftest distinguish-by-groups-of-two
  (testing "a string is returned"
    (ok (stringp (distinguish-by-groups-of-two "ab" #\c #\d))))
  (testing "can receive an empty string as TEXT"
    (ok (string= "" (distinguish-by-groups-of-two "" #\c #\d))))
  (testing "works as expected"
    (ok (string= "aba" (distinguish-by-groups-of-two "aa" #\b #\c :make-string-even nil)))
    (ok (string= "abab" (distinguish-by-groups-of-two "aa" #\b #\c :make-string-even t)))
    (ok (string= "bcb" (distinguish-by-groups-of-two "bb" #\b #\c :make-string-even nil)))
    (ok (string= "bcbc" (distinguish-by-groups-of-two "bb" #\b #\c :make-string-even t)))
    (ok (string= "ababbxbx" (distinguish-by-groups-of-two "aabbb" #\b #\x :make-string-even t)))
    (ok (string= "acabbcbccxcxcddcdcdcdc" (distinguish-by-groups-of-two "aabbbccccddddd" #\c #\x :make-string-even t)))
    (ok (string= "addle" (distinguish-by-groups-of-two "addle" #\b #\c :make-string-even nil)))
    (ok (string= "addleb" (distinguish-by-groups-of-two "addle" #\b #\c :make-string-even t))))
  (testing "ADDED-CHAR and SECONDARY-CHAR must be unique in relation to each other"
    (ok (signals (distinguish-by-groups-of-two "" #\a #\a) 'invalid-argument))))

(deftest put-in-string
  (testing "returns a string"
    (ok (stringp (put-in-string "a" (make-array 4 :element-type 'character :initial-contents (list #\0 #\1 #\2 #\3))))))
  (testing "accepts empty strings"
    (ok (string= "abc" (put-in-string "" "abc")))
    (ok (string= "" (put-in-string "a" "")))
    (ok (string= "" (put-in-string "" ""))))
  (testing "works as expected"
    (ok (string= "a123" (put-in-string "a" (make-array 4 :element-type 'character :initial-contents (list #\0 #\1 #\2 #\3)))))
    (ok (string= "0b23" (put-in-string "b" (make-array 4 :element-type 'character :initial-contents (list #\0 #\1 #\2 #\3)) 1)))
    (ok (string= "01ab" (put-in-string "ab" (make-array 4 :element-type 'character :initial-contents (list #\0 #\1 #\2 #\3)) 2)))
    (ok (string= "01a" (put-in-string "ab" (make-array 3 :element-type 'character :initial-contents (list #\0 #\1 #\2)) 2)))
    (ok (string= "0123456789" (put-in-string "5678" (make-array 10 :element-type 'character :initial-contents (list #\0 #\1 #\2 #\3 #\4 #\H #\E #\R #\E #\9)) 5)))
    (ok (string= "a" (put-in-string "abcd" (make-array 1 :element-type 'character :initial-contents (list #\0))))))
  (testing "negative index is not allowed"
    (ok (signals (put-in-string "abc" (make-array 4 :element-type 'character :initial-contents (list #\0 #\1 #\2 #\3)) -1) 'invalid-argument)))
  (testing "index may be beyond bounds of string"
    (ok (string= "012" (put-in-string "abc" (make-array 3 :element-type 'character :initial-contents (list #\0 #\1 #\2)) 3)))
    (ok (string= "012" (put-in-string "abc" (make-array 3 :element-type 'character :initial-contents (list #\0 #\1 #\2)) 999)))))

(defpackage :clyptic/tests/ciphers
  (:use :cl :clyptic :rove))
(in-package :clyptic/tests/ciphers)

(deftest adfgvx-cipher-simple_EN
  (testing "returns a string"
    (ok (stringp (adfgvx-cipher-simple "a"))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (adfgvx-cipher-simple ""))))
  (testing "basics"
    (ok (string= "aa" (adfgvx-cipher-simple "a")))
    (ok (string= "aaadaf" (adfgvx-cipher-simple "abc"))))
  (testing "downcases TEXT"
    (ok (string= "aaadaf" (adfgvx-cipher-simple "ABc"))))
  (testing "filters TEXT"
    (ok (string= "aa" (adfgvx-cipher-simple "#a."))))
  (testing "full-grid"
    (ok (string= "aaadafagavaxdadddfdgdvdxfafdfffgfvfxgagdgfgggvgxvavdvfvgvvvxxaxdxfxgxvxx" (adfgvx-cipher-simple +alphabet-and-numbers+))))
  (testing "more realistic examples"
    (ok (string= "ddavdxdxffavggavfxvafffdav" (adfgvx-cipher-simple "Hello, everyone!")))
    (ok (string= "dfgaavavvxvvafaagdga" (adfgvx-cipher-simple "I see 32 cats!")))))

(deftest adfgvx-cipher-simple_DE
  (testing "returns a string"
    (ok (stringp (adfgvx-cipher-simple "aa" :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (adfgvx-cipher-simple "" :encrypt nil))))
  (testing "basics"
    (ok (string= "a" (adfgvx-cipher-simple "aa" :encrypt nil)))
    (ok (string= "abc" (adfgvx-cipher-simple "aaadaf" :encrypt nil))))
  (testing "requires even number of characters in TEXT"
    (ok (signals (adfgvx-cipher-simple "aaa" :encrypt nil) 'odd-number-of-characters)))
  (testing "only allows characters from LETTERS string (and also disallows upper-case)"
    (ok (signals (adfgvx-cipher-simple "aA" :encrypt nil) 'invalid-character-in-string))
    (ok (signals (adfgvx-cipher-simple "#a" :encrypt nil) 'invalid-character-in-string)))
  (testing "full-grid"
    (ok (string= +alphabet-and-numbers+ (adfgvx-cipher-simple "aaadafagavaxdadddfdgdvdxfafdfffgfvfxgagdgfgggvgxvavdvfvgvvvxxaxdxfxgxvxx" :encrypt nil))))
  (testing "more realistic examples"
    (ok (string= "helloeveryone" (adfgvx-cipher-simple "ddavdxdxffavggavfxvafffdav" :encrypt nil)))
    (ok (string= "isee32cats" (adfgvx-cipher-simple "dfgaavavvxvvafaagdga" :encrypt nil)))))

(deftest adfgx-cipher-simple_EN
  (testing "returns a string"
    (ok (stringp (adfgx-cipher-simple "a"))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (adfgx-cipher-simple ""))))
  (testing "basics"
    (ok (string= "aa" (adfgx-cipher-simple "a")))
    (ok (string= "aaadaf" (adfgx-cipher-simple "abc"))))
  (testing "downcases TEXT"
    (ok (string= "aaadaf" (adfgx-cipher-simple "ABc"))))
  (testing "filters TEXT"
    (ok (string= "aa" (adfgx-cipher-simple "#a."))))
  (testing (format nil "~s and ~s are equivalent to this cipher" #\i #\j)
    (ok (string= (adfgx-cipher-simple "i") (adfgx-cipher-simple "j"))))
  (testing "full-grid"
    (ok (string= "aaadafagaxdadddfdgdxfafdfffgfxgagdgfgggxxaxdxfxgxx" (adfgx-cipher-simple +alphabet-without-j+))))
  (testing "more realistic examples"
    (ok (string="dfaxfafafgaxxaaxgdxgfgffax" (adfgx-cipher-simple "Hello, everyone!")))
    (ok (string= "dgdggxfdfxaxagggaxffdaaxaxgg" (adfgx-cipher-simple "I jumped 10 (ten) feet.")))))

(deftest adfgx-cipher-simple_DE
  (testing "returns a string"
    (ok (stringp (adfgx-cipher-simple "aa" :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (adfgx-cipher-simple "" :encrypt nil))))
  (testing "basics"
    (ok (string= "a" (adfgx-cipher-simple "aa" :encrypt nil)))
    (ok (string= "abc" (adfgx-cipher-simple "aaadaf" :encrypt nil))))
  (testing "requires even number of characters in TEXT"
    (ok (signals (adfgx-cipher-simple "aaa" :encrypt nil) 'odd-number-of-characters)))
  (testing "only allows characters from LETTERS string (and also disallows upper-case)"
    (ok (signals (adfgx-cipher-simple "aA" :encrypt nil) 'invalid-character-in-string))
    (ok (signals (adfgx-cipher-simple "#a" :encrypt nil) 'invalid-character-in-string)))
  (testing (format nil "~s relates to ~s only (not ~s)" "dg" #\i #\j)
    (ok (string= "i" (adfgx-cipher-simple "dg" :encrypt nil))))
  (testing "full-grid"
    (ok (string= +alphabet-without-j+ (adfgx-cipher-simple "aaadafagaxdadddfdgdxfafdfffgfxgagdgfgggxxaxdxfxgxx" :encrypt nil))))
  (testing "more realistic examples"
    (ok (string= "helloeveryone" (adfgx-cipher-simple "dfaxfafafgaxxaaxgdxgfgffax" :encrypt nil)))
    (ok (string= "iiumpedtenfeet" (adfgx-cipher-simple "dgdggxfdfxaxagggaxffdaaxaxgg" :encrypt nil)))))

(deftest affine-cipher_EN
  (testing "returns a string"
    (ok (stringp (affine-cipher "a"))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (affine-cipher ""))))
  (testing "basics"
    (ok (string= "a" (affine-cipher "a")))
    (ok (string= "b" (affine-cipher "a" :added-v 1)))
    (ok (string= "d" (affine-cipher "b")))
    (ok (string= "f" (affine-cipher "b" :added-v 2)))
    (ok (string= "f" (affine-cipher "b" :prime-v 5)))
    (ok (string= "i" (affine-cipher "b" :prime-v 5 :added-v 3)))
    (ok (string= "adg" (affine-cipher "abc")))
    (ok (string= "fil" (affine-cipher "abc" :added-v 5)))
    (ok (string= "afk" (affine-cipher "abc" :prime-v 5))))
  (testing "downcases TEXT"
    (ok (string= "a" (affine-cipher "A"))))
  (testing "filters TEXT according to ALPHABET"
    (ok (string= "a" (affine-cipher "#a."))))
  (testing "PRIME-V must be prime"
    (ok (signals (affine-cipher "a" :prime-v 4) 'invalid-number))
    (ok (signals (affine-cipher "a" :prime-v -3) 'invalid-number)))
  (testing "PRIME-V must be co-prime to length of ALPHABET"
    (ok (signals (affine-cipher "a" :prime-v 13) 'invalid-number))
    (ok (signals (affine-cipher "a" :prime-v 3 :alphabet "abcdef") 'invalid-number)))
  (testing "PRIME-V cannot be larger than 100"
    (ok (signals (affine-cipher "a" :prime-v 103) 'invalid-number)))
  (testing "works with very large positive or negative ADDED-V"
    (ok (string= "beh" (affine-cipher "abc" :added-v 1)))
    (ok (string= "beh" (affine-cipher "abc" :added-v (1+ (* 26 10020121)))))
    (ok (string= "beh" (affine-cipher "abc" :added-v (1+ (* 26 10020122)))))
    (ok (string= "beh" (affine-cipher "abc" :added-v (1+ (* 26 9899827394782984)))))
    (ok (string= "zcf" (affine-cipher "abc" :added-v -1)))
    (ok (string= "zcf" (affine-cipher "abc" :added-v (1- (* 26 -109398211)))))
    (ok (string= "zcf" (affine-cipher "abc" :added-v (1- (* 26 -109398212)))))
    (ok (string= "zcf" (affine-cipher "abc" :added-v (1- (* 26 -912872987342348))))))
  (testing "more complex examples"
    (ok (string= "ihhwvcswfrcp" (affine-cipher "Affine Cipher..." :prime-v 5 :added-v 8)))
    (ok (string= "hlvajtcqvoxztzivhqqrloijohuv" (affine-cipher "Is example 2 (two) more illustrative?" :added-v 9)))
    (ok (string= "afe" (affine-cipher "aBcxyz" :prime-v 5 :alphabet "abcdef")))))

(deftest affine-cipher_DE
  (testing "returns a string"
    (ok (stringp (affine-cipher "a" :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (affine-cipher "" :encrypt nil))))
  (testing "basics"
    (ok (string= "a" (affine-cipher "a" :encrypt nil)))
    (ok (string= "a" (affine-cipher "b" :added-v 1 :encrypt nil)))
    (ok (string= "b" (affine-cipher "d" :encrypt nil)))
    (ok (string= "b" (affine-cipher "f" :added-v 2 :encrypt nil)))
    (ok (string= "b" (affine-cipher "f" :prime-v 5 :encrypt nil)))
    (ok (string= "b" (affine-cipher "i" :prime-v 5 :added-v 3 :encrypt nil)))
    (ok (string= "abc" (affine-cipher "adg" :encrypt nil)))
    (ok (string= "abc" (affine-cipher "fil" :added-v 5 :encrypt nil)))
    (ok (string= "abc" (affine-cipher "afk" :prime-v 5 :encrypt nil))))
  (testing "only works with characters in ALPHABET"
    (ok (signals (affine-cipher "A" :encrypt nil) 'invalid-character-in-string))
    (ok (signals (affine-cipher "9" :encrypt nil) 'invalid-character-in-string)))
  (testing "PRIME-V must be prime"
    (ok (signals (affine-cipher "a" :prime-v 4 :encrypt nil) 'invalid-number))
    (ok (signals (affine-cipher "a" :prime-v -3 :encrypt nil) 'invalid-number)))
  (testing "PRIME-V must be co-prime to length of ALPHABET"
    (ok (signals (affine-cipher "a" :prime-v 13 :encrypt nil) 'invalid-number))
    (ok (signals (affine-cipher "a" :prime-v 3 :alphabet "abcdef" :encrypt nil) 'invalid-number)))
  (testing "PRIME-V cannot be larger than 100"
    (ok (signals (affine-cipher "a" :prime-v 103 :encrypt nil) 'invalid-number)))
  (testing "works with very large positive or negative ADDED-V"
    (ok (string= "abc" (affine-cipher "beh" :added-v 1 :encrypt nil)))
    (ok (string= "abc" (affine-cipher "beh" :added-v (1+ (* 26 10020121)) :encrypt nil)))
    (ok (string= "abc" (affine-cipher "beh" :added-v (1+ (* 26 10020122)) :encrypt nil)))
    (ok (string= "abc" (affine-cipher "beh" :added-v (1+ (* 26 9899827394782984)) :encrypt nil)))
    (ok (string= "abc" (affine-cipher "zcf" :added-v -1 :encrypt nil)))
    (ok (string= "abc" (affine-cipher "zcf" :added-v (1- (* 26 -109398211)) :encrypt nil)))
    (ok (string= "abc" (affine-cipher "zcf" :added-v (1- (* 26 -109398212)) :encrypt nil)))
    (ok (string= "abc" (affine-cipher "zcf" :added-v (1- (* 26 -912872987342348)) :encrypt nil))))
  (testing "more complex examples"
    (ok (string= "affinecipher" (affine-cipher "ihhwvcswfrcp" :prime-v 5 :added-v 8 :encrypt nil)))
    (ok (string= "isexampletwomoreillustrative" (affine-cipher "hlvajtcqvoxztzivhqqrloijohuv" :added-v 9 :encrypt nil)))
    (ok (string= "abc" (affine-cipher "afe" :prime-v 5 :alphabet "abcdef" :encrypt nil)))))

(deftest atbash-cipher_EN
  (testing "returns a string"
    (ok (stringp (atbash-cipher "a"))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (atbash-cipher ""))))
  (testing "basics"
    (ok (string= "z" (atbash-cipher "a")))
    (ok (string= "y" (atbash-cipher "b")))
    (ok (string= "zyx" (atbash-cipher "abc"))))
  (testing "downcases TEXT"
    (ok (string= "zyx" (atbash-cipher "ABc"))))
  (testing "filters TEXT"
    (ok (string= "z" (atbash-cipher "#a.")))
    (ok (string= "b" (atbash-cipher "ac" :alphabet "ab"))))
  (testing "full-alphabet"
    (ok (string= "zyxwvutsrqponmlkjihgfedcba" (atbash-cipher +alphabet+)))
    (ok (string= "cba" (atbash-cipher "abc" :alphabet "abc"))))
  (testing "more realistic examples"
    (ok (string= "svoolveviblmv" (atbash-cipher "Hello, everyone!")))
    (ok (string= "1r55gh79qr" (atbash-cipher "I see 32 cats!" :alphabet +alphabet-and-numbers+)))))

(deftest atbash-cipher_DE
  (testing "returns a string"
    (ok (stringp (atbash-cipher "z" :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (atbash-cipher "" :encrypt nil))))
  (testing "basics"
    (ok (string= "a" (atbash-cipher "z" :encrypt nil)))
    (ok (string= "b" (atbash-cipher "y" :encrypt nil)))
    (ok (string= "abc" (atbash-cipher "zyx" :encrypt nil))))
  (testing "only works with characters in ALPHABET"
    (ok (signals (atbash-cipher "A" :encrypt nil) 'invalid-character-in-string))
    (ok (signals (atbash-cipher "9" :encrypt nil) 'invalid-character-in-string))
    (ok (signals (atbash-cipher "abc" :alphabet "ab" :encrypt nil) 'invalid-character-in-string)))
  (testing "full-alphabet"
    (ok (string= +alphabet+ (atbash-cipher "zyxwvutsrqponmlkjihgfedcba" :encrypt nil)))
    (ok (string= "abc" (atbash-cipher "cba" :alphabet "abc" :encrypt nil))))
  (testing "more realistic examples"
    (ok (string= "helloeveryone" (atbash-cipher "svoolveviblmv" :encrypt nil)))
    (ok (string= "isee32cats" (atbash-cipher "1r55gh79qr" :alphabet +alphabet-and-numbers+ :encrypt nil)))))

(deftest autokey-cipher_EN
  (testing "returns a string"
    (ok (stringp (autokey-cipher "a" "a"))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (autokey-cipher "" "a"))))
  (testing "basics"
    (ok (string= "a" (autokey-cipher "a" "a")))
    (ok (string= "test" (autokey-cipher "test" "aaaa")))
    (ok (string= "tesm" (autokey-cipher "test" "aaa")))
    (ok (string= "cnce" (autokey-cipher "abcd" "cm"))))
  (testing "rotates through ALPHABET"
    (ok (string= "ax" (autokey-cipher "zy" "b"))))
  (testing "downcases TEXT"
    (ok (string= "cnce" (autokey-cipher "ABcd" "cm"))))
  (testing "downcases PRIMER"
    (ok (string= "cnce" (autokey-cipher "abcd" "Cm"))))
  (testing "filters TEXT"
    (ok (string= "cb" (autokey-cipher "#ab." "c")))
    (ok (string= "af" (autokey-cipher "beg" "f" :alphabet "abcdef"))))
  (testing "filters PRIMER"
    (ok (string= "cb" (autokey-cipher "ab" "#c.")))
    (ok (string= "af" (autokey-cipher "be" "fg" :alphabet "abcdef"))))
  (testing "PRIMER must consist of at least one character (after filtering)"
    (ok (signals (autokey-cipher "abc" "") 'invalid-key))
    (ok (signals (autokey-cipher "abc" "d" :alphabet "abc") 'invalid-key)))
  (testing "more realistic examples"
    (ok (string= "nvpphmikjfsyp" (autokey-cipher "Hello, everyone!" "greetings")))
    (ok (string= "s0xxrauexl" (autokey-cipher "I see 32 cats!" "kitty" :alphabet +alphabet-and-numbers+)))
    (ok (string= "qnxepvytwtwp" (autokey-cipher "Attack at dawn!" "queenly")))
    (ok (string= "wmpmmxxaeyhbryoca" (autokey-cipher "Meet at the fountain." "KILT")))))

(deftest autokey-cipher_DE
  (testing "returns a string"
    (ok (stringp (autokey-cipher "a" "a" :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (autokey-cipher "" "a" :encrypt nil))))
  (testing "basics"
    (ok (string= "a" (autokey-cipher "a" "a" :encrypt nil)))
    (ok (string= "test" (autokey-cipher "test" "aaaa" :encrypt nil)))
    (ok (string= "test" (autokey-cipher "tesm" "aaa" :encrypt nil)))
    (ok (string= "abcd" (autokey-cipher "cnce" "cm" :encrypt nil))))
  (testing "rotates through ALPHABET"
    (ok (string= "zy" (autokey-cipher "ax" "b" :encrypt nil))))
  (testing "downcases PRIMER"
    (ok (string= "abcd" (autokey-cipher "cnce" "Cm" :encrypt nil))))
  (testing "filters PRIMER"
    (ok (string= "ab" (autokey-cipher "cb" "#c." :encrypt nil)))
    (ok (string= "be" (autokey-cipher "af" "fg" :alphabet "abcdef" :encrypt nil))))
  (testing "PRIMER must consist of at least one character (after filtering)"
    (ok (signals (autokey-cipher "abc" "" :encrypt nil) 'invalid-key))
    (ok (signals (autokey-cipher "abc" "d" :alphabet "abc" :encrypt nil) 'invalid-key)))
  (testing "TEXT only has characters from ALPHABET"
    (ok (signals (autokey-cipher "A" "abc" :encrypt nil) 'invalid-character-in-string))
    (ok (signals (autokey-cipher "9" "abc" :encrypt nil) 'invalid-character-in-string))
    (ok (signals (autokey-cipher "abc" "abc" :alphabet "ab" :encrypt nil) 'invalid-character-in-string)))
  (testing "more realistic examples"
    (ok (string= "helloeveryone" (autokey-cipher "nvpphmikjfsyp" "greetings" :encrypt nil)))
    (ok (string= "isee32cats" (autokey-cipher "s0xxrauexl" "kitty" :alphabet +alphabet-and-numbers+ :encrypt nil)))
    (ok (string= "attackatdawn" (autokey-cipher "qnxepvytwtwp" "queenly" :encrypt nil)))
    (ok (string= "meetatthefountain" (autokey-cipher "wmpmmxxaeyhbryoca" "KILT" :encrypt nil)))))

(deftest bifid-cipher_EN
  (testing "returns a string"
    (ok (stringp (bifid-cipher "a"))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (bifid-cipher ""))))
  (testing "basics"
    (ok (string= "a" (bifid-cipher "a")))
    (ok (string= "aah" (bifid-cipher "abc"))))
  (testing "downcases TEXT"
    (ok (string= "aah" (bifid-cipher "ABc"))))
  (testing "filters TEXT"
    (ok (string= "a" (bifid-cipher "#a."))))
  (testing "TEXT gets replacement"
    (ok (string= "ii" (bifid-cipher "ij" :key "i"))))
  (testing "grid is keyed correctly"
    (ok (string= "apb" (bifid-cipher "pab" :key "app"))))
  (testing "KEY is downcased"
    (ok (string= "bac" (bifid-cipher "abc" :key "B"))))
  (testing "KEY is filtered"
    (ok (string= "bac" (bifid-cipher "abc" :key "9b"))))
  (testing "KEY gets replacement"
    (ok (string= "ia" (bifid-cipher "ia" :key "ji")))
    (ok (string= "ia" (bifid-cipher "ja" :key "ji"))))
  (testing "SHIFT works"
    (ok (string= "zan" (bifid-cipher "abc" :shift 1)))
    (ok (string= "wfc" (bifid-cipher "abc" :shift -1))))
  (testing "SHIFT works in combination with KEY"
    (ok (string= "bza" (bifid-cipher "zba" :key "b" :shift 1)))
    (ok (string= "fbc" (bifid-cipher "bfc" :key "f" :shift -1))))
  (testing (format nil "~s and ~s are equivalent in cipher with default arguments" #\i #\j)
    (ok (string= (bifid-cipher "i") (bifid-cipher "j"))))
  (testing "invalid custom arguments will cause error"
    (ok (signals (bifid-cipher "error" :grid-size -1) 'invalid-argument))
    (ok (signals (bifid-cipher "error" :grid-size 0) 'invalid-argument))
    (ok (signals (bifid-cipher "error" :grid-size 3) 'invalid-number-of-characters))
    (ok (signals (bifid-cipher "error" :alphabet "abc") 'invalid-argument))
    (ok (signals (bifid-cipher "error" :alphabet "abc" :replacing-chars nil) 'invalid-number-of-characters)))
  (testing "works with valid custom arguments"
    (ok (string= "gde" (bifid-cipher "had" :key "ae" :grid-size 3 :alphabet "abcdefghi" :replacing-chars nil)))
    (ok (string= "vv" (bifid-cipher "uv" :grid-size 1 :alphabet "v" :replacing-chars (cons "u" "v"))))
    (ok (string= "wwxy" (bifid-cipher "abcdwxyz" :shift 1 :key "x" :grid-size 2 :alphabet "wxyz" :replacing-chars nil))))
  (testing "more realistic examples"
    (ok (string="fnlvuncvdvwtp" (bifid-cipher "Hello, everyone!")))
    (ok (string= "gsldcfdtwztxey" (bifid-cipher "I jumped 10 (ten) feet.")))
    (ok (string= "uaeolwrins" (bifid-cipher "Flee at once!" :key "bgwkzqpndsioaxefclumthyvr")))
    (ok (string="tpkcskgowennzy" (bifid-cipher "Tonight at eight." :shift 8 :key "secret")))))

(deftest bifid-cipher_DE
  (testing "returns a string"
    (ok (stringp (bifid-cipher "a" :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (bifid-cipher "" :encrypt nil))))
  (testing "basics"
    (ok (string= "a" (bifid-cipher "a" :encrypt nil)))
    (ok (string= "abc" (bifid-cipher "aah" :encrypt nil))))
  (testing "only works with characters in ALPHABET"
    (ok (signals (bifid-cipher "j" :encrypt nil) 'invalid-character-in-string))
    (ok (signals (bifid-cipher "A" :encrypt nil) 'invalid-character-in-string))
    (ok (signals (bifid-cipher "9" :encrypt nil) 'invalid-character-in-string))
    (ok (signals (bifid-cipher "ab" :grid-size 1 :alphabet "a" :replacing-chars nil :encrypt nil) 'invalid-character-in-string)))
  (testing "grid is keyed correctly"
    (ok (string= "pab" (bifid-cipher "apb" :key "app" :encrypt nil))))
  (testing "KEY is downcased"
    (ok (string= "abc" (bifid-cipher "bac" :key "B" :encrypt nil))))
  (testing "KEY is filtered"
    (ok (string= "abc" (bifid-cipher "bac" :key "9b" :encrypt nil))))
  (testing "KEY gets replacement"
    (ok (string= "ia" (bifid-cipher "ia" :key "ji" :encrypt nil))))
  (testing "SHIFT works"
    (ok (string= "abc" (bifid-cipher "zan" :shift 1 :encrypt nil)))
    (ok (string= "abc" (bifid-cipher "wfc" :shift -1 :encrypt nil))))
  (testing "SHIFT works in combination with KEY"
    (ok (string= "zba" (bifid-cipher "bza" :key "b" :shift 1 :encrypt nil)))
    (ok (string= "bfc" (bifid-cipher "fbc" :key "f" :shift -1 :encrypt nil))))
  (testing "invalid custom arguments will cause error"
    (ok (signals (bifid-cipher "error" :grid-size -1 :encrypt nil) 'invalid-argument))
    (ok (signals (bifid-cipher "error" :grid-size 0 :encrypt nil) 'invalid-argument))
    (ok (signals (bifid-cipher "error" :grid-size 3 :encrypt nil) 'invalid-number-of-characters))
    (ok (signals (bifid-cipher "a" :alphabet "abc" :encrypt nil) 'invalid-argument))
    (ok (signals (bifid-cipher "a" :alphabet "abc" :replacing-chars nil :encrypt nil) 'invalid-number-of-characters)))
  (testing "works with valid custom arguments"
    (ok (string= "had" (bifid-cipher "gde" :key "ae" :grid-size 3 :alphabet "abcdefghi" :replacing-chars nil :encrypt nil)))
    (ok (string= "vv" (bifid-cipher "vv" :grid-size 1 :alphabet "v" :replacing-chars (cons "u" "v") :encrypt nil)))
    (ok (string= "wxyz" (bifid-cipher "wwxy" :shift 1 :key "x" :grid-size 2 :alphabet "wxyz" :replacing-chars nil :encrypt nil))))
  (testing "more realistic examples"
    (ok (string="helloeveryone" (bifid-cipher "fnlvuncvdvwtp" :encrypt nil)))
    (ok (string= "iiumpedtenfeet" (bifid-cipher "gsldcfdtwztxey" :encrypt nil)))
    (ok (string= "fleeatonce" (bifid-cipher "uaeolwrins" :key "bgwkzqpndsioaxefclumthyvr" :encrypt nil)))
    (ok (string="tonightateight" (bifid-cipher "tpkcskgowennzy" :shift 8 :key "secret" :encrypt nil)))))

(deftest caesar-cipher_EN
  (testing "returns a string"
    (ok (stringp (caesar-cipher "a" 0))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (caesar-cipher "" 0)))
    (ok (string= "" (caesar-cipher "" 5))))
  (testing "basics"
    (ok (string= "a" (caesar-cipher "a" 0)))
    (ok (string= "b" (caesar-cipher "a" 1)))
    (ok (string= "z" (caesar-cipher "a" -1)))
    (ok (string= "fgh" (caesar-cipher "abc" 5)))
    (ok (string= "tuv" (caesar-cipher "abc" -7))))
  (testing "downcases TEXT"
    (ok (string= "bcd" (caesar-cipher "ABc" 1))))
  (testing "filters TEXT"
    (ok (string= "a" (caesar-cipher "#a." 0)))
    (ok (string= "a" (caesar-cipher "ac" 0 :alphabet "ab"))))
  (testing "SHIFT can be very large or small"
    (ok (string= "bcd" (caesar-cipher "abc" (1+ (* 26 2938420394)))))
    (ok (string= "zab" (caesar-cipher "abc" (1- (* 26 2938420394 -1))))))
  (testing "more realistic examples"
    (ok (string= "khoorhyhubrqh" (caesar-cipher "Hello, everyone!" 3)))
    (ok (string= "9j55ut31kj" (caesar-cipher "I see 32 cats!" -9 :alphabet +alphabet-and-numbers+)))
    (ok (string= "qebnrfzhyoltkclugrjmplsboqebixwvald" (caesar-cipher "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG" 23)))))

(deftest caesar-cipher_DE
  (testing "returns a string"
    (ok (stringp (caesar-cipher "a" 0 :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (caesar-cipher "" 0 :encrypt nil)))
    (ok (string= "" (caesar-cipher "" 5 :encrypt nil))))
  (testing "basics"
    (ok (string= "a" (caesar-cipher "a" 0 :encrypt nil)))
    (ok (string= "a" (caesar-cipher "b" 1 :encrypt nil)))
    (ok (string= "a" (caesar-cipher "z" -1 :encrypt nil)))
    (ok (string= "abc" (caesar-cipher "fgh" 5 :encrypt nil)))
    (ok (string= "abc" (caesar-cipher "tuv" -7 :encrypt nil))))
  (testing "SHIFT can be very large or small"
    (ok (string= "abc" (caesar-cipher "bcd" (1+ (* 26 2938420394)) :encrypt nil)))
    (ok (string= "abc" (caesar-cipher "zab" (1- (* 26 2938420394 -1)) :encrypt nil))))
  (testing "only works with characters in ALPHABET"
    (ok (signals (caesar-cipher "A" 1 :encrypt nil) 'invalid-character-in-string))
    (ok (signals (caesar-cipher "9" 1 :encrypt nil) 'invalid-character-in-string))
    (ok (signals (caesar-cipher "ab" 1 :alphabet "a" :encrypt nil) 'invalid-character-in-string)))
  (testing "more realistic examples"
    (ok (string= "helloeveryone" (caesar-cipher "khoorhyhubrqh" 3 :encrypt nil)))
    (ok (string= "isee32cats" (caesar-cipher "9j55ut31kj" -9 :alphabet +alphabet-and-numbers+ :encrypt nil)))
    (ok (string= "thequickbrownfoxjumpsoverthelazydog" (caesar-cipher "qebnrfzhyoltkclugrjmplsboqebixwvald" 23 :encrypt nil)))))

(deftest caesar-cipher-ascii_EN
  (testing "returns a string"
    (ok (stringp (caesar-cipher-ascii "a" 0))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (caesar-cipher-ascii "" 0)))
    (ok (string= "" (caesar-cipher-ascii "" 5))))
  (testing "basics"
    (ok (string= "a" (caesar-cipher-ascii "a" 0)))
    (ok (string= "b" (caesar-cipher-ascii "a" 1)))
    (ok (string= "z" (caesar-cipher-ascii "a" -1)))
    (ok (string= "fgh" (caesar-cipher-ascii "abc" 5)))
    (ok (string= "tuv" (caesar-cipher-ascii "abc" -7))))
  (testing "retains case"
    (ok (string= "BCd" (caesar-cipher-ascii "ABc" 1))))
  (testing "retains non-alphabetic characters as is"
    (ok (string= "#b." (caesar-cipher-ascii "#a." 1))))
  (testing "SHIFT can be very large or small"
    (ok (string= "bcd" (caesar-cipher-ascii "abc" (1+ (* 26 2938420395)))))
    (ok (string= "zab" (caesar-cipher-ascii "abc" (1- (* 26 2938420397 -1))))))
  (testing "more realistic examples"
    (ok (string= "Khoor, hyhubrqh!" (caesar-cipher-ascii "Hello, everyone!" 3)))
    (ok (string= "Z jvv 32 trkj!" (caesar-cipher-ascii "I see 32 cats!" -9)))
    (ok (string= "QEB NRFZH YOLTK CLU GRJMP LSBO QEB IXWV ALD" (caesar-cipher-ascii "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG" 23)))))

(deftest caesar-cipher-ascii_DE
  (testing "returns a string"
    (ok (stringp (caesar-cipher-ascii "a" 0 :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (caesar-cipher-ascii "" 0 :encrypt nil)))
    (ok (string= "" (caesar-cipher-ascii "" 5 :encrypt nil))))
  (testing "basics"
    (ok (string= "a" (caesar-cipher-ascii "a" 0 :encrypt nil)))
    (ok (string= "a" (caesar-cipher-ascii "b" 1 :encrypt nil)))
    (ok (string= "a" (caesar-cipher-ascii "z" -1 :encrypt nil)))
    (ok (string= "abc" (caesar-cipher-ascii "fgh" 5 :encrypt nil)))
    (ok (string= "abc" (caesar-cipher-ascii "tuv" -7 :encrypt nil))))
  (testing "retains case"
    (ok (string= "ABc" (caesar-cipher-ascii "BCd" 1 :encrypt nil))))
  (testing "retains non-alphabetic characters as is"
    (ok (string= "#a." (caesar-cipher-ascii "#b." 1 :encrypt nil))))
  (testing "SHIFT can be very large or small"
    (ok (string= "abc" (caesar-cipher-ascii "bcd" (1+ (* 26 2938420355)) :encrypt nil)))
    (ok (string= "abc" (caesar-cipher-ascii "zab" (1- (* 26 2938420317 -1)) :encrypt nil))))
  (testing "more realistic examples"
    (ok (string= "Hello, everyone!" (caesar-cipher-ascii "Khoor, hyhubrqh!" 3 :encrypt nil)))
    (ok (string= "I see 32 cats!" (caesar-cipher-ascii "Z jvv 32 trkj!" -9 :encrypt nil)))
    (ok (string= "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG" (caesar-cipher-ascii "QEB NRFZH YOLTK CLU GRJMP LSBO QEB IXWV ALD" 23 :encrypt nil)))))

(deftest columnar-transposition-cipher_EN
  (testing "returns a string"
    (ok (stringp (columnar-transposition-cipher "a" "abc"))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (columnar-transposition-cipher "" "abc"))))
  (testing "basics"
    (ok (string= "acb" (columnar-transposition-cipher "abc" "ab")))
    (ok (string= "bac" (columnar-transposition-cipher "abc" "ba")))
    (ok (string= "eoeltrhlhe" (columnar-transposition-cipher "hellothere" "xbm"))))
  (testing "downcases TEXT"
    (ok (string= "acb" (columnar-transposition-cipher "ABc" "ab"))))
  (testing "filters TEXT"
    (ok (string= "a" (columnar-transposition-cipher "#a." "abc"))))
  (testing "KEY is downcased"
    (ok (string= "acb" (columnar-transposition-cipher "abc" "AB"))))
  (testing "KEY is filtered"
    (ok (string= "acb" (columnar-transposition-cipher "abc" "9ab"))))
  (testing "KEY must have at least 2 characters after filtering"
    (ok (signals (columnar-transposition-cipher "abc" "a") 'invalid-key))
    (ok (signals (columnar-transposition-cipher "abc" "a9") 'invalid-key))
    (ok (signals (columnar-transposition-cipher "abc" "!@#$") 'invalid-key))
    (ok (signals (columnar-transposition-cipher "abc" "de" :alphabet "abcd") 'invalid-key)))
  (testing "works with KEY that has repeating characters"
    (ok (string= "acb" (columnar-transposition-cipher "abc" "aa"))))
  (testing "works with KEY that is larger than text"
    (ok (string= "fclenoteea" (columnar-transposition-cipher "Flee at once!" "bgwkzqpndsioaxefclumthyvr"))))
  (testing "IRREGULAR argument works as expected"
    (let ((result (columnar-transposition-cipher "hellothere" "xbm" :irregular nil)))
      (ok (string= "eoe" (subseq result 0 3)))
      (ok (string= "ltr" (subseq result 4 7)))
      (ok (string= "hlhe" (subseq result 8)))))
  (testing "disruption works as expected"
    (let ((result (columnar-transposition-cipher "hellothere" "xbm" :disruption-key "yep")))
      (ok (string= "eloter" (subseq result 0 6)))
      (ok (string= "h" (subseq result 12 13)))
      (ok (string= "l" (subseq result 14 15)))
      (ok (string= "h" (subseq result 16 17)))
      (ok (string= "e" (subseq result 18)))))
  (testing "DISRUPTION-KEY is valid"
    (ok (signals (columnar-transposition-cipher "hellothere" "xbm" :disruption-key "a") 'invalid-key))
    (ok (signals (columnar-transposition-cipher "hellothere" "xbm" :disruption-key "123") 'invalid-key)))
  (testing "more examples"
    (ok (string="eeenoolrhvely" (columnar-transposition-cipher "Hello, everyone!" "people")))
    (ok (string= "udfjentipeemte" (columnar-transposition-cipher "I jumped 10 (ten) feet." "leap")))
    (ok (string="ntoatgiietthhg" (columnar-transposition-cipher "Tonight at eight." "secret")))))

(deftest columnar-transposition-cipher_DE
  (testing "returns a string"
    (ok (stringp (columnar-transposition-cipher "a" "abc" :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (columnar-transposition-cipher "" "abc" :encrypt nil))))
  (testing "basics"
    (ok (string= "abc" (columnar-transposition-cipher "acb" "ab" :encrypt nil)))
    (ok (string= "abc" (columnar-transposition-cipher "bac" "ba" :encrypt nil)))
    (ok (string= "hellothere" (columnar-transposition-cipher "eoeltrhlhe" "xbm" :encrypt nil))))
  (testing "KEY is downcased"
    (ok (string= "abc" (columnar-transposition-cipher "acb" "AB" :encrypt nil))))
  (testing "KEY is filtered"
    (ok (string= "abc" (columnar-transposition-cipher "acb" "9ab" :encrypt nil))))
  (testing "KEY must have at least 2 characters after filtering"
    (ok (signals (columnar-transposition-cipher "abc" "a" :encrypt nil) 'invalid-key))
    (ok (signals (columnar-transposition-cipher "abc" "a9" :encrypt nil) 'invalid-key))
    (ok (signals (columnar-transposition-cipher "abc" "!@#$" :encrypt nil) 'invalid-key))
    (ok (signals (columnar-transposition-cipher "abc" "de" :alphabet "abcd" :encrypt nil) 'invalid-key)))
  (testing "works with KEY that has repeating characters"
    (ok (string= "abc" (columnar-transposition-cipher "acb" "aa" :encrypt nil))))
  (testing "works with KEY that is larger than text"
    (ok (string= "fleeatonce" (columnar-transposition-cipher "fclenoteea" "bgwkzqpndsioaxefclumthyvr" :encrypt nil))))
  (testing "can decipher text that was enciphered with IRREGULAR argument"
    (ok (string= "hellotherezz" (columnar-transposition-cipher "eoezltrzhlhe" "xbm" :encrypt nil))))
  (testing "can decipher text that was disrupted during enciphering"
    (ok (string= "hellothere" (columnar-transposition-cipher "eloterzzzzzzhzlzhze" "xbm" :disruption-key "yep" :encrypt nil)))
    (ok (string= "hellothere" (columnar-transposition-cipher "eloterhwpkukhblbhoe" "xbm" :disruption-key "yep" :encrypt nil))))
  (testing "DISRUPTION-KEY is valid"
    (ok (signals (columnar-transposition-cipher "hellothere" "xbm" :disruption-key "a" :encrypt nil) 'invalid-key))
    (ok (signals (columnar-transposition-cipher "hellothere" "xbm" :disruption-key "123" :encrypt nil) 'invalid-key)))
  (testing "more examples"
    (ok (string="helloeveryone" (columnar-transposition-cipher "eeenoolrhvely" "people" :encrypt nil)))
    (ok (string= "ijumpedtenfeet" (columnar-transposition-cipher "udfjentipeemte" "leap" :encrypt nil)))
    (ok (string="tonightateight" (columnar-transposition-cipher "ntoatgiietthhg" "secret" :encrypt nil)))))

(deftest custom-n-by-m-cipher_EN
  (testing "must receive ALPHABET"
    (ok (signals (custom-n-by-m-cipher "hi" "abcde" "vwxyz") 'invalid-argument)))
  (testing "returns a string"
    (ok (stringp (custom-n-by-m-cipher "hi" "abcde" "vwxyz" :alphabet +alphabet-without-j+))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (custom-n-by-m-cipher "" "abcde" "vwxyz" :alphabet +alphabet-without-j+))))
  (testing "basics"
    (ok (string= "av" (custom-n-by-m-cipher "a" "abcde" "vwxyz" :alphabet +alphabet-without-j+)))
    (ok (string= "avawax" (custom-n-by-m-cipher "abc" "abcde" "vwxyz" :alphabet +alphabet-without-j+)))
    (ok (string= "dldmdn" (custom-n-by-m-cipher "abc" "defgh" "lmnop" :alphabet +alphabet-without-j+))))
  (testing "downcases TEXT"
    (ok (string= "avawax" (custom-n-by-m-cipher "aBC" "abcde" "vwxyz" :alphabet +alphabet-without-j+))))
  (testing "filters TEXT"
    (ok (string= "av" (custom-n-by-m-cipher "#a." "abcde" "vwxyz" :alphabet +alphabet-without-j+))))
  (testing "TEXT gets replacement"
    (ok (string= "bv" (custom-n-by-m-cipher "a" "abcde" "vwxyz" :alphabet +alphabet-without-j+ :replacing-chars (cons "a" "f"))))
    (ok (string= "by" (custom-n-by-m-cipher "j" "abcde" "vwxyz" :alphabet +alphabet-without-j+ :replacing-chars (cons "j" "i"))))
    (ok (string= (custom-n-by-m-cipher "j" "abcde" "vwxyz" :alphabet +alphabet-without-j+ :replacing-chars (cons "j" "i")) (custom-n-by-m-cipher "i" "abcde" "vwxyz" :alphabet +alphabet-without-j+))))
  (testing "grid is keyed correctly"
    (ok (string= "avawaxayazbv" (custom-n-by-m-cipher "cxabde" "abcde" "vwxyz" :key "cx" :alphabet +alphabet-without-j+))))
  (testing "KEY is downcased"
    (ok (string= "avaw" (custom-n-by-m-cipher "za" "abcde" "vwxyz" :key "Za" :alphabet +alphabet-without-j+))))
  (testing "KEY is filtered"
    (ok (string= "avawax" (custom-n-by-m-cipher "abc" "abcde" "vwxyz" :key "789" :alphabet +alphabet-without-j+)))
    (ok (string= "awav" (custom-n-by-m-cipher "ab" "abcde" "vwxyz" :key "bj" :alphabet +alphabet-without-j+))))
  (testing "KEY gets replacement"
    (ok (string= "avaw" (custom-n-by-m-cipher "ia" "abcde" "vwxyz" :key "ij" :alphabet +alphabet-without-j+ :replacing-chars (cons "j" "i")))))
  (testing "SHIFT works"
    (ok (string= "awaxay" (custom-n-by-m-cipher "abc" "abcde" "vwxyz" :shift 1 :alphabet +alphabet-without-j+)))
    (ok (string= "ezavaw" (custom-n-by-m-cipher "abc" "abcde" "vwxyz" :shift -1 :alphabet +alphabet-without-j+))))
  (testing "SHIFT works in combination with KEY"
    (ok (string= "avawax" (custom-n-by-m-cipher "bza" "abcde" "vwxyz" :key "b" :shift 1 :alphabet +alphabet-without-j+)))
    (ok (string= "avawax" (custom-n-by-m-cipher "cbd" "abcde" "vwxyz" :key "c" :shift -1 :alphabet +alphabet-without-j+))))
  (testing "VERT-STR and HORI-STR must contain no duplicates"
    (ok (signals (custom-n-by-m-cipher "" "aacde" "vwxyz" :alphabet +alphabet-without-j+) 'duplicate-character-in-string))
    (ok (signals (custom-n-by-m-cipher "" "abcde" "vvxyz" :alphabet +alphabet-without-j+) 'duplicate-character-in-string)))
  (testing "VERT-STR and HORI-STR can have characters not found in ALPHABET"
    (ok (string= "18192829" (custom-n-by-m-cipher "abcd" "12" "89" :alphabet "abcd"))))
  (testing "ALPHABET must be of length equal to VERT-STR length times HORI-STR length"
    (ok (signals (custom-n-by-m-cipher "" "abcd" "vwxyz" :alphabet +alphabet-without-j+) 'invalid-number-of-characters))
    (ok (signals (custom-n-by-m-cipher "" "abcde" "wxyz" :alphabet +alphabet-without-j+) 'invalid-number-of-characters))
    (ok (signals (custom-n-by-m-cipher "" "abcde" "vwxyz" :alphabet +alphabet+) 'invalid-number-of-characters))
    (ok (signals (custom-n-by-m-cipher "" "abc" "xyz" :alphabet "abcxyz") 'invalid-number-of-characters)))
  (testing "works with valid custom arguments"
    (ok (string= "cyayby" (custom-n-by-m-cipher "had" "abc" "xyz" :key "e" :alphabet "abcdefghi")))
    (ok (string= "abab" (custom-n-by-m-cipher "uv" "a" "b" :alphabet "v" :replacing-chars (cons "u" "v"))))
    (ok (string= "28182919" (custom-n-by-m-cipher "abcdwxyz" "12" "89" :shift 1 :key "x" :alphabet "wxyz"))))
  (testing "more realistic examples"
    (ok (string="bxazcvcvcyazevazdweycycxaz" (custom-n-by-m-cipher "Hello, everyone!" "abcde" "vwxyz" :alphabet +alphabet-without-j+ :replacing-chars (cons "j" "i"))))
    (ok (string= "@(@)$(#&#)!_!)%)%($*!_#*!+!_!_$*" (custom-n-by-m-cipher "I jumped 10 (ten) feet." "!@#$%^" "&*()_+" :alphabet +alphabet-and-numbers+)))
    (ok (string= "41433535335132234235" (custom-n-by-m-cipher "Flee at once!" "12345" "12345" :key "bgwkzqpndsioaxefclumthyvr" :alphabet +alphabet-without-j+)))))

(deftest custom-n-by-m-cipher_DE
  (testing "must receive ALPHABET"
    (ok (signals (custom-n-by-m-cipher "bxby" "abcde" "vwxyz" :encrypt nil) 'invalid-argument)))
  (testing "returns a string"
    (ok (stringp (custom-n-by-m-cipher "bxby" "abcde" "vwxyz" :alphabet +alphabet-without-j+ :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (custom-n-by-m-cipher "" "abcde" "vwxyz" :alphabet +alphabet-without-j+ :encrypt nil))))
  (testing "basics"
    (ok (string= "hi" (custom-n-by-m-cipher "bxby" "abcde" "vwxyz" :alphabet +alphabet-without-j+ :encrypt nil)))
    (ok (string= "abc" (custom-n-by-m-cipher "avawax" "abcde" "vwxyz" :alphabet +alphabet-without-j+ :encrypt nil)))
    (ok (string= "abc" (custom-n-by-m-cipher "dldmdn" "defgh" "lmnop" :alphabet +alphabet-without-j+ :encrypt nil))))
  (testing "must receive TEXT of even length when decrypting"
    (ok (signals (custom-n-by-m-cipher "a" "abcde" "abcde" :alphabet +alphabet-without-q+ :encrypt nil) 'odd-number-of-characters)))
  (testing "only works with characters in VERT-STR and HORI-STR"
    (ok (signals (custom-n-by-m-cipher "aA" "abcde" "abcde" :alphabet +alphabet-without-q+ :encrypt nil) 'invalid-subsequence))
    (ok (signals (custom-n-by-m-cipher "ab" "1" "1" :alphabet "a" :encrypt nil) 'invalid-subsequence)))
  (testing "grid is keyed correctly"
    (ok (string= "cxabde" (custom-n-by-m-cipher "avawaxayazbv" "abcde" "vwxyz" :key "cx" :alphabet +alphabet-without-j+ :encrypt nil))))
  (testing "KEY is downcased"
    (ok (string= "za" (custom-n-by-m-cipher "avaw" "abcde" "vwxyz" :key "Za" :alphabet +alphabet-without-j+ :encrypt nil))))
  (testing "KEY is filtered"
    (ok (string= "abc" (custom-n-by-m-cipher "avawax" "abcde" "vwxyz" :key "789" :alphabet +alphabet-without-j+ :encrypt nil)))
    (ok (string= "ab" (custom-n-by-m-cipher "awav" "abcde" "vwxyz" :key "bj" :alphabet +alphabet-without-j+ :encrypt nil))))
  (testing "KEY gets replacement"
    (ok (string= "ia" (custom-n-by-m-cipher "avaw" "abcde" "vwxyz" :key "ij" :alphabet +alphabet-without-j+ :replacing-chars (cons "j" "i") :encrypt nil))))
  (testing "SHIFT works"
    (ok (string= "abc" (custom-n-by-m-cipher "awaxay" "abcde" "vwxyz" :shift 1 :alphabet +alphabet-without-j+ :encrypt nil)))
    (ok (string= "abc" (custom-n-by-m-cipher "ezavaw" "abcde" "vwxyz" :shift -1 :alphabet +alphabet-without-j+ :encrypt nil))))
  (testing "SHIFT works in combination with KEY"
    (ok (string= "bza" (custom-n-by-m-cipher "avawax" "abcde" "vwxyz" :key "b" :shift 1 :alphabet +alphabet-without-j+ :encrypt nil)))
    (ok (string= "cbd" (custom-n-by-m-cipher "avawax" "abcde" "vwxyz" :key "c" :shift -1 :alphabet +alphabet-without-j+ :encrypt nil))))
  (testing "VERT-STR and HORI-STR must contain no duplicates"
    (ok (signals (custom-n-by-m-cipher "" "aacde" "vwxyz" :alphabet +alphabet-without-j+ :encrypt nil) 'duplicate-character-in-string))
    (ok (signals (custom-n-by-m-cipher "" "abcde" "vvxyz" :alphabet +alphabet-without-j+ :encrypt nil) 'duplicate-character-in-string)))
  (testing "ALPHABET must be of length equal to VERT-STR length times HORI-STR length"
    (ok (signals (custom-n-by-m-cipher "" "abcd" "vwxyz" :alphabet +alphabet-without-j+ :encrypt nil) 'invalid-number-of-characters))
    (ok (signals (custom-n-by-m-cipher "" "abcde" "wxyz" :alphabet +alphabet-without-j+ :encrypt nil) 'invalid-number-of-characters))
    (ok (signals (custom-n-by-m-cipher "" "abcde" "vwxyz" :alphabet +alphabet+ :encrypt nil) 'invalid-number-of-characters))
    (ok (signals (custom-n-by-m-cipher "" "abc" "xyz" :alphabet "abcxyz" :encrypt nil) 'invalid-number-of-characters)))
  (testing "works with valid custom arguments"
    (ok (string= "had" (custom-n-by-m-cipher "cyayby" "abc" "xyz" :key "e" :alphabet "abcdefghi" :encrypt nil)))
    (ok (string= "vv" (custom-n-by-m-cipher "abab" "a" "b" :alphabet "v" :replacing-chars (cons "u" "v") :encrypt nil)))
    (ok (string= "wxyz" (custom-n-by-m-cipher "28182919" "12" "89" :shift 1 :key "x" :alphabet "wxyz" :encrypt nil))))
  (testing "more realistic examples"
    (ok (string="helloeveryone" (custom-n-by-m-cipher "bxazcvcvcyazevazdweycycxaz" "abcde" "vwxyz" :alphabet +alphabet-without-j+ :replacing-chars (cons "j" "i") :encrypt nil)))
    (ok (string= "ijumped10tenfeet" (custom-n-by-m-cipher "@(@)$(#&#)!_!)%)%($*!_#*!+!_!_$*" "!@#$%^" "&*()_+" :alphabet +alphabet-and-numbers+ :encrypt nil)))
    (ok (string= "fleeatonce" (custom-n-by-m-cipher "41433535335132234235" "12345" "12345" :key "bgwkzqpndsioaxefclumthyvr" :alphabet +alphabet-without-j+ :encrypt nil)))))

(deftest custom-n-by-m-cipher_derived_functions
  (testing "ADFGVX-CIPHER"
    (ok (string= "aaadafagavaxdadddfdgdvdxfafdfffgfvfxgagdgfgggvgxvavdvfvgvvvxxaxdxfxgxvxx" (adfgvx-cipher +alphabet-and-numbers+))))
  (testing "ADFGX-CIPHER"
    (ok (string= "aaadafagaxdadddfdgdxfafdfffgfxgagdgfgggxxaxdxfxgxx" (adfgx-cipher +alphabet-without-j+))))
  (testing "POLYBIUS-CIPHER"
    (ok (string= "11121314152122232425313233343541424344455152535455" (polybius-cipher +alphabet-without-j+)))))

(deftest four-square-cipher_EN
  (testing "returns a string"
    (ok (stringp (four-square-cipher "hi"))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (four-square-cipher ""))))
  (testing "basics"
    (ok (string= "ih" (four-square-cipher "hi")))
    (ok (string= "badc" (four-square-cipher "abcd")))
    (ok (string= "icat" (four-square-cipher "abcd" :key1 "big" :key2 "cat"))))
  (testing "TEXT must have even number of characters"
    (ok (signals (four-square-cipher "a") 'odd-number-of-characters))
    (ok (signals (four-square-cipher "abc") 'odd-number-of-characters)))
  (testing "downcases TEXT"
    (ok (string= "ih" (four-square-cipher "HI"))))
  (testing "filters TEXT"
    (ok (string= "ba" (four-square-cipher "#ab."))))
  (testing "TEXT gets replacement"
    (ok (string= "ii" (four-square-cipher "ij"))))
  (testing "KEY is downcased"
    (ok (string= "aadc" (four-square-cipher "abcd" :key1 "B")))
    (ok (string= "bcdb" (four-square-cipher "abcd" :key2 "C"))))
  (testing "KEY is filtered"
    (ok (string= "aadc" (four-square-cipher "abcd" :key1 "9b")))
    (ok (string= "bcdb" (four-square-cipher "abcd" :key2 "c!"))))
  (testing "KEY gets replacement"
    (ok (string= "ia" (four-square-cipher "aa" :key1 "j")))
    (ok (string= "ai" (four-square-cipher "aa" :key2 "j"))))
  (testing "KEY1-grid and KEY2-grid are keyed correctly"
    (ok (string= "tahbicsdaemfpglheikkylbm" (four-square-cipher "aabbccddeeffgghhiikkllmm" :key1 "this is a sample key")))
    (ok (string= "ahbecrdiesfagnhoitkklymb" (four-square-cipher "aabbccddeeffgghhiikkllmm" :key2 "here is another key")))
    (ok (string= "thheirsiasmapnloetkkyybb" (four-square-cipher "aabbccddeeffgghhiikkllmm" :key1 "this is a sample key" :key2 "here is another key"))))
  (testing "SHIFT works"
    (ok (string= "zyazba" (four-square-cipher "aabbcc" :shift1 1 :shift2 2)))
    (ok (string= "bccdde" (four-square-cipher "aabbcc" :shift1 -1 :shift2 -2))))
  (testing "SHIFT works in combination with KEY"
    (ok (string= "mnzb" (four-square-cipher "aabb" :key1 "m" :key2 "n" :shift1 1 :shift2 -1))))
  (testing (format nil "~s and ~s are equivalent in cipher with default arguments" #\i #\j)
    (ok (string= (four-square-cipher "ii") (four-square-cipher "jj"))))
  (testing "invalid custom arguments will cause error"
    (ok (signals (four-square-cipher "hi" :grid-width 4) 'invalid-number-of-characters))
    (ok (signals (four-square-cipher "hi" :grid-height 6) 'invalid-number-of-characters))
    (ok (signals (four-square-cipher "hi" :alphabet "") 'invalid-argument))
    (ok (signals (four-square-cipher "hi" :alphabet "" :replacing-chars nil) 'invalid-number-of-characters)))
  (testing "works with valid custom arguments"
    (ok (string= "facf" (four-square-cipher "habi" :key1 "ae" :key2 "gb" :grid-height 2 :grid-width 5 :alphabet "abcdefghij" :replacing-chars nil)))
    (ok (string= "vv" (four-square-cipher "uv" :grid-height 1 :grid-width 1 :alphabet "v" :replacing-chars (cons "u" "v"))))
    (ok (string= "xyzxwzyw" (four-square-cipher "abc123wwxxyyzz" :key1 "x" :key2 "y" :shift1 1 :shift2 -1 :grid-height 2 :grid-width 2 :alphabet "wxyz" :replacing-chars nil))))
  (testing "more realistic examples"
    (ok (string= "vawiwbwi" (four-square-cipher "Hello, all!" :key1 "greetings" :key2 "yo" :shift1 10)))
    (ok (string= "garhpwbmahhbbn" (four-square-cipher "I jumped 10 (ten) feet." :key1 "jim" :key2 "bob" :shift2 6)))
    (ok (string= "qfzekmakza" (four-square-cipher "Flee at once!" :key1 "bgwkzqpndsioaxefclumthyvr" :key2 "whatever")))
    (ok (string= "hobxmfkkkimd" (four-square-cipher "Obiwan Kenobi" :key1 "example" :key2 "keyword" :replacing-chars nil :alphabet +alphabet-without-q+)))))

(deftest four-square-cipher_DE
  (testing "returns a string"
    (ok (stringp (four-square-cipher "ih" :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (four-square-cipher "" :encrypt nil))))
  (testing "basics"
    (ok (string= "hi" (four-square-cipher "ih" :encrypt nil)))
    (ok (string= "abcd" (four-square-cipher "badc" :encrypt nil)))
    (ok (string= "abcd" (four-square-cipher "icat" :key1 "big" :key2 "cat" :encrypt nil))))
  (testing "TEXT must have even number of characters"
    (ok (signals (four-square-cipher "a" :encrypt nil) 'odd-number-of-characters))
    (ok (signals (four-square-cipher "abc" :encrypt nil) 'odd-number-of-characters)))
  (testing "TEXT must consist of characters from ALPHABET only"
    (ok (signals (four-square-cipher "a1" :encrypt nil) 'invalid-character-in-string)))
  (testing "KEY is downcased"
    (ok (string= "abcd" (four-square-cipher "aadc" :key1 "B" :encrypt nil)))
    (ok (string= "abcd" (four-square-cipher "bcdb" :key2 "C" :encrypt nil))))
  (testing "KEY is filtered"
    (ok (string= "abcd" (four-square-cipher "aadc" :key1 "9b" :encrypt nil)))
    (ok (string= "abcd" (four-square-cipher "bcdb" :key2 "c!" :encrypt nil))))
  (testing "KEY gets replacement"
    (ok (string= "aa" (four-square-cipher "ia" :key1 "j" :encrypt nil)))
    (ok (string= "aa" (four-square-cipher "ai" :key2 "j" :encrypt nil))))
  (testing "KEY1-grid and KEY2-grid are keyed correctly"
    (ok (string= "aabbccddeeffgghhiikkllmm" (four-square-cipher "tahbicsdaemfpglheikkylbm" :key1 "this is a sample key" :encrypt nil)))
    (ok (string= "aabbccddeeffgghhiikkllmm" (four-square-cipher "ahbecrdiesfagnhoitkklymb" :key2 "here is another key" :encrypt nil)))
    (ok (string= "aabbccddeeffgghhiikkllmm" (four-square-cipher "thheirsiasmapnloetkkyybb" :key1 "this is a sample key" :key2 "here is another key" :encrypt nil))))
  (testing "SHIFT works"
    (ok (string= "aabbcc" (four-square-cipher "zyazba" :shift1 1 :shift2 2 :encrypt nil)))
    (ok (string= "aabbcc" (four-square-cipher "bccdde" :shift1 -1 :shift2 -2 :encrypt nil))))
  (testing "SHIFT works in combination with KEY"
    (ok (string= "aabb" (four-square-cipher "mnzb" :key1 "m" :key2 "n" :shift1 1 :shift2 -1 :encrypt nil))))
  (testing "invalid custom arguments will cause error"
    (ok (signals (four-square-cipher "ih" :grid-width 4 :encrypt nil) 'invalid-number-of-characters))
    (ok (signals (four-square-cipher "ih" :grid-height 6 :encrypt nil) 'invalid-number-of-characters))
    (ok (signals (four-square-cipher "ih" :alphabet "" :encrypt nil) 'invalid-argument))
    (ok (signals (four-square-cipher "ih" :alphabet "" :replacing-chars nil :encrypt nil) 'invalid-number-of-characters)))
  (testing "works with valid custom arguments"
    (ok (string= "habi" (four-square-cipher "facf" :key1 "ae" :key2 "gb" :grid-height 2 :grid-width 5 :alphabet "abcdefghij" :replacing-chars nil :encrypt nil)))
    (ok (string= "vv" (four-square-cipher "vv" :grid-height 1 :grid-width 1 :alphabet "v" :replacing-chars (cons "u" "v") :encrypt nil)))
    (ok (string= "wwxxyyzz" (four-square-cipher "xyzxwzyw" :key1 "x" :key2 "y" :shift1 1 :shift2 -1 :grid-height 2 :grid-width 2 :alphabet "wxyz" :replacing-chars nil :encrypt nil))))
  (testing "more realistic examples"
    (ok (string= "helloall" (four-square-cipher "vawiwbwi" :key1 "greetings" :key2 "yo" :shift1 10 :encrypt nil)))
    (ok (string= "iiumpedtenfeet" (four-square-cipher "garhpwbmahhbbn" :key1 "jim" :key2 "bob" :shift2 6 :encrypt nil)))
    (ok (string= "fleeatonce" (four-square-cipher "qfzekmakza" :key1 "bgwkzqpndsioaxefclumthyvr" :key2 "whatever" :encrypt nil)))
    (ok (string= "obiwankenobi" (four-square-cipher "hobxmfkkkimd" :key1 "example" :key2 "keyword" :replacing-chars nil :alphabet +alphabet-without-q+ :encrypt nil)))))

(deftest one-time-pad-cipher_EN
  (testing "returns a string"
    (ok (stringp (one-time-pad-cipher "a" (vector 0)))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (one-time-pad-cipher "" nil))))
  (testing "basics"
    (ok (string= "a" (one-time-pad-cipher "a" (vector 0))))
    (ok (string= "b" (one-time-pad-cipher "a" (vector 1))))
    (ok (string= "z" (one-time-pad-cipher "a" (vector -1))))
    (ok (string= "baa" (one-time-pad-cipher "abc" (vector 1 -1 -2)))))
  (testing "downcases TEXT"
    (ok (string= "dbd" (one-time-pad-cipher "ABc" (vector 3 0 1)))))
  (testing "filters TEXT"
    (ok (string= "a" (one-time-pad-cipher "#a." (vector 0))))
    (ok (string= "a" (one-time-pad-cipher "ac" (vector 0) :alphabet "ab"))))
  (testing "when NUMERICAL-SEQ is T, KEY-SEQ must be array or list"
    (ok (string= "dbd" (one-time-pad-cipher "abc" (vector 3 0 1))))
    (ok (string= "dbd" (one-time-pad-cipher "abc" (list 3 0 1))))
    (ok (signals (one-time-pad-cipher "abc" "abc") 'invalid-argument))
    (ok (signals (one-time-pad-cipher "abc" 9) 'invalid-argument)))
  (testing "when NUMERICAL-SEQ is NIL, KEY-SEQ must be string"
    (ok (string= "c" (one-time-pad-cipher "b" "b" :numerical-seq nil)))
    (ok (string= "dbd" (one-time-pad-cipher "abc" "dab" :numerical-seq nil)))
    (ok (signals (one-time-pad-cipher "abc" (vector 0 0 0) :numerical-seq nil) 'invalid-argument))
    (ok (signals (one-time-pad-cipher "abc" (list 0 0 0) :numerical-seq nil) 'invalid-argument))
    (ok (signals (one-time-pad-cipher "abc" 3 :numerical-seq nil) 'invalid-argument)))
  (testing "when KEY-SEQ is a string, it is downcased and filtered"
    (ok (string= "a" (one-time-pad-cipher "a" "A" :numerical-seq nil)))
    (ok (string= "b" (one-time-pad-cipher "a" "9b" :numerical-seq nil))))
  (testing "elements of numerical KEY-SEQ can be very large or small"
    (ok (string= "b" (one-time-pad-cipher "a" (vector (1+ (* 26 80938459087))))))
    (ok (string= "z" (one-time-pad-cipher "a" (list (1- (* 26 -89488349534)))))))
  (testing "KEY-SEQ must be at least as long as text"
    (ok (signals (one-time-pad-cipher "abc" (vector 1)) 'invalid-argument))
    (ok (signals (one-time-pad-cipher "abc" nil) 'invalid-argument))
    (ok (signals (one-time-pad-cipher "abc" "xy" :numerical-seq nil) 'invalid-argument)))
  (testing "KEY-SEQ can be longer than TEXT"
    (ok (string= "jfe" (one-time-pad-cipher "abc" (vector 9 4 2 9 12 4))))
    (ok (string= "ad" (one-time-pad-cipher "bc" "zbwia" :numerical-seq nil))))
  (testing "more realistic examples"
    (ok (string= "erqtnqkueucqz" (one-time-pad-cipher "Hello, everyone!" (vector 179 273 161 762 857 740 743 16 351 282 196 549 99 299 954))))
    (ok (string= "btl8lfdisk" (one-time-pad-cipher "I see 32 cats!" (list 173 361 7 174 378 49 397 476 827 604 601 24 594 170 311) :alphabet +alphabet-and-numbers+)))
    (ok (string= "eqnvz" (one-time-pad-cipher "hello" (vector 23 12 2 10 11))))
    (ok (string= "eqnvz" (one-time-pad-cipher "hello" "xmckl" :numerical-seq nil)))))

(deftest one-time-pad-cipher_DE
  (testing "returns a string"
    (ok (stringp (one-time-pad-cipher "a" (vector 0) :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (one-time-pad-cipher "" nil :encrypt nil))))
  (testing "basics"
    (ok (string= "a" (one-time-pad-cipher "a" (vector 0) :encrypt nil)))
    (ok (string= "a" (one-time-pad-cipher "b" (vector 1) :encrypt nil)))
    (ok (string= "a" (one-time-pad-cipher "z" (vector -1) :encrypt nil)))
    (ok (string= "abc" (one-time-pad-cipher "baa" (vector 1 -1 -2) :encrypt nil))))
  (testing "TEXT must consist exclusively of characters from ALPHABET"
    (ok (signals (one-time-pad-cipher "A" (vector 1) :encrypt nil) 'invalid-character-in-string))
    (ok (signals (one-time-pad-cipher "9" (vector 1) :encrypt nil) 'invalid-character-in-string))
    (ok (signals (one-time-pad-cipher "ab" (vector 1 2) :alphabet "a" :encrypt nil) 'invalid-character-in-string)))
  (testing "when NUMERICAL-SEQ is T, KEY-SEQ must be array or list"
    (ok (string= "abc" (one-time-pad-cipher "dbd" (vector 3 0 1) :encrypt nil)))
    (ok (string= "abc" (one-time-pad-cipher "dbd" (list 3 0 1) :encrypt nil)))
    (ok (signals (one-time-pad-cipher "dbd" "abc" :encrypt nil) 'invalid-argument))
    (ok (signals (one-time-pad-cipher "dbd" 9 :encrypt nil) 'invalid-argument)))
  (testing "when NUMERICAL-SEQ is NIL, KEY-SEQ must be string"
    (ok (string= "b" (one-time-pad-cipher "c" "b" :numerical-seq nil :encrypt nil)))
    (ok (string= "abc" (one-time-pad-cipher "dbd" "dab" :numerical-seq nil :encrypt nil)))
    (ok (signals (one-time-pad-cipher "dbd" (vector 0 0 0) :numerical-seq nil :encrypt nil) 'invalid-argument))
    (ok (signals (one-time-pad-cipher "dbd" (list 0 0 0) :numerical-seq nil :encrypt nil) 'invalid-argument))
    (ok (signals (one-time-pad-cipher "dbd" 3 :numerical-seq nil :encrypt nil) 'invalid-argument)))
  (testing "when KEY-SEQ is a string, it is downcased and filtered"
    (ok (string= "a" (one-time-pad-cipher "a" "A" :numerical-seq nil :encrypt nil)))
    (ok (string= "a" (one-time-pad-cipher "b" "9b" :numerical-seq nil :encrypt nil))))
  (testing "elements of numerical KEY-SEQ can be very large or small"
    (ok (string= "a" (one-time-pad-cipher "b" (vector (1+ (* 26 80938459087))) :encrypt nil)))
    (ok (string= "a" (one-time-pad-cipher "z" (list (1- (* 26 -89488349534))) :encrypt nil))))
  (testing "KEY-SEQ must be at least as long as text"
    (ok (signals (one-time-pad-cipher "abc" (vector 1) :encrypt nil) 'invalid-argument))
    (ok (signals (one-time-pad-cipher "abc" nil :encrypt nil) 'invalid-argument))
    (ok (signals (one-time-pad-cipher "abc" "xy" :numerical-seq nil :encrypt nil) 'invalid-argument)))
  (testing "KEY-SEQ can be longer than TEXT"
    (ok (string= "abc" (one-time-pad-cipher "jfe" (vector 9 4 2 9 12 4) :encrypt nil)))
    (ok (string= "bc" (one-time-pad-cipher "ad" "zbwia" :numerical-seq nil :encrypt nil))))
  (testing "more realistic examples"
    (ok (string= "helloeveryone" (one-time-pad-cipher "erqtnqkueucqz" (vector 179 273 161 762 857 740 743 16 351 282 196 549 99 299 954) :encrypt nil)))
    (ok (string= "isee32cats" (one-time-pad-cipher "btl8lfdisk" (list 173 361 7 174 378 49 397 476 827 604 601 24 594 170 311) :alphabet +alphabet-and-numbers+ :encrypt nil)))
    (ok (string= "hello" (one-time-pad-cipher "eqnvz" (vector 23 12 2 10 11) :encrypt nil)))
    (ok (string= "hello" (one-time-pad-cipher "eqnvz" "xmckl" :numerical-seq nil :encrypt nil)))))

(deftest playfair-cipher_EN
  (testing "returns a string"
    (ok (stringp (playfair-cipher "hi"))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (playfair-cipher ""))))
  (testing "basics"
    (ok (string= "ik" (playfair-cipher "hi")))
    (ok (string= "dt" (playfair-cipher "yo")))
    (ok (string= "im" (playfair-cipher "go")))
    (ok (string= "zb" (playfair-cipher "we"))))
  (testing "TEXT must have even number of characters"
    (ok (signals (playfair-cipher "a" :make-string-even nil) 'odd-number-of-characters))
    (ok (signals (playfair-cipher "abc" :make-string-even nil) 'odd-number-of-characters)))
  (testing "downcases TEXT"
    (ok (string= "im" (playfair-cipher "GO"))))
  (testing "filters TEXT"
    (ok (string= "bc" (playfair-cipher "#ab."))))
  (testing "TEXT gets replacement"
    (ok (string= "koko" (playfair-cipher "ipjp"))))
  (testing "KEY is downcased"
    (ok (string= "cl" (playfair-cipher "go" :key "G"))))
  (testing "KEY is filtered"
    (ok (string= "vcwa" (playfair-cipher "qvrw" :key "8c")))
    (ok (string= "vcwa" (playfair-cipher "qvrw" :key "c!"))))
  (testing "KEY gets replacement"
    (ok (string= "viwa" (playfair-cipher "qvrw" :key "j"))))
  (testing "grid is keyed correctly"
    (ok (string= "hmslnyqckfzavwzu" (playfair-cipher "tpiegbodakrzuvxz" :key "this is a sample key"))))
  (testing "SHIFT works"
    (ok (string= "uzva" (playfair-cipher "puqv" :shift 1)))
    (ok (string= "wbxc" (playfair-cipher "rwsx" :shift -1))))
  (testing "SHIFT works in combination with KEY"
    (ok (string= "txuzvawbyc" (playfair-cipher "otpuqvrwsy" :key "x" :shift 1))))
  (testing (format nil "~s and ~s are equivalent in cipher with default arguments" #\i #\j)
    (ok (string= (playfair-cipher "ip") (playfair-cipher "jp"))))
  (testing "PRIMARY-NULL-CHAR and SECONDARY-NULL-CHAR work as expected, and must be in ALPHABET, and must be distinct"
    (ok (string= "vuvu" (playfair-cipher "zzq")))
    (ok (string= "uvuv" (playfair-cipher "qqz")))
    (ok (signals (playfair-cipher "hi" :primary-null-char #\@) 'invalid-argument))
    (ok (signals (playfair-cipher "hi" :secondary-null-char #\@) 'invalid-argument))
    (ok (signals (playfair-cipher "hi" :primary-null-char #\a :secondary-null-char #\a) 'invalid-argument)))
  (testing "invalid custom arguments will cause error"
    (ok (signals (playfair-cipher "hi" :grid-width 4) 'invalid-number-of-characters))
    (ok (signals (playfair-cipher "hi" :grid-height 6) 'invalid-number-of-characters))
    (ok (signals (playfair-cipher "hi" :alphabet "") 'invalid-argument))
    (ok (signals (playfair-cipher "hi" :alphabet "" :replacing-chars nil) 'invalid-number-of-characters)))
  (testing "works with valid custom arguments"
    (ok (string= "ediajc" (playfair-cipher "hfgbcj" :key "he" :grid-height 2 :grid-width 5 :alphabet "abcdefghij" :replacing-chars nil :primary-null-char #\i :secondary-null-char #\a)))
    (ok (string= "zwxyyxwzwxxwyzzyzxxzywwy" (playfair-cipher "xyzwwzyxxwwxzyyzxzzxwyyw" :key "x" :shift 1 :grid-height 2 :grid-width 2 :alphabet "wxyz" :replacing-chars nil :primary-null-char #\x :secondary-null-char #\z))))
  (testing "more realistic examples"
    (ok (string= "dimsmpxpms" (playfair-cipher "Hello, all!" :key "greetings" :shift 10)))
    (ok (string= "lonfkracuaiuapau" (playfair-cipher "I jumped 10 (ten) feet." :key "feat" :shift 6)))
    (ok (string= "cuisixhiplis" (playfair-cipher "Flee at once!" :key "bgwkzqpndsioaxefclumthyvr")))
    (ok (string= "bmodzbxdnabekudmuixmmouvif" (playfair-cipher "hide the gold in the tree stump" :key "playfair example" :primary-null-char #\x)))))

(deftest playfair-cipher_DE
  (testing "returns a string"
    (ok (stringp (playfair-cipher "ik" :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (playfair-cipher "" :encrypt nil))))
  (testing "basics"
    (ok (string= "hi" (playfair-cipher "ik" :encrypt nil)))
    (ok (string= "yo" (playfair-cipher "dt" :encrypt nil)))
    (ok (string= "go" (playfair-cipher "im" :encrypt nil)))
    (ok (string= "we" (playfair-cipher "zb" :encrypt nil))))
  (testing "TEXT must have even number of characters"
    (ok (signals (playfair-cipher "a" :encrypt nil) 'odd-number-of-characters))
    (ok (signals (playfair-cipher "abc" :encrypt nil) 'odd-number-of-characters)))
  (testing "KEY is downcased"
    (ok (string= "go" (playfair-cipher "cl" :key "G" :encrypt nil))))
  (testing "KEY is filtered"
    (ok (string= "qvrw" (playfair-cipher "vcwa" :key "8c" :encrypt nil)))
    (ok (string= "qvrw" (playfair-cipher "vcwa" :key "c!" :encrypt nil))))
  (testing "KEY gets replacement"
    (ok (string= "qvrw" (playfair-cipher "viwa" :key "j" :encrypt nil))))
  (testing "grid is keyed correctly"
    (ok (string= "tpiegbodakrzuvxz" (playfair-cipher "hmslnyqckfzavwzu" :key "this is a sample key" :encrypt nil))))
  (testing "SHIFT works"
    (ok (string= "puqv" (playfair-cipher "uzva" :shift 1 :encrypt nil)))
    (ok (string= "rwsx" (playfair-cipher "wbxc" :shift -1 :encrypt nil))))
  (testing "SHIFT works in combination with KEY"
    (ok (string= "otpuqvrwsy" (playfair-cipher "txuzvawbyc" :key "x" :shift 1 :encrypt nil))))
  (testing "PRIMARY-NULL-CHAR and SECONDARY-NULL-CHAR are irrelevent when decrypting, and do not throw errors for duplicate characters or characters outside of alphabet"
    (ok (string= "hi" (playfair-cipher "ik" :primary-null-char #\@ :secondary-null-char #\@ :encrypt nil))))
  (testing "invalid custom arguments will cause error"
    (ok (signals (playfair-cipher "hi" :grid-width 4 :encrypt nil) 'invalid-number-of-characters))
    (ok (signals (playfair-cipher "hi" :grid-height 6 :encrypt nil) 'invalid-number-of-characters))
    (ok (signals (playfair-cipher "hi" :alphabet "" :encrypt nil) 'invalid-argument))
    (ok (signals (playfair-cipher "hi" :alphabet "" :replacing-chars nil :encrypt nil) 'invalid-number-of-characters)))
  (testing "works with valid custom arguments"
    (ok (string= "hfgbcj" (playfair-cipher "ediajc" :key "he" :grid-height 2 :grid-width 5 :alphabet "abcdefghij" :replacing-chars nil :primary-null-char #\i :secondary-null-char #\a :encrypt nil)))
    (ok (string= "xyzwwzyxxwwxzyyzxzzxwyyw" (playfair-cipher "zwxyyxwzwxxwyzzyzxxzywwy" :key "x" :shift 1 :grid-height 2 :grid-width 2 :alphabet "wxyz" :replacing-chars nil :primary-null-char #\x :secondary-null-char #\z :encrypt nil))))
  (testing "more realistic examples"
    (ok (string= "helqloallq" (playfair-cipher "dimsmpxpms" :key "greetings" :shift 10 :encrypt nil)))
    (ok (string= "iqiumpedtenfeqet" (playfair-cipher "lonfkracuaiuapau" :key "feat" :shift 6 :encrypt nil)))
    (ok (string= "fleqeatonceq" (playfair-cipher "cuisixhiplis" :key "bgwkzqpndsioaxefclumthyvr" :encrypt nil)))
    (ok (string= "hidethegoldinthetrexestump" (playfair-cipher "bmodzbxdnabekudmuixmmouvif" :key "playfair example" :primary-null-char #\x :encrypt nil)))))

(deftest polybius-cipher-simple_EN
  (testing "returns a string"
    (ok (stringp (polybius-cipher-simple "a"))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (polybius-cipher-simple ""))))
  (testing "basics"
    (ok (string= "11" (polybius-cipher-simple "a")))
    (ok (string= "111213" (polybius-cipher-simple "abc"))))
  (testing "downcases TEXT"
    (ok (string= "111213" (polybius-cipher-simple "ABc"))))
  (testing "filters TEXT"
    (ok (string= "11" (polybius-cipher-simple "#a."))))
  (testing (format nil "~s and ~s are equivalent to this cipher" #\i #\j)
    (ok (string= (polybius-cipher-simple "i") (polybius-cipher-simple "j"))))
  (testing "full-grid"
    (ok (string= "11121314152122232425313233343541424344455152535455" (polybius-cipher-simple +alphabet-without-j+))))
  (testing "more realistic examples"
    (ok (string="23153131341551154254343315" (polybius-cipher-simple "Hello, everyone!")))
    (ok (string= "2424453235151444153321151544" (polybius-cipher-simple "I jumped 10 (ten) feet.")))))

(deftest polybius-cipher-simple_DE
  (testing "returns a string"
    (ok (stringp (polybius-cipher-simple "11" :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (polybius-cipher-simple "" :encrypt nil))))
  (testing "basics"
    (ok (string= "a" (polybius-cipher-simple "11" :encrypt nil)))
    (ok (string= "abc" (polybius-cipher-simple "111213" :encrypt nil))))
  (testing "TEXT must have even length"
    (ok (signals (polybius-cipher-simple "1" :encrypt nil) 'odd-number-of-characters))
    (ok (signals (polybius-cipher-simple "123" :encrypt nil) 'odd-number-of-characters)))
  (testing "TEXT must consist of only the characters 1, 2, 3, 4, and 5"
    (ok (signals (polybius-cipher-simple "1a" :encrypt nil) 'invalid-subsequence))
    (ok (signals (polybius-cipher-simple "1131a5" :encrypt nil) 'invalid-subsequence)))
  (testing "full-grid"
    (ok (string= +alphabet-without-j+ (polybius-cipher-simple "11121314152122232425313233343541424344455152535455" :encrypt nil))))
  (testing "more realistic examples"
    (ok (string="helloeveryone" (polybius-cipher-simple "23153131341551154254343315" :encrypt nil)))
    (ok (string= "iiumpedtenfeet" (polybius-cipher-simple "2424453235151444153321151544" :encrypt nil)))))

(deftest rail-fence-cipher_EN
  (testing "returns a string"
    (ok (stringp (rail-fence-cipher "a"))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (rail-fence-cipher ""))))
  (testing "basics"
    (ok (string= "a" (rail-fence-cipher "a")))
    (ok (string= "hoell" (rail-fence-cipher "hello")))
    (ok (string= "hoelsl" (rail-fence-cipher "hellos")))
    (ok (string= "cast" (rail-fence-cipher "cats"))))
  (testing "downcases TEXT"
    (ok (string= "heaapntnepsc" (rail-fence-cipher "HappEnStance" :downcase-text t))))
  (testing "can retain uppercase"
    (ok (string= "HEaapntnepSc" (rail-fence-cipher "HappEnStance" :downcase-text nil))))
  (testing "removes whitespace"
    (ok (string= "abdc" (rail-fence-cipher (coerce (list #\a #\space #\b #\tab #\c #\return #\d) 'string)))))
  (testing "removes punctuation"
    (ok (string= "horelteelh" (rail-fence-cipher (coerce (list #\h #\! #\e #\- #\l #\' #\l #\" #\o #\? #\t #\. #\h #\, #\e #\; #\r #\: #\e) 'string)))))
  (testing "removes custom list of characters when passed"
    (ok (string= "aebdfc" (rail-fence-cipher "azbpcdenf" :list-of-other-chars-to-remove (list #\z #\p #\n)))))
  (testing "FENCE-LENGTH cannot be less than 2"
    (ok (signals (rail-fence-cipher "" :fence-length 1) 'invalid-number))
    (ok (signals (rail-fence-cipher "" :fence-length -5) 'invalid-number)))
  (testing "FENCE-LENGTH can be huge without efficiency issues"
    (ok (string= "test" (rail-fence-cipher "test" :fence-length (* 9230948 2308420394)))))
  (testing "other FENCE-LENGTHS work as expected"
    (ok (string= "hloel" (rail-fence-cipher "hello" :fence-length 2)))
    (ok (string= "helol" (rail-fence-cipher "hello" :fence-length 4)))
    (ok (string= "ctas" (rail-fence-cipher "cats" :fence-length 2)))
    (ok (string= "hpesacapntne" (rail-fence-cipher "happenstance" :fence-length 2)))
    (ok (string= "hsantepeacpn" (rail-fence-cipher "happenstance" :fence-length 4)))
    (ok (string= "haatnpscpnee" (rail-fence-cipher "happenstance" :fence-length 5)))
    (ok (string= "hcanepaptesn" (rail-fence-cipher "happenstance" :fence-length 6)))
    (ok (string= "haepcpneants" (rail-fence-cipher "happenstance" :fence-length 7)))
    (ok (string= "happeecnnsat" (rail-fence-cipher "happenstance" :fence-length 8))))
  (testing "more realistic examples"
    (ok (string= "horeeleeynlvo" (rail-fence-cipher "Hello, everyone!")))
    (ok (string= "i3tse2asec" (rail-fence-cipher "I see 32 cats!")))
    (ok (string= "wecruoerdsoeerntneaivdac" (rail-fence-cipher "We are discovered. Run at once!")))))

(deftest rail-fence-cipher_DE
  (testing "returns a string"
    (ok (stringp (rail-fence-cipher "a" :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (rail-fence-cipher "" :encrypt nil))))
  (testing "basics"
    (ok (string= "a" (rail-fence-cipher "a" :encrypt nil)))
    (ok (string= "hello" (rail-fence-cipher "hoell" :encrypt nil)))
    (ok (string= "hellos" (rail-fence-cipher "hoelsl" :encrypt nil)))
    (ok (string= "cats" (rail-fence-cipher "cast" :encrypt nil))))
  (testing "FENCE-LENGTH cannot be less than 2"
    (ok (signals (rail-fence-cipher "" :fence-length 1 :encrypt nil) 'invalid-number))
    (ok (signals (rail-fence-cipher "" :fence-length -5 :encrypt nil) 'invalid-number)))
  (testing "FENCE-LENGTH can be huge without efficiency issues"
    (ok (string= "test" (rail-fence-cipher "test" :fence-length (* 9230948 2308420394) :encrypt nil))))
  (testing "other FENCE-LENGTHS work as expected"
    (ok (string= "hello" (rail-fence-cipher "hloel" :fence-length 2 :encrypt nil)))
    (ok (string= "hello" (rail-fence-cipher "helol" :fence-length 4 :encrypt nil)))
    (ok (string= "cats" (rail-fence-cipher "ctas" :fence-length 2 :encrypt nil)))
    (ok (string= "happenstance" (rail-fence-cipher "hpesacapntne" :fence-length 2 :encrypt nil)))
    (ok (string= "happenstance" (rail-fence-cipher "hsantepeacpn" :fence-length 4 :encrypt nil)))
    (ok (string= "happenstance" (rail-fence-cipher "haatnpscpnee" :fence-length 5 :encrypt nil)))
    (ok (string= "happenstance" (rail-fence-cipher "hcanepaptesn" :fence-length 6 :encrypt nil)))
    (ok (string= "happenstance" (rail-fence-cipher "haepcpneants" :fence-length 7 :encrypt nil)))
    (ok (string= "happenstance" (rail-fence-cipher "happeecnnsat" :fence-length 8 :encrypt nil))))
  (testing "decryption does not alter TEXT, such as with downcasing or punctuation removal..."
    (ok (string= "x y !" (rail-fence-cipher "x!  y" :downcase-text t :remove-whitespace t :remove-punctuation t :list-of-other-chars-to-remove (list #\y) :encrypt nil))))
  (testing "more realistic examples"
    (ok (string= "helloeveryone" (rail-fence-cipher "horeeleeynlvo" :encrypt nil)))
    (ok (string= "isee32cats" (rail-fence-cipher "i3tse2asec" :encrypt nil)))
    (ok (string= "wearediscoveredrunatonce" (rail-fence-cipher "wecruoerdsoeerntneaivdac" :encrypt nil)))))

(deftest scytale-cipher_EN
  (testing "returns a string"
    (ok (stringp (scytale-cipher "a"))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (scytale-cipher ""))))
  (testing "basics"
    (ok (string= "a" (scytale-cipher "a")))
    (ok (string= "yaeh" (scytale-cipher "yeah")))
    (ok (string= "acebd" (scytale-cipher "abcde")))
    (ok (string= "acebdf" (scytale-cipher "abcdef")))
    (ok (string= "adgbecf" (scytale-cipher "abcdefg")))
    (ok (string= "adgbehcf" (scytale-cipher "abcdefgh")))
    (ok (string= "adgbehcfi" (scytale-cipher "abcdefghi"))))
  (testing "downcases TEXT"
    (ok (string= "heaannpscpte" (scytale-cipher "HappEnStance" :downcase-text t))))
  (testing "can retain uppercase"
    (ok (string= "HEaannpScpte" (scytale-cipher "HappEnStance" :downcase-text nil))))
  (testing "removes whitespace"
    (ok (string= "acbd" (scytale-cipher (coerce (list #\a #\space #\b #\tab #\c #\return #\d) 'string)))))
  (testing "removes punctuation"
    (ok (string= "horetelhle" (scytale-cipher (coerce (list #\h #\! #\e #\- #\l #\' #\l #\" #\o #\? #\t #\. #\h #\, #\e #\; #\r #\: #\e) 'string)))))
  (testing "removes custom list of characters when passed"
    (ok (string= "acebdf" (scytale-cipher "azbpcdenf" :list-of-other-chars-to-remove (list #\z #\p #\n)))))
  (testing "RADIUS cannot be less than 2"
    (ok (signals (scytale-cipher "" :radius 1) 'invalid-number))
    (ok (signals (scytale-cipher "" :radius -5) 'invalid-number)))
  (testing "RADIUS can be huge without efficiency issues"
    (ok (string= "happenstance" (scytale-cipher "happenstance" :radius (* 9230942 2308420393)))))
  (testing "other RADIUS values work as expected"
    (ok (string= "acbd" (scytale-cipher "abcd" :radius 2)))
    (ok (string= "adbec" (scytale-cipher "abcde" :radius 2)))
    (ok (string= "acebd" (scytale-cipher "abcde" :radius 4)))
    (ok (string= "adbecf" (scytale-cipher "abcdef" :radius 2)))
    (ok (string= "acebdf" (scytale-cipher "abcdef" :radius 4)))
    (ok (string= "acegbdf" (scytale-cipher "abcdefg" :radius 5)))
    (ok (string= "aeimquybfjnrvzcgkoswdhlptx" (scytale-cipher +alphabet+ :radius 8)))
    (ok (string= "adgjmpsvy147behknqtwz258cfilorux0369" (scytale-cipher +alphabet-and-numbers+ :radius 16))))
  (testing "when RADIUS is less than or equal to length of TEXT, TEXT is unchanged"
    (ok (string= "123456789" (scytale-cipher "123456789" :radius 9)))
    (ok (string= "abcd" (scytale-cipher "abcd" :radius 4)))
    (ok (string= "abc" (scytale-cipher "abc"))))
  (testing "more realistic examples"
    (ok (string= "heoevnleelroy" (scytale-cipher "Hello, everyone!")))
    (ok (string= "iecss3ae2t" (scytale-cipher "I see 32 cats!" :radius 4)))
    (ok (string= "Iryyatbhmvaehedlurlp" (scytale-cipher "I am hurt very badly, help!" :radius 4 :downcase-text nil)))))

(deftest scytale-cipher_DE
  (testing "returns a string"
    (ok (stringp (scytale-cipher "a" :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (scytale-cipher "" :encrypt nil))))
  (testing "basics"
    (ok (string= "a" (scytale-cipher "a" :encrypt nil)))
    (ok (string= "yeah" (scytale-cipher "yaeh" :encrypt nil)))
    (ok (string= "abcde" (scytale-cipher "acebd" :encrypt nil)))
    (ok (string= "abcdef" (scytale-cipher "acebdf" :encrypt nil)))
    (ok (string= "abcdefg" (scytale-cipher "adgbecf" :encrypt nil)))
    (ok (string= "abcdefgh" (scytale-cipher "adgbehcf" :encrypt nil)))
    (ok (string= "abcdefghi" (scytale-cipher "adgbehcfi" :encrypt nil))))
  (testing "RADIUS cannot be less than 2"
    (ok (signals (scytale-cipher "" :radius 1 :encrypt nil) 'invalid-number))
    (ok (signals (scytale-cipher "" :radius -5 :encrypt nil) 'invalid-number)))
  (testing "RADIUS can be huge without efficiency issues"
    (ok (string= "happenstance" (scytale-cipher "happenstance" :radius (* 9230942 2308420393) :encrypt nil))))
  (testing "other RADIUS values work as expected"
    (ok (string= "abcd" (scytale-cipher "acbd" :radius 2 :encrypt nil)))
    (ok (string= "abcde" (scytale-cipher "adbec" :radius 2 :encrypt nil)))
    (ok (string= "abcde" (scytale-cipher "acebd" :radius 4 :encrypt nil)))
    (ok (string= "abcdef" (scytale-cipher "adbecf" :radius 2 :encrypt nil)))
    (ok (string= "abcdef" (scytale-cipher "acebdf" :radius 4 :encrypt nil)))
    (ok (string= "abcdefg" (scytale-cipher "acegbdf" :radius 5 :encrypt nil)))
    (ok (string= +alphabet+ (scytale-cipher "aeimquybfjnrvzcgkoswdhlptx" :radius 8 :encrypt nil)))
    (ok (string= +alphabet-and-numbers+ (scytale-cipher "adgjmpsvy147behknqtwz258cfilorux0369" :radius 16 :encrypt nil))))
  (testing "when RADIUS is less than or equal to length of TEXT, TEXT is unchanged"
    (ok (string= "123456789" (scytale-cipher "123456789" :radius 9 :encrypt nil)))
    (ok (string= "abcd" (scytale-cipher "abcd" :radius 4 :encrypt nil)))
    (ok (string= "abc" (scytale-cipher "abc" :encrypt nil))))
  (testing "decryption does not alter TEXT, such as with downcasing or punctuation removal..."
    (ok (string= "x y !" (scytale-cipher "xy!  " :downcase-text t :remove-whitespace t :remove-punctuation t :list-of-other-chars-to-remove (list #\y) :encrypt nil))))
  (testing "more realistic examples"
    (ok (string= "helloeveryone" (scytale-cipher "heoevnleelroy" :encrypt nil)))
    (ok (string= "isee32cats" (scytale-cipher "iecss3ae2t" :radius 4 :encrypt nil)))
    (ok (string= "Iamhurtverybadlyhelp" (scytale-cipher "Iryyatbhmvaehedlurlp" :radius 4 :encrypt nil)))))

(deftest simple-substitution-cipher_EN
  (testing "returns a string"
    (ok (stringp (simple-substitution-cipher "a" "abc" "def"))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (simple-substitution-cipher "" "abc" "def"))))
  (testing "basics"
    (ok (string= "d" (simple-substitution-cipher "a" "abc" "def")))
    (ok (string= "@!#" (simple-substitution-cipher "bac" "abc" "!@#"))))
  (testing "downcases TEXT"
    (ok (string= "def" (simple-substitution-cipher "ABc" "abc" "def"))))
  (testing "filters TEXT"
    (ok (string= "x" (simple-substitution-cipher "#a." "abc" "xyz"))))
  (testing "downcases ALPHA1 and ALPHA2"
    (ok (string= "def" (simple-substitution-cipher "abc" "ABC" "def")))
    (ok (string= "def" (simple-substitution-cipher "abc" "abc" "DEF"))))
  (testing "works as expected when DOWNCASING is NIL..."
    (ok (string= "f" (simple-substitution-cipher "ABc" "abc" "def" :downcasing nil)))
    (ok (string= "e" (simple-substitution-cipher "abc" "AbC" "def" :downcasing nil)))
    (ok (string= "DEf" (simple-substitution-cipher "abc" "abc" "DEf" :downcasing nil))))
  (testing "works as expected when FILTER is NIL..."
    (ok (string= "#$f" (simple-substitution-cipher "#$c" "abc" "def" :filter nil)))
    (ok (string= "def" (simple-substitution-cipher "ABc" "abc" "def" :filter nil :downcasing t)))
    (ok (string= "ABf" (simple-substitution-cipher "ABc" "abc" "def" :filter nil :downcasing nil))))
  (testing "ALPHA1 and ALPHA2 must be exactly same size"
    (ok (signals (simple-substitution-cipher "" "abc" "xy") 'invalid-argument))
    (ok (signals (simple-substitution-cipher "" "" "xy") 'invalid-argument)))
  (testing "more realistic examples"
    (ok (string= "!@##$()@%@^&$*@_" (simple-substitution-cipher "Hello, everyone!" "helovryn, !" "!@#$%^&*()_")))
    (ok (string= "erttqwyuir" (simple-substitution-cipher "I see 32 cats!" "32isecats" "qwertyui")))))

(deftest simple-substitution-cipher_DE
  (testing "returns a string"
    (ok (stringp (simple-substitution-cipher "d" "abc" "def" :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (simple-substitution-cipher "" "abc" "def" :encrypt nil))))
  (testing "basics"
    (ok (string= "a" (simple-substitution-cipher "d" "abc" "def" :encrypt nil)))
    (ok (string= "bac" (simple-substitution-cipher "@!#" "abc" "!@#" :encrypt nil))))
  (testing "downcases ALPHA1 and ALPHA2"
    (ok (string= "abc" (simple-substitution-cipher "def" "ABC" "def" :encrypt nil)))
    (ok (string= "abc" (simple-substitution-cipher "def" "abc" "DEF" :encrypt nil))))
  (testing "works as expected when FILTER is T..."
    (ok (signals (simple-substitution-cipher "#$c" "abc" "def" :filter t :encrypt nil) 'invalid-character-in-string)))
  (testing "works as expected when FILTER is NIL..."
    (ok (string= "#$c" (simple-substitution-cipher "#$f" "abc" "def" :filter nil :encrypt nil))))
  (testing "works as expected when DOWNCASING is NIL..."
    (ok (string= "C" (simple-substitution-cipher "f" "abC" "def" :downcasing nil :encrypt nil)))
    (ok (string= "A" (simple-substitution-cipher "D" "Abc" "Def" :downcasing nil :encrypt nil))))
  (testing "works when both DOWNCASING and FILTER are NIL"
    (ok (string= "ABC" (simple-substitution-cipher "ABf" "abC" "def" :filter nil :downcasing nil :encrypt nil))))
  (testing "ALPHA1 and ALPHA2 must be exactly same size"
    (ok (signals (simple-substitution-cipher "" "abc" "xy" :encrypt nil) 'invalid-argument))
    (ok (signals (simple-substitution-cipher "" "" "xy" :encrypt nil) 'invalid-argument)))
  (testing "more realistic examples"
    (ok (string= "hello, everyone!" (simple-substitution-cipher "!@##$()@%@^&$*@_" "helovryn, !" "!@#$%^&*()_" :encrypt nil)))
    (ok (string= "isee32cats" (simple-substitution-cipher "erttqwyuir" "32isecats" "qwertyui" :encrypt nil)))))

(deftest trifid-cipher_EN
  (testing "returns a string"
    (ok (stringp (trifid-cipher "hi"))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (trifid-cipher ""))))
  (testing "basics"
    (ok (string= "cx" (trifid-cipher "hi")))
    (ok (string= "aaf" (trifid-cipher "abc")))
    (ok (string= "bojn+s" (trifid-cipher "hellos")))
    (ok (string= "blimbreamkaq" (trifid-cipher "happenstance"))))
  (testing "downcases TEXT"
    (ok (string= "cx" (trifid-cipher "HI"))))
  (testing "filters TEXT"
    (ok (string= "cx" (trifid-cipher "#hi."))))
  (testing "TEXT gets replacement"
    (ok (string= "cx" (trifid-cipher "ho" :replacing-chars (cons "o" "i")))))
  (testing "KEY works..."
    (ok (string= "aad" (trifid-cipher "man" :key "m"))))
  (testing "KEY is downcased"
    (ok (string= "aad" (trifid-cipher "man" :key "M"))))
  (testing "KEY is filtered"
    (ok (string= "aad" (trifid-cipher "man" :key "8m")))
    (ok (string= "aad" (trifid-cipher "man" :key "m!"))))
  (testing "KEY gets replacement"
    (ok (string= "aad" (trifid-cipher "man" :key "x" :replacing-chars (cons "x" "m")))))
  (testing "grid is keyed correctly"
    (ok (string= "thisamplekybcdfgjnoqruvwxz+" (trifid-cipher "attlh ibzso jadym cuegz qewwg w+" :key "this is a sample key"))))
  (testing "SHIFT works"
    (ok (string= "+ab" (trifid-cipher "++e" :shift 1)))
    (ok (string= "bcd" (trifid-cipher "bbg" :shift -1))))
  (testing "SHIFT works in combination with KEY"
    (ok (string= "x+a" (trifid-cipher "xxd" :key "x" :shift 1))))
  (testing "GROUPING cannot be less than 2"
    (ok (signals (trifid-cipher "" :grouping 1) 'invalid-number))
    (ok (signals (trifid-cipher "" :grouping -5) 'invalid-number)))
  (testing "GROUPING works as expected"
    (ok (string= "cnmiqg" (trifid-cipher "hellos" :grouping 2)))
    (ok (string= "bvoody" (trifid-cipher "hellos" :grouping 3)))
    (ok (string= "bqbrqg" (trifid-cipher "hellos" :grouping 4)))
    (ok (string= "bovdoy" (trifid-cipher "hellos" :grouping 6)))
    (ok (string= "bujkweyadjkq" (trifid-cipher "happenstance" :grouping 3)))
    (ok (string= "bpzafwbkdbdq" (trifid-cipher "happenstance" :grouping 4)))
    (ok (string= "bkypzjeamkaq" (trifid-cipher "happenstance" :grouping 10)))
    (ok (string= "bkyjuwakjedq" (trifid-cipher "happenstance" :grouping 12))))
  (testing "GROUPING can be a huge number without issue"
    (ok (string= "bkyjuwakjedq" (trifid-cipher "happenstance" :grouping (* 983289742 872398472)))))
  (testing "invalid custom arguments will cause error"
    (ok (signals (trifid-cipher "" :grid-count-height-width 4) 'invalid-number-of-characters))
    (ok (signals (trifid-cipher "" :grid-count-height-width 0) 'invalid-number))
    (ok (signals (trifid-cipher "" :grid-count-height-width 3.5) 'invalid-number))
    (ok (signals (trifid-cipher "" :grid-count-height-width 37) 'invalid-number))
    (ok (signals (trifid-cipher "" :alphabet +alphabet+) 'invalid-number-of-characters)))
  (testing "works with valid values for GRID-COUNT-HEIGHT-WIDTH"
    (ok (string= "a" (trifid-cipher "a" :grid-count-height-width 1 :alphabet "a")))
    (ok (string= "efd" (trifid-cipher "bad" :key "he" :grid-count-height-width 2 :alphabet "abcdefgh")))
    (ok (string= "q}wc<b}cm|bxq6$?t.qlbjrq(" (trifid-cipher "Ugh, that's messy... sheesh!" :grid-count-height-width 4 :alphabet "abcdefghijklmnopqrstuvwxyz0123456789)!@#$%^&*(-_=+[{]}|;:',<.>/?"))))
  (testing "more realistic examples"
    (ok (string= "joimzd+l" (trifid-cipher "Hello, all!" :key "greetings" :shift 10)))
    (ok (string= "hmuymvb+xwfefd" (trifid-cipher "I jumped 10 (ten) feet." :key "feat" :shift 6 :grouping 2)))
    (ok (string= "fxnagmtemc" (trifid-cipher "Flee at once!" :key "bgwjkzqpndsioaxef+clumthyvr")))
    (ok (string= "fmjfvoissuftfpufeqqc" (trifid-cipher "aide-toi, le ciel t'aidera" :key "FELIX MARIE DELASTELLE")))))

(deftest trifid-cipher_DE
  (testing "returns a string"
    (ok (stringp (trifid-cipher "cx" :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (trifid-cipher "" :encrypt nil))))
  (testing "basics"
    (ok (string= "hi" (trifid-cipher "cx" :encrypt nil)))
    (ok (string= "abc" (trifid-cipher "aaf" :encrypt nil)))
    (ok (string= "hellos" (trifid-cipher "bojn+s" :encrypt nil)))
    (ok (string= "happenstance" (trifid-cipher "blimbreamkaq" :encrypt nil))))
  (testing "TEXT must contain only characters from ALPHABET"
    (ok (signals (trifid-cipher "aA" :encrypt nil) 'invalid-character-in-string))
    (ok (signals (trifid-cipher "a9" :encrypt nil) 'invalid-character-in-string)))
  (testing "KEY works..."
    (ok (string= "man" (trifid-cipher "aad" :key "m" :encrypt nil))))
  (testing "KEY is downcased"
    (ok (string= "man" (trifid-cipher "aad" :key "M" :encrypt nil))))
  (testing "KEY is filtered"
    (ok (string= "man" (trifid-cipher "aad" :key "8m" :encrypt nil)))
    (ok (string= "man" (trifid-cipher "aad" :key "m!" :encrypt nil))))
  (testing "KEY gets replacement"
    (ok (string= "man" (trifid-cipher "aad" :key "x" :replacing-chars (cons "x" "m") :encrypt nil))))
  (testing "grid is keyed correctly"
    (ok (string= "attlhibzsojadymcuegzqewwgw+" (trifid-cipher "thisamplekybcdfgjnoqruvwxz+" :key "this is a sample key" :encrypt nil))))
  (testing "SHIFT works"
    (ok (string= "++e" (trifid-cipher "+ab" :shift 1 :encrypt nil)))
    (ok (string= "bbg" (trifid-cipher "bcd" :shift -1 :encrypt nil))))
  (testing "SHIFT works in combination with KEY"
    (ok (string= "xxd" (trifid-cipher "x+a" :key "x" :shift 1 :encrypt nil))))
  (testing "GROUPING cannot be less than 2"
    (ok (signals (trifid-cipher "" :grouping 1 :encrypt nil) 'invalid-number))
    (ok (signals (trifid-cipher "" :grouping -5 :encrypt nil) 'invalid-number)))
  (testing "GROUPING works as expected"
    (ok (string= "hellos" (trifid-cipher "cnmiqg" :grouping 2 :encrypt nil)))
    (ok (string= "hellos" (trifid-cipher "bvoody" :grouping 3 :encrypt nil)))
    (ok (string= "hellos" (trifid-cipher "bqbrqg" :grouping 4 :encrypt nil)))
    (ok (string= "hellos" (trifid-cipher "bovdoy" :grouping 6 :encrypt nil)))
    (ok (string= "happenstance" (trifid-cipher "bujkweyadjkq" :grouping 3 :encrypt nil)))
    (ok (string= "happenstance" (trifid-cipher "bpzafwbkdbdq" :grouping 4 :encrypt nil)))
    (ok (string= "happenstance" (trifid-cipher "bkypzjeamkaq" :grouping 10 :encrypt nil)))
    (ok (string= "happenstance" (trifid-cipher "bkyjuwakjedq" :grouping 12 :encrypt nil))))
  (testing "GROUPING can be a huge number without issue"
    (ok (string= "happenstance" (trifid-cipher "bkyjuwakjedq" :grouping (* 983289742 872398472) :encrypt nil))))
  (testing "invalid custom arguments will cause error"
    (ok (signals (trifid-cipher "" :grid-count-height-width 4 :encrypt nil) 'invalid-number-of-characters))
    (ok (signals (trifid-cipher "" :grid-count-height-width 0 :encrypt nil) 'invalid-number))
    (ok (signals (trifid-cipher "" :grid-count-height-width 3.5 :encrypt nil) 'invalid-number))
    (ok (signals (trifid-cipher "" :grid-count-height-width 37 :encrypt nil) 'invalid-number))
    (ok (signals (trifid-cipher "" :alphabet +alphabet+ :encrypt nil) 'invalid-number-of-characters)))
  (testing "works with valid values for GRID-COUNT-HEIGHT-WIDTH"
    (ok (string= "a" (trifid-cipher "a" :grid-count-height-width 1 :alphabet "a" :encrypt nil)))
    (ok (string= "bad" (trifid-cipher "efd" :key "he" :grid-count-height-width 2 :alphabet "abcdefgh" :encrypt nil)))
    (ok (string= "ugh,that'smessy...sheesh!" (trifid-cipher "q}wc<b}cm|bxq6$?t.qlbjrq(" :grid-count-height-width 4 :alphabet "abcdefghijklmnopqrstuvwxyz0123456789)!@#$%^&*(-_=+[{]}|;:',<.>/?" :encrypt nil))))
  (testing "more realistic examples"
    (ok (string= "helloall" (trifid-cipher "joimzd+l" :key "greetings" :shift 10 :encrypt nil)))
    (ok (string= "ijumpedtenfeet" (trifid-cipher "hmuymvb+xwfefd" :key "feat" :shift 6 :grouping 2 :encrypt nil)))
    (ok (string= "fleeatonce" (trifid-cipher "fxnagmtemc" :key "bgwjkzqpndsioaxef+clumthyvr" :encrypt nil)))
    (ok (string= "aidetoilecieltaidera" (trifid-cipher "fmjfvoissuftfpufeqqc" :key "FELIX MARIE DELASTELLE" :encrypt nil)))))

(deftest trithemius-cipher_EN
  (testing "returns a string"
    (ok (stringp (trithemius-cipher "a"))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (trithemius-cipher ""))))
  (testing "basics"
    (ok (string= "a" (trithemius-cipher "a")))
    (ok (string= "ace" (trithemius-cipher "abc")))
    (ok (string= "bdf" (trithemius-cipher "bcd")))
    (ok (string= "dpgvxmozexbv" (trithemius-cipher "doesthiswork"))))
  (testing "INITIAL-OFFSET works as expected"
    (ok (string= "b" (trithemius-cipher "a" :initial-offset 1)))
    (ok (string= "z" (trithemius-cipher "a" :initial-offset -1)))
    (ok (string= "bdf" (trithemius-cipher "abc" :initial-offset 1))))
  (testing "INITIAL-OFFSET can be very big or very small"
    (ok (string= "b" (trithemius-cipher "a" :initial-offset (1+ (* 26 1289732948573985)))))
    (ok (string= "z" (trithemius-cipher "a" :initial-offset (1- (* 26 1289732948573986))))))
  (testing "INITIAL-OFFSET must be an integer"
    (ok (signals (trithemius-cipher "" :initial-offset "blah") 'invalid-argument))
    (ok (signals (trithemius-cipher "" :initial-offset 2.5) 'invalid-argument)))
  (testing "downcases TEXT"
    (ok (string= "ace" (trithemius-cipher "ABc"))))
  (testing "filters TEXT"
    (ok (string= "a" (trithemius-cipher "#a.")))
    (ok (string= "a" (trithemius-cipher "ac" :alphabet "ab"))))
  (testing "TEXT gets replacement"
    (ok (string= "ace" (trithemius-cipher "abz" :replacing-chars (cons "z" "c")))))
  (testing "works when every character in alphabet is used"
    (ok (string= "acegikmoqsuwyacegikmoqsuwy" (trithemius-cipher +alphabet+)))
    (ok (string= "acb" (trithemius-cipher "abc" :alphabet "abc"))))
  (testing "is equivalent to Vigenere Cipher when Vigenere is keyed by same alphabet that is shifted by negative INITIAL-OFFSET"
    (ok (string= (vigenere-cipher "This should be equivalent!" +alphabet+) (trithemius-cipher "This should be equivalent!")))
    (ok (string= (vigenere-cipher "This should be equivalent!" (shift-string +alphabet+ -5)) (trithemius-cipher "This should be equivalent!" :initial-offset 5))))
  (testing "more realistic examples"
    (ok (string= "hfnosjblzhyyq" (trithemius-cipher "Hello, everyone!")))
    (ok (string= "nylmccnm66" (trithemius-cipher "I see 32 cats!" :initial-offset 5 :alphabet +alphabet-and-numbers+)))))

(deftest trithemius-cipher_DE
  (testing "returns a string"
    (ok (stringp (trithemius-cipher "a" :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (trithemius-cipher "" :encrypt nil))))
  (testing "basics"
    (ok (string= "a" (trithemius-cipher "a" :encrypt nil)))
    (ok (string= "abc" (trithemius-cipher "ace" :encrypt nil)))
    (ok (string= "bcd" (trithemius-cipher "bdf" :encrypt nil)))
    (ok (string= "doesthiswork" (trithemius-cipher "dpgvxmozexbv" :encrypt nil))))
  (testing "INITIAL-OFFSET works as expected"
    (ok (string= "a" (trithemius-cipher "b" :initial-offset 1 :encrypt nil)))
    (ok (string= "a" (trithemius-cipher "z" :initial-offset -1 :encrypt nil)))
    (ok (string= "abc" (trithemius-cipher "bdf" :initial-offset 1 :encrypt nil))))
  (testing "INITIAL-OFFSET can be very big or very small"
    (ok (string= "a" (trithemius-cipher "b" :initial-offset (1+ (* 26 1289732948573985)) :encrypt nil)))
    (ok (string= "a" (trithemius-cipher "z" :initial-offset (1- (* 26 1289732948573986)) :encrypt nil))))
  (testing "INITIAL-OFFSET must be an integer"
    (ok (signals (trithemius-cipher "" :initial-offset "blah" :encrypt nil) 'invalid-argument))
    (ok (signals (trithemius-cipher "" :initial-offset 2.5 :encrypt nil) 'invalid-argument)))
  (testing "TEXT must consist of only characters from ALPHABET"
    (ok (signals (trithemius-cipher "aA" :encrypt nil) 'invalid-character-in-string))
    (ok (signals (trithemius-cipher "a9" :encrypt nil) 'invalid-character-in-string)))
  (testing "works when every character in alphabet is used"
    (ok (string= +alphabet+ (trithemius-cipher "acegikmoqsuwyacegikmoqsuwy" :encrypt nil)))
    (ok (string= "abc" (trithemius-cipher "acb" :alphabet "abc" :encrypt nil))))
  (testing "is equivalent to Vigenere Cipher when Vigenere is keyed by same alphabet that is shifted by negative INITIAL-OFFSET"
    (ok (string= (vigenere-cipher "tikvwmubtmlpqdixlrdxho" +alphabet+ :encrypt nil) (trithemius-cipher "tikvwmubtmlpqdixlrdxho" :encrypt nil)))
    (ok (string= (vigenere-cipher "ynpabrzgyrquvincqwicmt" (shift-string +alphabet+ -5) :encrypt nil) (trithemius-cipher "ynpabrzgyrquvincqwicmt" :initial-offset 5 :encrypt nil))))
  (testing "more realistic examples"
    (ok (string= "helloeveryone" (trithemius-cipher "hfnosjblzhyyq" :encrypt nil)))
    (ok (string= "isee32cats" (trithemius-cipher "nylmccnm66" :initial-offset 5 :alphabet +alphabet-and-numbers+ :encrypt nil)))))

(deftest two-square-cipher_EN
  (testing "returns a string"
    (ok (stringp (two-square-cipher "hi"))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (two-square-cipher ""))))
  (testing "basics"
    (ok (string= "ih" (two-square-cipher "hi")))
    (ok (string= "badc" (two-square-cipher "abcd")))
    (ok (string= "dw" (two-square-cipher "by")))
    (ok (string= "icat" (two-square-cipher "bagb" :key1 "big" :key2 "cat"))))
  (testing "TEXT must have even number of characters"
    (ok (signals (two-square-cipher "a") 'odd-number-of-characters))
    (ok (signals (two-square-cipher "abc") 'odd-number-of-characters)))
  (testing "downcases TEXT"
    (ok (string= "ih" (two-square-cipher "HI"))))
  (testing "filters TEXT"
    (ok (string= "ba" (two-square-cipher "#ab."))))
  (testing "TEXT gets replacement"
    (ok (string= "ii" (two-square-cipher "ij"))))
  (testing "KEY is downcased"
    (ok (string= "abdc" (two-square-cipher "abcd" :key1 "B")))
    (ok (string= "ccdb" (two-square-cipher "abcd" :key2 "C"))))
  (testing "KEY is filtered"
    (ok (string= "abdc" (two-square-cipher "abcd" :key1 "9b")))
    (ok (string= "ccdb" (two-square-cipher "abcd" :key2 "c!"))))
  (testing "KEY gets replacement"
    (ok (string= "ib" (two-square-cipher "aa" :key1 "j")))
    (ok (string= "bi" (two-square-cipher "aa" :key2 "j"))))
  (testing "KEY1-grid and KEY2-grid are keyed correctly"
    (ok (string= "hascaepfehkkbldnfpnqqsruvvxxzz" (two-square-cipher "tbidaemglikkymcofpgrotruuwwyzz" :key1 "this is a sample key")))
    (ok (string= "bhdresgaiokkmyocpfrgtmuqwuywzz" (two-square-cipher "aeciesfnhtkklbndpfqlspuqvvxxzz" :key2 "here is another key")))
    (ok (string= "thheirsiasmapnloetkkyybbccddffggnlomqprquuvvwwxxzz" (two-square-cipher "thheirsiasmapnloetkkyybbccddffggnlomqprquuvvwwxxzz" :key1 "this is a sample key" :key2 "here is another key"))))
  (testing "SHIFT works"
    (ok (string= "zzbg" (two-square-cipher "aycf" :shift1 1 :shift2 2)))
    (ok (string= "ccaa" (two-square-cipher "bdzb" :shift1 -1 :shift2 -2))))
  (testing "SHIFT works in combination with KEY"
    (ok (string= "znbcyz" (two-square-cipher "mbadxa" :key1 "m" :key2 "n" :shift1 1 :shift2 -1))))
  (testing (format nil "~s and ~s are equivalent in cipher with default arguments" #\i #\j)
    (ok (string= (two-square-cipher "ii") (two-square-cipher "jj"))))
  (testing "READ-VERTICALLY can be NIL with expected results"
    (ok (string= "fbcikelm" (two-square-cipher "aghdeklm" :read-vertically nil))))
  (testing "OBSCURE-WITH-REVERSE-READ works as expected"
    (ok (string= "af" (two-square-cipher "af" :read-vertically t)))
    (ok (string= "fa" (two-square-cipher "af" :read-vertically t :obscure-with-reverse-read t)))
    (ok (string= "ab" (two-square-cipher "ab" :read-vertically nil)))
    (ok (string= "ba" (two-square-cipher "ab" :read-vertically nil :obscure-with-reverse-read t))))
  (testing "invalid custom arguments will cause error"
    (ok (signals (two-square-cipher "hi" :grid-width 4) 'invalid-number-of-characters))
    (ok (signals (two-square-cipher "hi" :grid-height 6) 'invalid-number-of-characters))
    (ok (signals (two-square-cipher "hi" :alphabet "") 'invalid-argument))
    (ok (signals (two-square-cipher "hi" :alphabet "" :replacing-chars nil) 'invalid-number-of-characters)))
  (testing "works with valid custom arguments"
    (ok (string= "egbcdjgehi" (two-square-cipher "abcadjffih" :key1 "ae" :key2 "gb" :grid-height 2 :grid-width 5 :alphabet "abcdefghij" :replacing-chars nil)))
    (ok (string= "fggbhaicjd" (two-square-cipher "aeefbhcidj" :key1 "ae" :key2 "gb" :grid-height 2 :grid-width 5 :alphabet "abcdefghij" :replacing-chars nil :read-vertically nil)))
    (ok (string= "vv" (two-square-cipher "uv" :grid-height 1 :grid-width 1 :alphabet "v" :replacing-chars (cons "u" "v"))))
    (ok (string= "zyyz" (two-square-cipher "abc123xxww" :key1 "x" :key2 "y" :shift1 1 :shift2 -1 :grid-height 2 :grid-width 2 :alphabet "wxyz" :replacing-chars nil))))
  (testing "more realistic examples"
    (ok (string= "chmkmbmk" (two-square-cipher "Hello, all!" :key1 "greetings" :key2 "yo" :shift1 10)))
    (ok (string= "vqxwtwxw" (two-square-cipher "Hello, all!" :key1 "greetings" :key2 "yo" :shift1 10 :read-vertically nil)))
    (ok (string= "iitnlhhphkefhq" (two-square-cipher "I jumped 10 (ten) feet." :key1 "jim" :key2 "bob" :shift2 6)))
    (ok (string= "mfeexaonmh" (two-square-cipher "Flee at once!" :key1 "bgwkzqpndsioaxefclumthyvr" :key2 "whatever")))
    (ok (string= "sdjyanhotkdg" (two-square-cipher "Obiwan Kenobi" :key1 "example" :key2 "keyword" :replacing-chars nil :alphabet +alphabet-without-q+)))))

(deftest two-square-cipher_DE
  (testing "returns a string"
    (ok (stringp (two-square-cipher "ih" :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (two-square-cipher "" :encrypt nil))))
  (testing "basics"
    (ok (string= "hi" (two-square-cipher "ih" :encrypt nil)))
    (ok (string= "abcd" (two-square-cipher "badc" :encrypt nil)))
    (ok (string= "by" (two-square-cipher "dw" :encrypt nil)))
    (ok (string= "bagb" (two-square-cipher "icat" :key1 "big" :key2 "cat" :encrypt nil))))
  (testing "TEXT must have even number of characters"
    (ok (signals (two-square-cipher "a" :encrypt nil) 'odd-number-of-characters))
    (ok (signals (two-square-cipher "abc" :encrypt nil) 'odd-number-of-characters)))
  (testing "TEXT must consist only of characters from ALPHABET"
    (ok (signals (two-square-cipher "aA" :encrypt nil) 'invalid-character-in-string))
    (ok (signals (two-square-cipher "a9" :encrypt nil) 'invalid-character-in-string)))
  (testing "KEY is downcased"
    (ok (string= "abcd" (two-square-cipher "abdc" :key1 "B" :encrypt nil)))
    (ok (string= "abcd" (two-square-cipher "ccdb" :key2 "C" :encrypt nil))))
  (testing "KEY is filtered"
    (ok (string= "abcd" (two-square-cipher "abdc" :key1 "9b" :encrypt nil)))
    (ok (string= "abcd" (two-square-cipher "ccdb" :key2 "c!" :encrypt nil))))
  (testing "KEY gets replacement"
    (ok (string= "aa" (two-square-cipher "ib" :key1 "j" :encrypt nil)))
    (ok (string= "aa" (two-square-cipher "bi" :key2 "j" :encrypt nil))))
  (testing "KEY1-grid and KEY2-grid are keyed correctly"
    (ok (string= "tbidaemglikkymcofpgrotruuwwyzz" (two-square-cipher "hascaepfehkkbldnfpnqqsruvvxxzz" :key1 "this is a sample key" :encrypt nil)))
    (ok (string= "aeciesfnhtkklbndpfqlspuqvvxxzz" (two-square-cipher "bhdresgaiokkmyocpfrgtmuqwuywzz" :key2 "here is another key" :encrypt nil)))
    (ok (string= "thheirsiasmapnloetkkyybbccddffggnlomqprquuvvwwxxzz" (two-square-cipher "thheirsiasmapnloetkkyybbccddffggnlomqprquuvvwwxxzz" :key1 "this is a sample key" :key2 "here is another key" :encrypt nil))))
  (testing "SHIFT works"
    (ok (string= "aycf" (two-square-cipher "zzbg" :shift1 1 :shift2 2 :encrypt nil)))
    (ok (string= "bdzb" (two-square-cipher "ccaa" :shift1 -1 :shift2 -2 :encrypt nil))))
  (testing "SHIFT works in combination with KEY"
    (ok (string= "mbadxa" (two-square-cipher "znbcyz" :key1 "m" :key2 "n" :shift1 1 :shift2 -1 :encrypt nil))))
  (testing "READ-VERTICALLY can be NIL with expected results"
    (ok (string= "aghdeklm" (two-square-cipher "fbcikelm" :read-vertically nil :encrypt nil))))
  (testing "OBSCURE-WITH-REVERSE-READ works as expected"
    (ok (string= "af" (two-square-cipher "af" :read-vertically t :encrypt nil)))
    (ok (string= "af" (two-square-cipher "fa" :read-vertically t :obscure-with-reverse-read t :encrypt nil)))
    (ok (string= "ab" (two-square-cipher "ab" :read-vertically nil :encrypt nil)))
    (ok (string= "ab" (two-square-cipher "ba" :read-vertically nil :obscure-with-reverse-read t :encrypt nil))))
  (testing "invalid custom arguments will cause error"
    (ok (signals (two-square-cipher "hi" :grid-width 4 :encrypt nil) 'invalid-number-of-characters))
    (ok (signals (two-square-cipher "hi" :grid-height 6 :encrypt nil) 'invalid-number-of-characters))
    (ok (signals (two-square-cipher "hi" :alphabet "" :encrypt nil) 'invalid-argument))
    (ok (signals (two-square-cipher "hi" :alphabet "" :replacing-chars nil :encrypt nil) 'invalid-number-of-characters)))
  (testing "works with valid custom arguments"
    (ok (string= "abcadjffih" (two-square-cipher "egbcdjgehi" :key1 "ae" :key2 "gb" :grid-height 2 :grid-width 5 :alphabet "abcdefghij" :replacing-chars nil :encrypt nil)))
    (ok (string= "aeefbhcidj" (two-square-cipher "fggbhaicjd" :key1 "ae" :key2 "gb" :grid-height 2 :grid-width 5 :alphabet "abcdefghij" :replacing-chars nil :read-vertically nil :encrypt nil)))
    (ok (string= "vv" (two-square-cipher "vv" :grid-height 1 :grid-width 1 :alphabet "v" :replacing-chars (cons "u" "v") :encrypt nil)))
    (ok (string= "xxww" (two-square-cipher "zyyz" :key1 "x" :key2 "y" :shift1 1 :shift2 -1 :grid-height 2 :grid-width 2 :alphabet "wxyz" :replacing-chars nil :encrypt nil))))
  (testing "more realistic examples"
    (ok (string= "helloall" (two-square-cipher "chmkmbmk" :key1 "greetings" :key2 "yo" :shift1 10 :encrypt nil)))
    (ok (string= "helloall" (two-square-cipher "vqxwtwxw" :key1 "greetings" :key2 "yo" :shift1 10 :read-vertically nil :encrypt nil)))
    (ok (string= "iiumpedtenfeet" (two-square-cipher "iitnlhhphkefhq" :key1 "jim" :key2 "bob" :shift2 6 :encrypt nil)))
    (ok (string= "fleeatonce" (two-square-cipher "mfeexaonmh" :key1 "bgwkzqpndsioaxefclumthyvr" :key2 "whatever" :encrypt nil)))
    (ok (string= "obiwankenobi" (two-square-cipher "sdjyanhotkdg" :key1 "example" :key2 "keyword" :replacing-chars nil :alphabet +alphabet-without-q+ :encrypt nil)))))

(deftest vigenere-cipher_EN
  (testing "returns a string"
    (ok (stringp (vigenere-cipher "a" "b"))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (vigenere-cipher "" "b"))))
  (testing "basics"
    (ok (string= "a" (vigenere-cipher "a" "a")))
    (ok (string= "z" (vigenere-cipher "a" "z")))
    (ok (string= "baa" (vigenere-cipher "abc" "bzy"))))
  (testing "KEY can be shorter than TEXT and KEY repeats up to length of TEXT"
    (ok (string= "bcdefg" (vigenere-cipher "abcdef" "b")))
    (ok (string= "cceegg" (vigenere-cipher "abcdef" "cb"))))
  (testing "downcases TEXT"
    (ok (string= "dbd" (vigenere-cipher "ABc" "dab"))))
  (testing "filters TEXT"
    (ok (string= "a" (vigenere-cipher "#a." "a")))
    (ok (string= "a" (vigenere-cipher "ac" "a" :alphabet "ab"))))
  (testing "KEY is downcased"
    (ok (string= "a" (vigenere-cipher "a" "A"))))
  (testing "KEY is filtered"
    (ok (string= "b" (vigenere-cipher "a" "9b"))))
  (testing "KEY must be of at least length 1 (after filtering)"
    (ok (signals (vigenere-cipher "" "") 'invalid-key))
    (ok (signals (vigenere-cipher "" "123!@#") 'invalid-key)))
  (testing "KEY can be longer than TEXT"
    (ok (string= "ad" (vigenere-cipher "bc" "zbwia"))))
  (testing "more realistic examples"
    (ok (string= "psqqsvhcxpsrx" (vigenere-cipher "Hello, everyone!" "I offer my greetings")))
    (ok (string= "00klmapgb0" (vigenere-cipher "I see 32 cats!" "sighting" :alphabet +alphabet-and-numbers+)))
    (ok (string= "eqnvz" (vigenere-cipher "hello" "xmckl")))))

(deftest vigenere-cipher_DE
  (testing "returns a string"
    (ok (stringp (vigenere-cipher "a" "b" :encrypt nil))))
  (testing "TEXT can be an empty string"
    (ok (string= "" (vigenere-cipher "" "b" :encrypt nil))))
  (testing "basics"
    (ok (string= "a" (vigenere-cipher "a" "a" :encrypt nil)))
    (ok (string= "a" (vigenere-cipher "z" "z" :encrypt nil)))
    (ok (string= "abc" (vigenere-cipher "baa" "bzy" :encrypt nil))))
  (testing "TEXT must consist only of characters from ALPHABET"
    (ok (signals (vigenere-cipher "A" "b" :encrypt nil) 'invalid-character-in-string))
    (ok (signals (vigenere-cipher "9" "b" :encrypt nil) 'invalid-character-in-string)))
  (testing "KEY can be shorter than TEXT and KEY repeats up to length of TEXT"
    (ok (string= "abcdef" (vigenere-cipher "bcdefg" "b" :encrypt nil)))
    (ok (string= "abcdef" (vigenere-cipher "cceegg" "cb" :encrypt nil))))
  (testing "KEY is downcased"
    (ok (string= "a" (vigenere-cipher "a" "A" :encrypt nil))))
  (testing "KEY is filtered"
    (ok (string= "a" (vigenere-cipher "b" "9b" :encrypt nil))))
  (testing "KEY must be of at least length 1 (after filtering)"
    (ok (signals (vigenere-cipher "" "" :encrypt nil) 'invalid-key))
    (ok (signals (vigenere-cipher "" "123!@#" :encrypt nil) 'invalid-key)))
  (testing "KEY can be longer than TEXT"
    (ok (string= "bc" (vigenere-cipher "ad" "zbwia" :encrypt nil))))
  (testing "more realistic examples"
    (ok (string= "helloeveryone" (vigenere-cipher "psqqsvhcxpsrx" "I offer my greetings" :encrypt nil)))
    (ok (string= "isee32cats" (vigenere-cipher "00klmapgb0" "sighting" :alphabet +alphabet-and-numbers+ :encrypt nil)))
    (ok (string= "hello" (vigenere-cipher "eqnvz" "xmckl" :encrypt nil)))))

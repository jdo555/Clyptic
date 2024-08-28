# Clyptic

A library for classical encryption in Common Lisp

## What is this?

This is a collection of tools, entirely written in the programming language of Common Lisp, that enables enciphering and deciphering of text, and also various string manipulation techniques. Primarily it is a collection of cipher implementations, such as ADFGX cipher, trifid cipher, four-square cipher, etc., prepared with a mind towards consistency and systematic overlap.

## Purpose

This is a personal project that began many years ago as simple coding practice, before branching out into an exploration of classical cryptography. After a certain amount of time pursuing that curiosity, I decided I might as well turn the resulting code into a more formal library, in the hopes that someone might find it useful.

To be clear, this library deals with *classical* encryption or cryptography, which might otherwise be referred to as enciphering, and which very much predates modern strong cryptography. As such, the primary purpose of this software is amusement.

## Included ciphers

1. ADFGVX cipher
2. ADFGX cipher
3. Affine cipher
4. Atbash cipher
5. Autokey cipher
6. Bifid cipher
7. Caesar cipher
8. Columnar Transposition cipher
9. Custom-n-by-m cipher
10. Four-square cipher
11. One-time-pad cipher
12. Playfair cipher
13. Polybius cipher
14. Rail-fence cipher
15. Scytale cipher
16. Simple substitution cipher
17. Trifid cipher
18. Trithemius cipher
19. Two-square cipher
20. Vigenere cipher

## Important general information about ciphers

This software will, with most ciphers, downcase all text. I opted for the software to consistently use lowercase where case is irrelevant (such as with ciphers like the ADFGX cipher), although it is popular online to have all enciphered text in uppercase.

Many ciphers can receive an alphabet as an argument, which will cause filtering of the original text such that all characters in text that are not in the alphabet are lost. Some may consider this effect undesirable, but, because one of my goals for this software was to simulate serious application of the various ciphers, I opted for consistent filtering where appropriate so as to prevent all transparencies (seeing that most historical applications of ciphers proceeded without space, punctuation, etc.). This can cause other potential issues when it comes to deciphering enciphered text; but as long as you are deciphering text that was enciphered within this library, there will be no issues; however, if you are trying to decipher something found elsewhere (such as online), you may need to use one of the libraries general functions, such as REMOVE-CHARACTERS-FROM-STRING, to clear away textual noise beforehand, because alphabet-using ciphers will trigger an error if the text has a character not in the alphabet.

Some ciphers that use alphabets, but without a grid (such as the Columnar Transposition cipher) will also filter, though such filtering can be avoided by providing a custom alphabet string. On the other hand, some other ciphers that do not use grids (such as Scytale and Rail-fence ciphers) do not have an alphabet parameter, and allow for more fine-tuned filtering of text based on type (such as spacing, punctuation, etc.).

More could be said (and may later be said), but for now, please consult the code and its documentation strings for any other concerns.

## Running the software

Run the following code in a common-lisp REPL, making sure to provide the appropriate path for the asd file:
```common-lisp
(asdf:load-asd #p"provide/path/to/clyptic.asd")
(asdf:load-system "clyptic")
(in-package :clyptic)
```

### Sample executions

```common-lisp
(trifid-cipher "Here is an example run." :shift 5 :key "unicorn" :grouping 5 :encrypt t)
```
```text
"badyvmanlghmbfhuzk"
```
```common-lisp
(trifid-cipher "badyvmanlghmbfhuzk" :shift 5 :key "unicorn" :grouping 5 :encrypt nil)
```
```text
"hereisanexamplerun"
```
Notice from this example that all space, upper-casing, and punctuation are lost; many ciphers in this library will do something similar in order to prevent transparancies in the enciphered text.

## Testing of software

I have prepared extensive test suites for this library that make use of the **[Rove](https://github.com/fukamachi/rove)** library. For more details about the tests, please see the README in the **[TESTS/](./tests/)** directory.

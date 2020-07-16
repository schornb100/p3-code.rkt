(check-expect (elim-contains-char #\b (list "ben" "hello" "abc" "world"))
              (list "hello" "world"))
(check-expect (elim-contains-char #\w (list "who" "what" "where" ))
              (list))
(check-expect (elim-contains-char #\A (list "apple" "AP" "Alphabet"))
              (list "apple"))
              
(check-expect (valid-words
              (list "them" "thee" "theme" "hem") (list #\t #\h #\e #\m))
              (list "them" "thee" "theme" "hem"))
(check-expect (valid-words
              (list "olga" "germ" "hello" "ogre") (list #\e #\g #\o #\m #\r))
              (list "germ" "ogre"))
(check-expect (valid-words
              (list "CHI" "rice" "RICH" "CHIN") (list #\R #\I #\C #\H #\E))
              (list "CHI" "RICH"))
              
(check-expect (unique (list 10 11 12 13 10))
              (list 10 11 12 13))
(check-expect (unique (list "hello" "world" "hi"))
              (list "hello" "world" "hi"))
(check-expect (unique (list 11.5 11.5 12 11.5 55 11.5 "done"))
              (list 11.5 12 55 "done"))
              
(check-expect (l33t (list "ApplE" "Hello" "irIs"))
              (list "4ppl3" "H3ll0" "1r1s"))
(check-expect (l33t (list "What" "is" "Going" "On"))
              (list "Wh4t" "1s" "G01ng" "0n"))
(check-expect (l33t (list "aE2iO" "BcDf"))
              (list "43210" "BcDf"))

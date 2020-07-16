; Char List-of-strings -> List-of-strings
; consumes a list of strings and a character
; returns the same list of words that exclude the words that contain Char
(check-expect (elim-contains-char #\b (list "ben" "hello" "abc" "world"))
              (list "hello" "world"))
(check-expect (elim-contains-char #\w (list "who" "what" "where" ))
              (list))
(check-expect (elim-contains-char #\A (list "apple" "AP" "Alphabet"))
              (list "apple"))
(define (elim-contains-char Char List-of-strings)
  (cond
    [(empty? List-of-strings) '()]
    [(not (member Char (string->list (first List-of-strings))))
      (cons (first List-of-strings) (elim-contains-char Char (rest List-of-strings)))]
    [else (elim-contains-char Char (rest List-of-strings))]))



; List-of-chars List-of-chars -> Boolean
; consumes two lists of chars -- List-of-chars and List-of-chars2
; returns #t if all elements in List-of-chars are in List-of-chars2
; returns #f otherwise
(check-expect (same-list?
              (list #\t #\h #\e #\e) (list #\t #\h #\e #\m))
              #t)
(check-expect (same-list?
              (list #\h #\e #\l #\l #\o) (list #\h #\a  #\l #\o))
              #f)
(check-expect (same-list?
              (list #\H #\I) (list #\H #\i))
              #f)
(define (same-list? List-of-chars List-of-chars2)
  (cond
   [(empty? List-of-chars) #t]
   [(not (member (first List-of-chars) List-of-chars2)) #f]
   [else (same-list? (rest List-of-chars) List-of-chars2)]))

; List-of-strings List-of-chars -> List-of-strings
; consumes a list of strings and a list of chars
; returns words from List-of-string that only contains chars from List-of-chars
(check-expect (valid-words
              (list "them" "thee" "theme" "hem") (list #\t #\h #\e #\m))
              (list "them" "thee" "theme" "hem"))
(check-expect (valid-words
              (list "olga" "germ" "hello" "ogre" "shrek") (list #\e #\g #\o #\m #\r))
              (list "germ" "ogre"))
(check-expect (valid-words
              (list "CHI" "rice" "RICH" "CHIN") (list #\R #\I #\C #\H #\E))
              (list "CHI" "RICH"))
(define (valid-words List-of-strings List-of-chars)
  (cond
    [(empty? List-of-strings) '()]
    [(same-list? (string->list (first List-of-strings)) List-of-chars)
      (cons (first List-of-strings)
        (valid-words (rest List-of-strings) List-of-chars))]
    [else (valid-words (rest List-of-strings) List-of-chars)]))
    
    
    
; Any List-of-any -> List-of-any
; consumes a given value Any and a list of values
; deletes all instances of Any in the list
(check-expect (delete-all 10 (list 10 11 12 13 10))
              (list 11 12 13))
(check-expect (delete-all "hi" (list "hello" "hola" "ih"))
              (list "hello" "hola" "ih"))
(define (delete-all Any List-of-any)
  (cond
    [(empty? List-of-any) '()]
    [(equal? (first List-of-any) Any) (delete-all Any (rest List-of-any))]
    [else (cons (first List-of-any) (delete-all Any (rest List-of-any)))]))

; List-of-any -> List-of-any
; consumes a list of values
; returns a list of the same values without duplicates
(check-expect (unique (list 10 11 12 13 10))
              (list 10 11 12 13))
(check-expect (unique (list "hello" "world" "hi"))
              (list "hello" "world" "hi"))
(check-expect (unique (list 11.5 11.5 12 11.5 55 11.5 "done"))
              (list 11.5 12 55 "done"))
(define (unique List-of-any)
  (cond
    [(empty? List-of-any) '()]
    [(member (first List-of-any) (rest List-of-any))
      (cons (first List-of-any)
        (unique (delete-all (first List-of-any) (rest List-of-any))))]
    [else (cons (first List-of-any) (unique (rest List-of-any)))]))
    


; List-of-chars -> List-of-chars
; consumes a list of chars
; modifies and returns the list of chars with the following changes:
; it turns #\A and #\a into #\4
; it turns #\E and #\e into #\3
; it turns #\I and #\i into #\1
; it turns #\O and #\o into #\0
(check-expect (leet (list #\h #\e #\l #\l #\o))
              (list #\h #\3 #\l #\l #\0))
(check-expect (leet (list #\b #\C #\d))
              (list #\b #\C #\d))
(check-expect (leet (list #\A #\e #\i #\O #\u))
              (list #\4 #\3 #\1 #\0 #\u))
(define (leet List-of-chars)
  (cond
    [(empty? List-of-chars) '()]
    [(or (equal? #\A (first List-of-chars))
         (equal? #\a (first List-of-chars)))
           (cons #\4 (leet (rest List-of-chars)))]
    [(or (equal? #\E (first List-of-chars))
         (equal? #\e (first List-of-chars)))
           (cons #\3 (leet (rest List-of-chars)))]
    [(or (equal? #\I (first List-of-chars))
         (equal? #\i (first List-of-chars)))
           (cons #\1 (leet (rest List-of-chars)))]
    [(or (equal? #\O (first List-of-chars))
         (equal? #\o (first List-of-chars)))
           (cons #\0 (leet (rest List-of-chars)))]
    [else (cons (first List-of-chars) (leet (rest List-of-chars)))]))

; List-of-strings -> List-of-strings
; consumes a list of strings
; performs the helper function 'leet' on each string
; returns the motified list
(check-expect (l33t (list "ApplE" "Hello" "irIs"))
              (list "4ppl3" "H3ll0" "1r1s"))
(check-expect (l33t (list "What" "is" "Going" "On"))
              (list "Wh4t" "1s" "G01ng" "0n"))
(check-expect (l33t (list "aE2iO" "BcDf"))
              (list "43210" "BcDf"))
(define (l33t List-of-strings)
  (cond
    [(empty? List-of-strings) '()]
    [else
      (cons (list->string (leet (string->list (first List-of-strings))))
      (l33t (rest List-of-strings)))]))




; List-of-chars -> List-of-chars
; consumes a list of chars
; returns modified list of chars where all the vowels are deleted
(check-expect
 (no-vowel (cons #\A (cons #\i (cons #\U '()))))
 (list))
(check-expect
 (no-vowel (cons #\B (cons #\Z (cons #\R '()))))
 (cons #\B (cons #\Z (cons #\R '()))))
(check-expect
 (no-vowel (cons #\A (cons #\B (cons #\O (cons #\e (cons #\C '()))))))
 (cons #\B (cons #\C '())))
(define (no-vowel List-of-chars)
  (cond
    [(empty? List-of-chars) '()]
    [(member (first List-of-chars) (cons #\a (cons #\A (cons #\e (cons #\E (cons #\i (cons #\I (cons #\o (cons #\O (cons #\u (cons #\U '()))))))))))) (no-vowel (rest List-of-chars))]
    [else (cons (first List-of-chars) (no-vowel (rest List-of-chars)))]))

; List-of-strings -> List-of-strings
; consumes a list of strings
; returns the last list of strings but all the vowel in each string are deleted
(check-expect
 (strip-vowels (cons "apple" (cons "hEllo" (cons "irIs" '()))))
 (cons "ppl" (cons "hll" (cons "rs" '()))))
(check-expect
 (strip-vowels (cons "aAa" (cons "bBb" (cons "vOwEls" '()))))
 (cons "" (cons "bBb" (cons "vwls" '()))))
(check-expect
 (strip-vowels (cons "HolU" (cons "WouldA" '())))
 (cons "Hl" (cons "Wld" '())))

; Char List-of-strings -> List-of-strings
; consumes a list of strings and a character
; returns the same list of words that exclude the words that contain Char
(check-expect
 (elim-contains-char #\b (cons "ben" (cons "hello" (cons "abc" (cons "world" '())))))
 (cons "hello" (cons "world" '())))
(check-expect
 (elim-contains-char #\w (cons "who" (cons "what" (cons "where" '()))))
 (list))
(check-expect
 (elim-contains-char #\A (cons "apple" (cons "AP" (cons "Alphabet" '()))))
 (cons "apple" '()))
(define (elim-contains-char Char List-of-strings)
  (cond
    [(empty? List-of-strings) '()]
    [(not (member Char (string->list (first List-of-strings)))) (cons (first List-of-strings) (elim-contains-char Char (rest List-of-strings)))]
    [else (elim-contains-char Char (rest List-of-strings))]))




; Any List-of-any -> List-of-any
; consumes a given value Any and a list of values
; deletes all instances of Any in the list
(check-expect
 (delete-all 10 (cons 10 (cons 11 (cons 12 (cons 13 (cons 10 '()))))))
 (cons 11 (cons 12 (cons 13 '()))))
(check-expect
 (delete-all "hi" (cons "hello" (cons "hola" (cons "ih" '()))))
 (cons "hello" (cons "hola" (cons "ih" '()))))
(define (delete-all Any List-of-any)
  (cond
    [(empty? List-of-any) '()]
    [(equal? (first List-of-any) Any) (delete-all Any (rest List-of-any))]
    [else (cons (first List-of-any) (delete-all Any (rest List-of-any)))]))

; List-of-any -> List-of-any
; consumes a list of values
; returns a list of the same values without duplicates
(check-expect
 (unique (cons 10 (cons 11 (cons 12 (cons 13 (cons 10 '()))))))
 (cons 10 (cons 11 (cons 12 (cons 13 '())))))
(check-expect
 (unique (cons "hello" (cons "world" (cons "hi" '()))))
 (cons "hello" (cons "world" (cons "hi" '()))))
(check-expect
 (unique (cons 11.5 (cons 11.5 (cons 12 (cons 11.5 (cons 55 (cons 11.5 (cons "done" '()))))))))
 (cons 11.5 (cons 12 (cons 55 (cons "done" '())))))
(define (unique List-of-any)
  (cond
    [(empty? List-of-any) '()]
    [(member (first List-of-any) (rest List-of-any)) (cons (first List-of-any) (unique (delete-all (first List-of-any) (rest List-of-any))))]
    [else (cons (first List-of-any) (unique (rest List-of-any)))]))
    


; List-of-chars -> List-of-chars
; consumes a list of chars
; modifies and returns the list of chars with the following changes:
; it turns #\A and #\a into #\4
; it turns #\E and #\e into #\3
; it turns #\I and #\i into #\1
; it turns #\O and #\o into #\0
(define (leet List-of-chars)
  (cond
    [(empty? List-of-chars) '()]
    [(or (equal? #\A (first List-of-chars)) (equal? #\a (first List-of-chars))) (cons #\4 (leet (rest List-of-chars)))]
    [(or (equal? #\E (first List-of-chars)) (equal? #\e (first List-of-chars))) (cons #\3 (leet (rest List-of-chars)))]
    [(or (equal? #\I (first List-of-chars)) (equal? #\i (first List-of-chars))) (cons #\1 (leet (rest List-of-chars)))]
    [(or (equal? #\O (first List-of-chars)) (equal? #\o (first List-of-chars))) (cons #\0 (leet (rest List-of-chars)))]
    [else (cons (first List-of-chars) (leet (rest List-of-chars)))]))

; consumes a list of strings
; performs the helper function 'leet' on each string and returns the motified list
(check-expect
 (l33t (cons "ApplE" (cons "Hello" (cons "irIs" '()))))
 (cons "4ppl3" (cons "H3ll0" (cons "1r1s" '()))))
(define (l33t List-of-strings)
  (cond
    [(empty? List-of-strings) '()]
    [else (cons (list->string (leet (string->list (first List-of-strings)))) (l33t (rest List-of-strings)))]))




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

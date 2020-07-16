; Char List-of-strings -> List-of-strings
; consumes a list of strings and a character
; returns the same list of words that exclude the words that contain Char
(check-expect (elim-contains-char #\b (list "ben" "hello" "abc" "world"))
              (list "hello" "world"))
(check-expect (elim-contains-char #\w (list "who" "what" "where" ))
              (list))
(check-expect (elim-contains-char #\A (list "apple" "AP" "Alphabet"))
              (list "apple"))
(check-expect (elim-contains-char #\r (list "no" "yes" "maybe"))
              (list "no" "yes" "maybe"))
(check-expect (elim-contains-char #\A (list))
              (list))
(define (elim-contains-char Char alos)
  (cond
    [(empty? alos) '()]
    [(not (member Char (string->list (first alos))))
      (cons (first alos) (elim-contains-char Char (rest alos)))]
    [else (elim-contains-char Char (rest alos))]))



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
(define (same-list? aloc aloc2)
  (cond
   [(empty? aloc) #t]
   [(not (member (first aloc) aloc2)) #f]
   [else (same-list? (rest aloc) aloc2)]))

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
(check-expect (valid-words
              (list "hello" "world") (list #\a))
              (list))
(check-expect (valid-words
              (list) (list))
              (list))
(define (valid-words alos aloc)
  (cond
    [(empty? alos) '()]
    [(same-list? (string->list (first alos)) aloc)
      (cons (first alos)
        (valid-words (rest alos) aloc))]
    [else (valid-words (rest alos) aloc)]))
    
    
    
; Any List-of-any -> List-of-any
; consumes a given value Any and a list of values
; deletes all instances of Any in the list
(check-expect (delete-all 10 (list 10 11 12 13 10))
              (list 11 12 13))
(check-expect (delete-all "hi" (list "hello" "hola" "ih"))
              (list "hello" "hola" "ih"))
(check-expect (delete-all 11.5 (list 11.5 11.5 11.5 11.5))
              (list))
(define (delete-all Any aloa)
  (cond
    [(empty? aloa) '()]
    [(equal? (first aloa) Any) (delete-all Any (rest aloa))]
    [else (cons (first aloa) (delete-all Any (rest aloa)))]))

; List-of-any -> List-of-any
; consumes a list of values
; returns a list of the same values without duplicates
(check-expect (unique (list 10 11 12 13 10))
              (list 10 11 12 13))
(check-expect (unique (list "hello" "world" "hi"))
              (list "hello" "world" "hi"))
(check-expect (unique (list 11.5 11.5 11.5 11.5))
              (list 11.5))
(check-expect (unique (list #\A #\a #\A))
              (list #\A #\a))
(check-expect (unique (list))
              (list))
(define (unique aloa)
  (cond
    [(empty? aloa) '()]
    [(member (first aloa) (rest aloa))
      (cons (first aloa)
        (unique (delete-all (first aloa) (rest aloa))))]
    [else (cons (first aloa) (unique (rest aloa)))]))
    


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
(define (leet aloc)
  (cond
    [(empty? aloc) '()]
    [(or (equal? #\A (first aloc))
         (equal? #\a (first aloc)))
           (cons #\4 (leet (rest aloc)))]
    [(or (equal? #\E (first aloc))
         (equal? #\e (first aloc)))
           (cons #\3 (leet (rest aloc)))]
    [(or (equal? #\I (first aloc))
         (equal? #\i (first aloc)))
           (cons #\1 (leet (rest aloc)))]
    [(or (equal? #\O (first aloc))
         (equal? #\o (first aloc)))
           (cons #\0 (leet (rest aloc)))]
    [else (cons (first aloc) (leet (rest aloc)))]))

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
(check-expect (l33t (list "Aeiou"))
              (list "4310u"))
(check-expect (l33t (list))
              (list))
(define (l33t alos)
  (cond
    [(empty? alos) '()]
    [else
      (cons (list->string (leet (string->list (first alos))))
        (l33t (rest alos)))]))



; List-of-chars -> List-of-chars
; consumes a list of chars
; returns modified list of chars where all the vowels are deleted
(check-expect (no-vowel (list #\A #\i #\U))
              (list))
(check-expect (no-vowel (list #\B #\Z #\R))
              (list #\B #\Z #\R))
(check-expect (no-vowel (list #\A #\B #\O #\e #\C))
              (list #\B #\C))
(define (no-vowel aloc)
  (cond
    [(empty? aloc) '()]
    [(member (first aloc) (list #\a #\A #\e #\E #\i #\I #\o #\O #\u #\U))
      (no-vowel (rest aloc))]
    [else (cons (first aloc) (no-vowel (rest aloc)))]))

; List-of-strings -> List-of-strings
; consumes a list of strings
; returns modified alos where the vowels in each string are deleted
(check-expect (strip-vowels (list "apple" "hEllo" "irIs"))
              (list "ppl" "hll" "rs"))
(check-expect (strip-vowels (list "aAa" "bBb" "vOwEls"))
              (list "" "bBb" "vwls"))
(check-expect (strip-vowels (list "HolU" "WouldA"))
              (list "Hl" "Wld"))
(check-expect (strip-vowels (list "AEIOU"))
              (list ""))
(check-expect (strip-vowels (list))
              (list))     
(define (strip-vowels alos)
  (cond
    [(empty? alos) '()]
    [else
      (cons (list->string (no-vowel (string->list (first alos))))
        (strip-vowels (rest alos)))]))

(check-expect
 (elim-contains-char #\b (list "ben" "hello" "abc" "world"))
 (list "hello" "world"))
(check-expect
 (elim-contains-char #\w (list "who" "what" "where" ))
 (list))
(check-expect
 (elim-contains-char #\A (list "apple" "AP" "Alphabet"))
 (list "apple"))

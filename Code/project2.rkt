#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301
;; Fall 2013
;;
;; Project #2
;;
;; Jacob Chapman
;; W01012392
;;
;; The purpose of this lab is to create a 
;; program that aids in the deciphering of 
;; Caesar and Vigenere ciphers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require plot)

(define (get_char_shift l n)
  ; l is a character
  ; n is the offset
  (define z_shift '("Z" "Y" "X" "W" "V" "U" "T" "S" "R" "Q" "P" "O" "N" 
                    "M" "L" "K" "J" "I" "H" "G" "F" "E" "D" "C" "B" "A"))
  (let loop ((i 0))
    (cond
      ((equal? l (list-ref z_shift i))
       (string-ref (list-ref z_shift (modulo (+ n i) 26)) 0))
      (else (loop (+ i 1))))))

(define (get_offset l)
  ; Returns a number based on the offset character
  (define c_shift '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" 
                    "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
  (let loop ((i 0))
    (cond 
      ((equal? l (list-ref c_shift i))
       i)
      (else (loop (+ i 1))))))
  
(define (get_v_shift l v)
  ; Gives the offset of character l by character v
  (get_char_shift l (get_offset v))
  )

(define (build_ht_C file)
  ; Sum the number of times the alphabetical characters were found
  ; Stores them in the c_ht hash table
  ; If the number is already there, it increments 
  ; Prints a histogram of the hash table
  (define c_ht (make-hash))
  
  (let ((infile (open-input-file file)))
    (let loop ((next-char (read-char infile)))
      (when (not (eof-object? next-char))        
        (cond 
          ((not (hash-has-key? c_ht next-char)) 
           (hash-set! c_ht next-char 1))
          (else 
           (hash-set! c_ht next-char (+ 1 (hash-ref c_ht next-char)))))
        (loop (read-char infile))))    
    (close-input-port infile))
  
  (print_histo c_ht))

(define (v_histogram file n y)
  ; Sums every n characters after a given offset and prints to 
  ; a histogram
  (define c_ht (make-hash))
  
  (let ((infile (open-input-file file)))
    (let loop ((i 0) (g y) (next-char (read-char infile)))
      ; i stores the n characters to go to before taking another value
      ; g tells us how many characters to ignore at the begining
      (when (not (eof-object? next-char))
        (cond
          ((equal? #\  next-char) (loop i g (read-char infile)))
          ((equal? #\return  next-char) (loop i g (read-char infile)))
          ((equal? #\newline  next-char) (loop i g (read-char infile)))
          ; Ignre the first g characters
          ((= g 0) ; Offset over, begin getting frequency of every n characters
           (cond
             ((= i 0)              
              (cond
                ((not (hash-has-key? c_ht next-char))
                      (hash-set! c_ht next-char 1))
                (else (hash-set! c_ht next-char (+ 1 (hash-ref c_ht next-char)))))
                (loop (- n 1) 0 (read-char infile)))
             (else (loop (- i 1) 0 (read-char infile)))))
          (else (loop 0 (- g 1) (read-char infile))))))
    (close-input-port infile))
  (print_histo c_ht))

(define (build_v_ht file n)
  (define v_ht (make-hash))
  ; n is the length of the substring
  (let ((infile (open-input-file file)))
    (let loop ((i 0) (l '()) (next-char (read-char infile)))
      (when (not (eof-object? next-char))
        (cond            
          ; Gets the first substring of n characters
          ; And skips the empty character
          ((equal? #\  next-char) (loop i l (read-char infile)))
          ((equal? #\return  next-char) (loop i l (read-char infile)))
          ((equal? #\newline  next-char) (loop i l (read-char infile)))
          ((< i n) (loop (+ i 1) (append l (cons next-char null)) 
                         (read-char infile)))
          (else (cond
                  ; Prints the distances between 2 iterations of a substring
                  ((not (hash-has-key? v_ht l)) (hash-set! v_ht l i))
                  (else (display (- (- (- i 1) n) (hash-ref v_ht l)))
                        (display l)
                        (newline)
                        (hash-set! v_ht l i)))))
        (loop (+ i 1) (cdr (append l (cons next-char null)))(read-char infile))))
    (close-input-port infile)))  

(define (Caesar_Decipher file n ofile) 
  ; This file operates nearly identically to charcopy.rkt, but performs
  ; a character shift before outputing to the file.
  (let ((infile (open-input-file file))
      (outfile (open-output-file ofile
                                 #:mode 'text 
                                 #:exists 'replace)))
  (let loop ((next-char (read-char infile)))
    (when (not (eof-object? next-char))
      (cond 
        ((char-alphabetic? next-char) 
         ; Shifts the characters here
         (write-char 
          (get_char_shift (make-string 1 next-char) n)
          outfile))
        
         ; Print out the following to retain formatting
        ((equal? #\newline next-char)
         (write-char next-char outfile))
        ((equal? #\  next-char)
         (write-char next-char outfile)))      
      (loop (read-char infile))))
  (close-input-port infile)
  (close-output-port outfile))
  )

(define (Vigenere_Decipher file key ofile)
  ; Uses the string key to decipher a file
  (let ((infile (open-input-file file))
      (outfile (open-output-file ofile
                                 #:mode 'text 
                                 #:exists 'replace)))
  (let loop ((i 0) (next-char (read-char infile)))
    (when (not (eof-object? next-char))
      (cond ; If the input character is alphabetical, space,
            ; or new line, it outputs to the file
        ((char-alphabetic? next-char) 
        
         ; Shift the characters here
         (write-char 
          (get_v_shift (make-string 1 next-char) 
                       (make-string 1 (string-ref key 
                                          (modulo i (string-length key)))))
          outfile)
         (loop (+ i 1) (read-char infile)))
        
        ; Print out the following to retain formatting
        ((equal? #\newline next-char)
         (write-char next-char outfile))
        ((equal? #\  next-char)
         (write-char next-char outfile)))
        
      (loop i (read-char infile))))
  (close-input-port infile)
  (close-output-port outfile))
  )

(define (print_histo ht)
  ; Prints a histogram of a hash table
  (plot (discrete-histogram (list 
                             (vector "A" (hash-ref ht #\A))
                             (vector "B" (hash-ref ht #\B))
                             (vector "C" (hash-ref ht #\C))
                             (vector "D" (hash-ref ht #\D))
                             (vector "E" (hash-ref ht #\E))
                             (vector "F" (hash-ref ht #\F))
                             (vector "G" (hash-ref ht #\G))
                             (vector "H" (hash-ref ht #\H))
                             (vector "I" (hash-ref ht #\I))
                             (vector "J" (hash-ref ht #\J))
                             (vector "K" (hash-ref ht #\K))
                             (vector "L" (hash-ref ht #\L))
                             (vector "M" (hash-ref ht #\M))
                             (vector "N" (hash-ref ht #\N))
                             (vector "O" (hash-ref ht #\O))
                             (vector "P" (hash-ref ht #\P))
                             (vector "Q" (hash-ref ht #\Q))
                             (vector "R" (hash-ref ht #\R))
                             (vector "S" (hash-ref ht #\S))
                             (vector "T" (hash-ref ht #\T))
                             (vector "U" (hash-ref ht #\U))
                             (vector "V" (hash-ref ht #\V))
                             (vector "W" (hash-ref ht #\W))
                             (vector "X" (hash-ref ht #\X))
                             (vector "Y" (hash-ref ht #\Y))
                             (vector "Z" (hash-ref ht #\Z))))))

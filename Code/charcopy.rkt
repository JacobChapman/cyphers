#lang racket

(let ((infile (open-input-file "caesar.txt"))
      (outfile (open-output-file "test.txt"
                                 #:mode 'text 
                                 #:exists 'replace)))
  (let loop ((next-char (read-char infile)))
    (when (not (eof-object? next-char))
      (cond ; If the input character is alphabetical, space,
            ; or new line, it outputs to the file
        ((char-alphabetic? next-char)
         (write-char next-char outfile))
        ((equal? #\newline next-char)
         (write-char next-char outfile))
        ((equal? #\  next-char)
         (write-char next-char outfile)))
      (loop (read-char infile))))
  (close-input-port infile)
  (close-output-port outfile))
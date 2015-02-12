#lang racket

(let ((infile (open-input-file "filecopy.rkt"))
      (outfile (open-output-file "myoutfile.txt"
                                 #:mode 'text 
                                 #:exists 'replace)))
  (let loop ((next-char (read-char infile)))
    (when (not (eof-object? next-char))
      (write-char next-char outfile)
      (loop (read-char infile))))
  (close-input-port infile)
  (close-output-port outfile))
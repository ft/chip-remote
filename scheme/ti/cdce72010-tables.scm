;;;;
;; Copyright 2011 Frank Terbeck <ft@bewatermyfriend.org>, All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
;; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; AUTHOR OR CONTRIBUTORS OF THE PROJECT BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define-module (ti cdce72010-tables)
  :export (divider-table
           get-bits-for-divider
           get-bits-for-output-mode
           output-modes))

(define divider-table
  ;; We need these to configure each output divider and the feedback divider.
  ;;
  ;; d   : divider setting
  ;; bits: register bits
  ;; cyc : phase-lag (cycles)
  ;; deg : phase-lag (degrees) [cyc * 360]
  ;;
  ;; d     bits       cyc    deg
  '(( 1  #b0100000    0.0      0)
    ( 2  #b1000000    0.5    180)
    ( 3  #b1000001    0.0      0)
    ( 4  #b1000010    0.5    180)
    ( 4  #b0000000   14.5   5220)
    ( 5  #b1000011    0.0      0)
    ( 6  #b0000001   21.0   7560)
    ( 8  #b0000010   28.5  10260)
    ( 8  #b0000100   16.5   5940)
    (10  #b0000011   35.0  12600)
    (12  #b0000101   24.0   8640)
    (12  #b0001000   18.5   6660)
    (16  #b0000110   32.5  11700)
    (16  #b0001100   20.5   7380)
    (18  #b0001001   27.0   9720)
    (20  #b0000111   40.0  14400)
    (20  #b0010000   22.5   8100)
    (24  #b0001010   36.5  13140)
    (24  #b0001101   30.0  10800)
    (24  #b0010100   24.5   8820)
    (28  #b0011000   26.5   9540)
    (30  #b0001011   45.0  16200)
    (30  #b0010001   33.0  11880)
    (32  #b0001110   40.5  14580)
    (32  #b0011100   28.5  10260)
    (36  #b0010101   36.0  12960)
    (40  #b0001111   50.0  18000)
    (40  #b0010010   40.0  16020)
    (42  #b0011001   39.0  14040)
    (48  #b0010110   48.5  17460)
    (48  #b0011101   42.0  15120)
    (50  #b0010011   50.0  19800)
    (56  #b0011010   52.5  18900)
    (60  #b0010111   60.0  21600)
    (64  #b0011110   56.5  20340)
    (70  #b0011011   65.0  23400)
    (80  #b0011111   70.0  25200)))

(define (get-bits-for-divider value)
  (let next((v divider-table))
    (cond
     ((null? v)
      (display (format #f "Invalid divider `~d'. Falling back to 80.\n" value))
      #b0011111)
     (else
      (if (= value (caar v))
          (cadar v)
          (next (cdr v)))))))

(define output-modes
  '((off           #b0110100)
    ;; for completeness, both pins can be low or 3-stated
    (both-3-state  #b0010100)
    (both-low      #b0001010)

    ;; the -high versions are the same mode, just with increased output swing
    (lvpecl        #b1000000)
    (lvpecl-high   #b1000001)

    (lvds          #b1110100)
    (lvds-high     #b1110101)

    ;; with -p/-n the opposing pin is 3-stated
    (lvcmos-p      #b0010010)
    (lvcmos-n      #b0001100)
    ;; p active (n driven low)
    (lvcmos-p-n0   #b0000010)
    ;; n active (p driven low)
    (lvcmos-n-p0   #b0001000)
    ;; both pins active, (synchronously)
    (lvcmos-p+n    #b0001010)
    ;; both pins active (180 degrees phase difference)
    (lvcmos-diff   #b0011010)))

(define (get-bits-for-output-mode mode)
  (let next ((m output-modes))
    (cond
     ((null? m)
      (display (format #f "Invalid mode specifier ~s.\n"
                       (symbol->string mode)))
      (display "Falling back to 'off.\n")
      #b0110100)
     (else
      (if (equal? mode (caar m))
          (cadar m)
          (next (cdr m)))))))

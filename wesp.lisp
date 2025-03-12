; -*- mode:lisp; package:user; base:10 -*-

(defconstant *wesp-mts-addr* #o772520)
(defconstant *wesp-mtc-addr* #o772522)
(defconstant *wesp-mtbrc-addr* #o772524)
(defconstant *wesp-mtcma-addr* #o772526)

(defconstant *wesp-command-offline* 0)
(defconstant *wesp-command-read* 1)
(defconstant *wesp-command-write* 2)
(defconstant *wesp-command-write-eof* 3)
(defconstant *wesp-command-space-for* 4)
(defconstant *wesp-command-space-rev* 5)
(defconstant *wesp-command-write-erg* 6)
(defconstant *wesp-command-rewind* 7)

(defparameter *wesp-debug-unibus-mapping* nil)

(defun wesp-print-array (arr)
  (fresh-line)
  (dotimes (i (array-length arr)) (format t "~2,'0x " (aref arr i)))
  nil)

(defun wesp-fill-array-orderly (arr &key (from 0) (to 256))
  (dotimes (i (array-length arr)) (aset (+ (mod i (- to from)) from) arr i))
  arr)

(defun wesp-make-array (len)
  (make-array len :type art-8b))

(defconstant *wesp-test-array*
             (wesp-fill-array-orderly (wesp-make-array 256)))

(defun %unibus-map-array (arr)
  "maps arr to unibus mapping, returns the offset to be used from #o140000"
  (let* ((vadr-real (%pointer arr)) ; actual address
         (vadr (- vadr-real (ldb (byte 8 0) vadr-real))) ; page aligned address
         (n-words (+ 1 ; 1 word header in the array
                     (%p-ldb sys:%%array-long-length-flag arr) ; 1 word length if long length array
                     (ceiling (array-length arr) 4))) ; ceil(num 1 byte elements / 4)
         (vadr-real-last (+ vadr-real n-words)) ; actual last word address
         (vadr-last (- vadr-real-last (ldb (byte 8 0) vadr-real-last))) ; page aligned last word address
         (vadr-last+1 (+ vadr-last si:page-size)) ; next page aligned last word address (covering the last word)
         ;; n-pages is minimum 1, for example it is 2 if array spans two pages even though it is smaller than a page
         (n-pages (lsh (- vadr-last+1 vadr) -8)))
    (when *wesp-debug-unibus-mapping*
          (format t
              "~%vadr-real: #o~O vadr-aligned: #o~O vadr-last+1: #o~O n-pages: ~D~%"
            vadr-real
            vadr
            vadr-last+1
            n-pages))
    (do ((i 0 (1+ i)) (adr vadr (+ adr si:page-size))) ((= i n-pages))
      (si:wire-page adr)
      (%unibus-map-vadr i adr))
    (lsh (+ 1 (%p-ldb sys:%%array-long-length-flag arr) (- vadr-real (lsh (lsh vadr-real -8) 8))) 2)))

(defun %unibus-unmap-array (arr)
  "unmaps arr from unibus mapping"
  (let* ((vadr-real (%pointer arr))
         (vadr (- vadr-real (ldb (byte 8 0) vadr-real)))
         (n-pages (ceiling
                    (+
                     1
                     (%p-ldb sys:%%array-long-length-flag arr)
                     (ceiling (array-length arr) 4))
                    si:page-size)))
    (do ((i 0 (1+ i)) (adr vadr (+ adr si:page-size))) ((= i n-pages))
      (%unibus-write (+ #o766140 (* i 2)) 0)
      (si:unwire-page adr)
      (when *wesp-debug-unibus-mapping*
            (format t "unibus-unmap-page: ~D~%" i))))
  t)

(defun %unibus-map-vadr (unibus-map-page vadr)
  "setup unibus map so that unibus-map-page maps to the page of vadr, vadr has to be wired"
  (let* ((padr (sys:%physical-address vadr))
         (page-no (lsh padr -8)))
    (when *wesp-debug-unibus-mapping*
          (format t
              "unibus-map-page: ~D vadr: #x~X padr: #x~X page-no: #o~O~%"
            unibus-map-page
            vadr
            padr
            page-no))
    (%unibus-write (+ #o766140 (* unibus-map-page 2))
                   (dpb #b11 (byte 2 14) page-no)))
  t)

(defun wesp-mts (&optional value)
  (cond
   (value (%unibus-write *wesp-mts-addr* value))
   (t (%unibus-read *wesp-mts-addr*))))

(defun wesp-mtc (&optional value)
  (cond
   (value (%unibus-write *wesp-mtc-addr* value))
   (t (%unibus-read *wesp-mtc-addr*))))

(defun wesp-mtbrc (&optional value)
  (cond
   (value (%unibus-write *wesp-mtbrc-addr* value))
   (t (%unibus-read *wesp-mtbrc-addr*))))

(defun wesp-mtcma (&optional value)
  (cond
   (value (%unibus-write *wesp-mtcma-addr* value))
   (t (%unibus-read *wesp-mtcma-addr*))))

(defun wesp-controller-ready-p ()
  (ldb-test (byte 1 7) (wesp-mtc)))

(defun wesp-unit-ready-p ()
  (ldb-test (byte 1 0) (wesp-mts)))

(defun wesp-wait-controller-ready ()
  (or (wesp-controller-ready-p)
      (process-wait "tape ctrl" #'wesp-controller-ready-p))
  nil)

(defun wesp-wait-unit-ready ()
  (or (wesp-unit-ready-p)
      (process-wait "tape unit" #'wesp-unit-ready-p))
  nil)

(defun wesp-mtc-err-p ()
  (ldb-test (byte 1 15) (wesp-mtc)))

(defun wesp-mts-ilc-p ()
  (ldb-test (byte 1 15) (wesp-mts)))

(defun wesp-mts-eof-p ()
  (ldb-test (byte 1 14) (wesp-mts)))

(defun wesp-mts-eot-p ()
  (ldb-test (byte 1 10) (wesp-mts)))

(defun wesp-mts-rle-p ()
  (ldb-test (byte 1 9) (wesp-mts)))

(defun wesp-mts-bte-p ()
  (ldb-test (byte 1 8) (wesp-mts)))

(defun wesp-mts-nxm-p ()
  (ldb-test (byte 1 7) (wesp-mts)))

(defun wesp-mts-not-selr-p ()
  (not (ldb-test (byte 1 6) (wesp-mts))))

(defun throw-error-on-mtc-err ()
  (when (wesp-mtc-err-p)
        (cond
         ((wesp-mts-ilc-p) (throw 'ilc))
         ((wesp-mts-eof-p) (throw 'eof))
         ((wesp-mts-eot-p) (throw 'eot))
         ((wesp-mts-rle-p) (throw 'rle))
         ((wesp-mts-bte-p) (throw 'bte))
         ((wesp-mts-nxm-p) (throw 'nxm))
         (throw 'err))))

(defun wesp-power-clear ()
  (wesp-mtc (dpb 1 (byte 1 12) 0))
  t)

(defun wesp-select-unit (unit)
  (wesp-wait-controller-ready)
  (wesp-mtc (dpb unit (byte 3 8) (wesp-mtc)))
  (throw-error-on-mtc-err)
  (when (wesp-mts-not-selr-p) (throw 'selr))
  (wesp-wait-unit-ready)
  t)

(defun wesp-set-address (addr)
  ; ea never becomes > 0 in lispm
  ; because unibus map address is maximum #o177777
  (let ((ea (ldb (byte 2 17) addr))
        (cma (ldb (byte 16 0) addr)))
    (wesp-mtc (dpb ea (byte 2 4) (wesp-mtc)))
    (wesp-mtcma cma))
  t)

(defun wesp-go (function)
  "go with the function"
  (wesp-wait-controller-ready)
  (wesp-wait-unit-ready)
  (wesp-mtc (dpb 1 (byte 1 0) (dpb function (byte 3 1) (wesp-mtc))))
  (throw-error-on-mtc-err)
  (wesp-wait-unit-ready)
  t)

(defun wesp-available-units ()
  (let ((available-units nil))
    (dotimes (unit 8) (catch 'selr
                        (wesp-select-unit unit)
                        (setf available-units (cons unit available-units))))
    (reverse available-units)))

(defun %wesp-rewind-or-offline (offline-p)
  (if offline-p
      (wesp-go *wesp-command-rewind-offline*)
      (wesp-go *wesp-command-rewind*))
  t)

(defun wesp-offline () 
    (%wesp-rewind-or-offline t))

(defun wesp-read (arr)
  (let* ((offset-from-page-start (%unibus-map-array arr))
         (cma (+ #o140000 offset-from-page-start)))
    (wesp-set-address cma)
    (wesp-mtbrc (- (array-length arr)))
    (unwind-protect
        (wesp-go *wesp-command-read*)
      (%unibus-unmap-array arr)))
  (- (wesp-mtbrc) (array-length arr)))

(defun wesp-write (arr)
  (let* ((offset-from-page-start (%unibus-map-array arr))
         (cma (+ #o140000 offset-from-page-start)))
    (wesp-set-address cma)
    (wesp-mtbrc (- (array-length arr)))
    (unwind-protect
        (wesp-go *wesp-command-write*)
      (%unibus-unmap-array arr)))
  t)

(defun wesp-write-eof ()
  (wesp-go *wesp-command-write-eof*)
  t)

(defun wesp-space-for (&key (count 1))
  (wesp-mtbrc count)
  (wesp-go *wesp-command-space-for*)
  t)

(defun wesp-space-rev (&key (count 1))
  (wesp-mtbrc count)
  (wesp-go *wesp-command-space-rev*)
  t)

(defun wesp-rewind ()
  (%wesp-rewind-or-offline nil))

(defun arrays-equal (arr1 arr2)
     (cond
      ((dotimes (i (array-length arr1))
         (if (not (= (aref arr1 i) (aref arr2 i))) (return t))) nil)
      (t)))

(defun %wesp-test-write-read-compare-p (&key (count 256))
   (let* ((arr-to-write (wesp-make-array count))
          (arr-to-read (wesp-make-array count)))
        (wesp-fill-array-orderly arr-to-write)
        (wesp-rewind)
        (wesp-write arr-to-write)
        (wesp-rewind)
        (wesp-read arr-to-read)
        (arrays-equal arr-to-write arr-to-read)))

(defun wesp-test-write-read-compare (&key (unit 0) (start 4) (times 12))
  (fresh-line)
  (wesp-select-unit unit)
  (format t "Testing tape unit ~D with different record sizes...~%" unit)
  (dotimes (i times)
    (let ((count (lsh start i)))
      (format t "~D " count)
      (catch-all
       (unwind-protect
           (if (%wesp-test-write-read-compare-p :count count)
               (format t "OK")
               (format t "read != write"))
         (fresh-line)))))
  t)

(comment

 (defun wesp-space-test (&optional (unit 0))
   (let* ((arr-to-write (make-array 256 :type art-8b))
          (arr-to-read (make-array 256 :type art-8b)))
     ; initialize with ordered data (0x00-0xFF)
     (dotimes (i (array-length arr-to-write)) (aset (mod i 256.) arr-to-write i))
     (wesp-power-clear)
     (wesp-rewind unit)
     (wesp-write arr-to-write unit)
     ; read back
     (wesp-rewind unit)
     (wesp-read count arr-to-read unit)))

 (defun wesp-fill-test (&optional (unit 0))
   (let ((arr-to-write (make-array 4096 :type art-8b)))
     ; initialize with ordered data (0x00-0xFF)
     (dotimes (i (array-length arr-to-write)) (aset (mod i 256.) arr-to-write i))
     (wesp-wait-controller-ready)
     (wesp-power-clear)
     (wesp-select-unit unit)
     (wesp-wait-unit-ready)
     (wesp-rewind)
     (let* ((offset-from-page-start (%unibus-map-array arr-to-write))
            (cma (+ #o140000 offset-from-page-start)))
       (do-forever
        (wesp-set-address cma)
        (wesp-mtbrc (- (array-length arr-to-write)))
        (wesp-go %wesp-command-write)
        (wesp-wait-unit-ready)
        (when (wesp-is-mtc-err) (return)))
       (%unibus-unmap-array arr-to-write))))

)

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
(defparameter *wesp-max-retry-count* 0 "retry count of read and write operations, 0 means no retry, execute read/write only once")

;;; ARRAY-UNIBUS MAPPING FUNCTIONS

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



;;; REGISTER ACCESS FUNCTIONS

(defun %wesp-mts (&optional value)
  "read/write MTS (STATUS) register"
  (cond
   (value (%unibus-write *wesp-mts-addr* value))
   (t (%unibus-read *wesp-mts-addr*))))

(defun %wesp-mtc (&optional value)
  "read/write MTC (COMMAND) register"
  (cond
   (value (%unibus-write *wesp-mtc-addr* value))
   (t (%unibus-read *wesp-mtc-addr*))))

(defun %wesp-mtbrc (&optional value)
  "read/write MTBRC (BYTE/RECORD COUNT) register"
  (cond
   (value (%unibus-write *wesp-mtbrc-addr* value))
   (t (%unibus-read *wesp-mtbrc-addr*))))

(defun %wesp-mtcma (&optional value)
  "read/write MTCMA (CURRENT MEMORY ADDRESS) register"
  (cond
   (value (%unibus-write *wesp-mtcma-addr* value))
   (t (%unibus-read *wesp-mtcma-addr*))))

;;; REGISTER BIT FIELD PREDICATES

(defun %wesp-mtc-cur-p ()
  (ldb-test (byte 1 7) (%wesp-mtc)))

(defun %wesp-mts-ilc-p ()
  (ldb-test (byte 1 15) (%wesp-mts)))

(defun %wesp-mts-eof-p ()
  (ldb-test (byte 1 14) (%wesp-mts)))

(defun %wesp-mts-pae-p ()
  (ldb-test (byte 1 12) (%wesp-mts)))

(defun %wesp-mts-bgl-p ()
  (ldb-test (byte 1 11) (%wesp-mts)))

(defun %wesp-mts-eot-p ()
  (ldb-test (byte 1 10) (%wesp-mts)))

(defun %wesp-mts-rle-p ()
  (ldb-test (byte 1 9) (%wesp-mts)))

(defun %wesp-mts-bte-p ()
  (ldb-test (byte 1 8) (%wesp-mts)))

(defun %wesp-mts-nxm-p ()
  (ldb-test (byte 1 7) (%wesp-mts)))

(defun %wesp-mts-selr-p ()
  (ldb-test (byte 1 6) (%wesp-mts)))

(defun %wesp-mts-bot-p ()
  (ldb-test (byte 1 5) (%wesp-mts)))

(defun %wesp-mts-wrl-p ()
  (ldb-test (byte 1 2) (%wesp-mts)))

(defun %wesp-mts-tur-p ()
  (ldb-test (byte 1 0) (%wesp-mts)))

;;; WAIT FUNCTIONS

(defun %wesp-wait-controller-ready ()
  (or (%wesp-mtc-cur-p)
      (process-wait "tape ctrl" #'%wesp-mtc-cur-p))
  nil)

(defun %wesp-wait-unit-ready ()
  (or (%wesp-mts-tur-p)
      (process-wait "tape unit" #'%wesp-mts-tur-p))
  nil)

;;; HELPER FUNCTIONS FOR OPERATIONS

(defun %wesp-select-unit (unit)
  (%wesp-mtc (dpb unit (byte 3 8) (%wesp-mtc)))
  (unless (%wesp-mts-selr-p) (ferror nil "unit ~D is not online" unit))
  t)

(defun %wesp-set-address (addr)
  ; ea never becomes > 0 in lispm
  ; because unibus map address is maximum #o177777
  (let ((ea (ldb (byte 2 17) addr))
        (cma (ldb (byte 16 0) addr)))
    (%wesp-mtc (dpb ea (byte 2 4) (%wesp-mtc)))
    (%wesp-mtcma cma))
  t)

(defun %wesp-go (function)
  "go with the function"
  (%wesp-mtc (dpb 1 (byte 1 0) (dpb function (byte 3 1) (%wesp-mtc))))
  t)

;;; NON-COMMAND PUBLIC FUNCTIONS

(defun wesp-make-array (len)
  "make an appropriate array for wesp read and write"
  (make-array len :type art-8b))

(defun wesp-available-units ()
  "return the list of available tape units"
  (let ((available-units nil))
    (dotimes (unit 8)
      ;; dont use wesp-select-unit, it ferrors if unit is not available
      (%wesp-mtc (dpb unit (byte 3 8) (%wesp-mtc)))
      (if (%wesp-mts-selr-p)
          (setf available-units (cons unit available-units))))
    (reverse available-units)))

(defun wesp-power-clear ()
  "initiate power clear"
  (%wesp-mtc (dpb 1 (byte 1 12) 0))
  t)

;;; COMMAND PUBLIC FUNCTIONS

(defun wesp-offline (&key (unit 0))
  "execute Off Line command based on TC-131 Rewind Flow Char"
  (%wesp-wait-controller-ready)
  (%wesp-select-unit unit)
  (%wesp-wait-unit-ready)
  (%wesp-go *wesp-command-offline*)
  t)

;; if re-allocate-p is t, 
;; the size of arr can be adjusted if record length error is encountered
;; thus, it is important to use the returned value (in case arr is a new array)
;; if re-allocate-p is nil, arr is not changed
(defun wesp-read (arr &key (unit 0) (re-allocate-p nil))
  "execute Read command based on TC-131 Read Flow Chart"
  (let* ((offset-from-page-start (%unibus-map-array arr))
         (cma (+ #o140000 offset-from-page-start)))
    (unwind-protect
        (do ((retry-count 0)) (nil)
          (%wesp-wait-controller-ready)
          (%wesp-select-unit unit)
          (%wesp-wait-unit-ready)
          (when (%wesp-mts-wrl-p) (ferror nil "tape: operator error: tape ~D is write-locked" unit))
          (%wesp-set-address cma)
          (%wesp-mtbrc (- (array-length arr)))
          (%wesp-go *wesp-command-read*)
          (%wesp-wait-unit-ready)
          (when (or (%wesp-mts-bgl-p)
                    (%wesp-mts-nxm-p)
                    (%wesp-mts-ilc-p))
                (ferror nil "tape: fatal hardware error (data late, nxm or illegal)"))
          (unless (or (%wesp-mts-pae-p)
                      (%wesp-mts-bte-p))
            (if (%wesp-mts-rle-p)
                (if re-allocate-p
                    (setf arr (adjust-array (lsh (array-length arr) 2)))
                    (ferror nil "tape: arr is small (re-allocate memory)"))
                (return-from wesp-read arr)))
          (if (= retry-count *wesp-max-retry-count*)
              (ferror nil "tape: non-recoverable (max retry)"))
          (incf retry-count)
          (wesp-space-rev))
      (%unibus-unmap-array arr)))
  t)

(defun wesp-write (arr &key (unit 0))
  "execute Write command based on TC-131 Write Flow Chart"
  (let* ((offset-from-page-start (%unibus-map-array arr))
         (cma (+ #o140000 offset-from-page-start)))
    (unwind-protect
        (do ((retry-count 0)) (nil)
          (%wesp-wait-controller-ready)
          (%wesp-select-unit unit)
          (%wesp-wait-unit-ready)
          (when (%wesp-mts-wrl-p) (ferror nil "tape: operator error: tape ~D is write-locked" unit))
          (%wesp-set-address cma)
          (%wesp-mtbrc (- (array-length arr)))
          (if (= retry-count 0)
              (%wesp-go *wesp-command-write*)
              (%wesp-go *wesp-command-write-erg*))
          (%wesp-wait-unit-ready)
          (when (or (%wesp-mts-bgl-p)
                    (%wesp-mts-nxm-p)
                    (%wesp-mts-ilc-p))
                (ferror nil "tape: fatal hardware error (data late, nxm or illegal)"))
          (unless (or (%wesp-mts-pae-p)
                      (%wesp-mts-bte-p))
            (return-from wesp-write t))
          (if (= retry-count *wesp-max-retry-count*)
              (ferror nil "tape: fatal error (max retry)"))
          (incf retry-count)
          (wesp-space-rev))
      ; there is an erase in flow chart, what is that ? 
      (%unibus-unmap-array arr)))
  t)

(defun wesp-write-eof (&key (unit 0))
  "execute Write EOF command based on TC-131 Write EOF Flow Chart"
  (%wesp-wait-controller-ready)
  (%wesp-select-unit unit)
  (%wesp-wait-unit-ready)
  (when (%wesp-mts-wrl-p) (ferror nil "tape: operator error: tape ~D is write-locked" unit))
  (%wesp-go *wesp-command-write-eof*)
  (%wesp-wait-unit-ready)
  (when (or (%wesp-mts-bgl-p)
            (%wesp-mts-nxm-p)
            (%wesp-mts-ilc-p))
        (ferror nil "tape: fatal hardware error (bus late, nxm or illegal)"))
  (unless (%wesp-mts-eof-p)
    (ferror nil "tape: fatal error"))
  t)

(defun wesp-space-for (count &key (unit 0))
  "execute Space Forward command based on TC-131 Space Forward/Reverse Flow Chart"
  (%wesp-wait-controller-ready)
  (%wesp-select-unit unit)
  (%wesp-wait-unit-ready)
  (%wesp-mtbrc (- count))
  (%wesp-go *wesp-command-space-for*)
  t)

(defun wesp-space-rev (count &key (unit 0))
  "execute Space Reverse command based on TC-131 Space Forward/Reverse Flow Chart"
  (%wesp-wait-controller-ready)
  (%wesp-select-unit unit)
  (%wesp-wait-unit-ready)
  (%wesp-mtbrc (- count))
  (%wesp-go *wesp-command-space-rev*)
  t)

(defun wesp-rewind (&key (unit 0))
  "execute Rewind command based on TC-131 Rewind Flow Chart"
  (%wesp-wait-controller-ready)
  (%wesp-select-unit unit)
  (%wesp-wait-unit-ready)
  (%wesp-go *wesp-command-rewind*)
  t)

;;; TEST FUNCTIONS

(defun %wesp-test-print-array (arr)
  (fresh-line)
  (dotimes (i (array-length arr)) (format t "~2,'0x " (aref arr i)))
  nil)

(defun %wesp-test-fill-array-orderly (arr &key (from 0) (to 256))
  (dotimes (i (array-length arr)) (aset (+ (mod i (- to from)) from) arr i))
  arr)

(defun %wesp-test-fill-array-with (arr value)
  (dotimes (i (array-length arr)) (aset value arr i))
  arr)

(defun %wesp-test-arrays-equal-p (arr1 arr2)
  (assert (= (array-length arr1) (array-length arr2)) () "array lengths do not match")
  (dotimes (i (array-length arr1))
    (if (not (eql (aref arr1 i) (aref arr2 i)))
        (return-from %wesp-test-arrays-equal-p nil)))
  t)

(defun wesp-test-records (&key (unit 0) (min-record-length 4) (max-record-length 8192))
  (fresh-line)
  (format t "Testing tape unit ~D with different record sizes...~%" unit)
  (do ((record-length min-record-length (lsh record-length 1)))
      (nil)
    (if (> record-length max-record-length) (return t))
    (format t "~D " record-length)
    (let* ((arr-to-write (wesp-make-array record-length))
           (arr-to-read (wesp-make-array record-length)))
      (%wesp-test-fill-array-orderly arr-to-write)
      (wesp-rewind :unit unit)
      (wesp-write arr-to-write :unit unit)
      (wesp-rewind :unit unit)
      (wesp-read arr-to-read :unit unit)
      (assert (%wesp-test-arrays-equal-p arr-to-write arr-to-read) ()
              "write-read-compare failed at record-length: ~D bytes" record-length)))
  (format t "~%Test completed with success.~%")
  t)

(defun wesp-test-files (&key (unit 0) (record-size 4096))
  (fresh-line)
  (format t "Testing tape unit ~D with files...~%" unit)
  (format t "Preparing buffers...~%")
  ;; two files f1 and f2, each file has two records
  ;; buffer holds what is read back
  (let ((f1r1 (%wesp-test-fill-array-with (wesp-make-array record-size) (char-int 1)))
        (f1r2 (%wesp-test-fill-array-with (wesp-make-array record-size) (char-int 2)))
        (f2r1 (%wesp-test-fill-array-with (wesp-make-array record-size) (char-int 3)))
        (f2r2 (%wesp-test-fill-array-with (wesp-make-array record-size) (char-int 4)))
        (buffer (wesp-make-array record-size)))
    (wesp-rewind :unit unit)
    ;; write file-1
    (format t "Writing file-1 to tape...~%")
    (wesp-write f1r1 :unit unit)
    (wesp-write f1r2 :unit unit)
    (wesp-write-eof :unit unit)
    ;; write file-2
    (format t "Writing file-2 to tape...~%")
    (wesp-write f2r1 :unit unit)
    (wesp-write f2r2 :unit unit)
    (wesp-write-eof :unit unit)
    ;; read-compare files
    (wesp-rewind :unit unit)
    ;; read-compare file-1
    (defun test-file-1-and-eof ()
      (wesp-read buffer :unit unit)
      (assert (%wesp-test-arrays-equal-p buffer f1r1) ()
          "error reading file-1:record-1")
      (wesp-read buffer :unit unit)
      (assert (%wesp-test-arrays-equal-p buffer f1r2) ()
          "error reading file-1:record-2")
      (assert (%wesp-mts-eof-p) ()
          "no EOF found after file-1"))
    (format t "Testing the records of file-1 and EOF...~%")
    (test-file-1-and-eof)
    ;; read-compare file-2
    (defun test-file-2-and-eof () 
      (wesp-read buffer :unit unit)
      (assert (%wesp-test-arrays-equal-p buffer f2r1) ()
          "error reading file-2:record-1")
      (wesp-read buffer :unit unit)
      (assert (%wesp-test-arrays-equal-p buffer f2r2) ()
          "error reading file-2:record-2")
      (assert (%wesp-mts-eof-p) ()
          "no EOF found after file-2"))
    (format t "Testing the records of file-2 and EOF...~%")
    (test-file-2-and-eof)
    ;; skip file-1/record-1 and read record-2
    (format t "Testing skipping file-1:record-1 (space forward 1) and reading file-1:record-2...~%")
    (wesp-rewind :unit unit)
    (wesp-space-for 1)
    (wesp-read buffer :unit unit)
    (assert (%wesp-test-arrays-equal-p buffer f1r2) ()
        "error reading file-1:record-2")
    (assert (%wesp-mts-eof-p) ()
        "no EOF found after file-1")
    ;; skip file-1 and read-compare file-2
    (format t "Testing skipping file-1 (space forward 0) and reading file-2...~%")
    (wesp-rewind :unit unit)
    (wesp-space-for 0 :unit unit)
    (test-file-2-and-eof)
    ;; skip end of file-2 (space reverse 1) and read record-1
    (format t "Testing skipping reverse from end of file-2 (space reverse 2) and reading file-2:record-1...~%")
    (wesp-space-rev 2 :unit unit)
    (wesp-read buffer :unit unit)
    (assert (%wesp-test-arrays-equal-p buffer f2r1) ()
        "error reading file-2:record-1")
    ;; go to end of file-2 (2x space forward 0), then go back to start of file-2 (space reverse 0)
    (format t "Testing skipping both files (2x space forward 0) then skipping reverse to start of file-2 (space reverse 0) and reading file-2:record-1...~%")
    (wesp-rewind :unit unit)
    ;; skip file-1
    (wesp-space-for 0 :unit unit)
    ;; skip file-2
    (wesp-space-for 0 :unit unit)
    ;; go back to file-2
    (wesp-space-rev 0 :unit unit)
    ;; read file-2:record-1
    (wesp-read buffer :unit unit)
    (assert (%wesp-test-arrays-equal-p buffer f2r1) ()
        "error reading file-2:record-1")
    (format t "Test completed with success.~%"))
  t)

(defun wesp-test-space-ops (&key (unit 0))
  (fresh-line)
  (format t "Testing tape unit ~D for space operations...~%" unit)
  (format t "Testing moving back beyond the beginning of tape...~&")
  (wesp-rewind :unit unit)
  (assert (%wesp-mts-bot-p) () "bot not set after rewind")
  ; these should not crash
  (wesp-space-rev 0 :unit unit)
  (wesp-space-rev 1 :unit unit)
  (assert (%wesp-mts-bot-p) () "bot not set after space-rev")
  (comment
   (format t "Testing moving forward beyond the end of tape...~&")
   (let ((arr (%wesp-test-fill-array-orderly (wesp-make-array 4096))))
     (do () (nil)
       (wesp-write arr :unit unit)
       (when (%wesp-mts-eot-p) (return))))
    ; these should not crash
    (wesp-space-for 0 :unit unit)
    (assert (%wesp-mts-eot-p) () "eot not set after space-for")
    (wesp-space-for 1 :unit unit)
    (assert (%wesp-mts-eot-p) () "eot not set after space-for"))
  (format t "Test completed with success.~%")
  t)

(defun wesp-test-all (&key (unit 0))
  (wesp-test-records :unit unit)
  (wesp-test-files :unit unit)
  (wesp-test-space-ops :unit unit)
  t)
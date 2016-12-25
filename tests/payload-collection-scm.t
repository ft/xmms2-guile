;; -*- scheme -*-

;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test payload)
             (test setup)
             (xmms2 types)
             (xmms2 payload))

(init-test-tap!)

(define-syntax test-><-collection?
  (syntax-rules ()
    ((_ c)
     (begin (perform-payload-><-test make-collection-payload
                                     payload->collection
                                     c pass-if-equal?)
            (perform-payload-><-test make-value-payload
                                     payload->collection
                                     c pass-if-equal?)
            (perform-payload-><-test make-collection-payload
                                     payload->value
                                     c pass-if-equal?)
            (perform-payload-><-test make-value-payload
                                     payload->value
                                     c pass-if-equal?)))))

(define *tests-per-back-and-forth* 4)

(define *manual-universe*
  #vu8(0 0 0 4     ; Value-Type: Collection
       0 0 0 1     ; Collection-Type: Universe
       0 0 0 0     ; Size of Attribute Dictionary (here: empty)
       0 0 0 2     ; ID List Type: Int64
       0 0 0 0     ; ID List Size: Empty
       0 0 0 4     ; List of child nodes
       0 0 0 0))   ; Empty

;; This is an example fetched from an actual server:
;;
;; xmms2 coll create big-three artist:Slayer \
;;                          OR artist:Metallica \
;;                          OR artist:Anthrax
(define *big-three-payload*
  #vu8(0 0 0 4     ; Value-Type: Collection
       0 0 0 2     ; Collection-Type: Union
       0 0 0 0     ; Size of Attribute Dictionary (here: empty)
       0 0 0 2     ; ID List Type: Int64
       0 0 0 0     ; ID List Size: Empty
       0 0 0 4     ; List of child nodes
       0 0 0 3     ; Root node has three children

       ;; First node:
       0 0 0 8    ; Collection-Type: Equals
       0 0 0 2    ; Size of Attribute Dictionary: 2
       0 0 0 6 102 105 101 108 100 0  ; Key: field
       0 0 0 3    ; Value Type: String
       0 0 0 7 97 114 116 105 115 116 0 ; Value: "artist"
       0 0 0 6 118 97 108 117 101 0  ; Key: value
       0 0 0 3    ; Value Type: String
       0 0 0 7 83 108 97 121 101 114 0 ; Value: "Slayer"
       0 0 0 2    ; ID List Type: Int64
       0 0 0 0    ; ID List Size: Empty
       0 0 0 4    ; List of child nodes
       0 0 0 1    ; Number of entries: 1

       ;; Children of first node:
       0 0 0 1    ; Collection-Type: Universe
       0 0 0 0    ; Size of Attribute Dictionary (here: empty)
       0 0 0 2    ; ID List Type: Int64
       0 0 0 0    ; ID List Size: Empty
       0 0 0 4    ; List of child nodes
       0 0 0 0    ; Empty

       ;; Second node:
       0 0 0 8    ; Collection-Type: Equals
       0 0 0 2    ; Size of Attribute Dictionary: 2
       0 0 0 6 102 105 101 108 100 0 ; Key: field
       0 0 0 3    ; Value Type: String
       0 0 0 7 97 114 116 105 115 116 0 ; Value: "artist"
       0 0 0 6 118 97 108 117 101 0 ; Key: value
       0 0 0 3    ; Value Type: String
       0 0 0 10 77 101 116 97 108 108 105 99 97 0 ; Value: "Metallica"
       0 0 0 2    ; ID List Type: Int64
       0 0 0 0    ; ID List Size: Empty
       0 0 0 4    ; List of child nodes
       0 0 0 1    ; Number of entries: 1

       ;; Children of second node:
       0 0 0 1   ; Collection-Type: Universe
       0 0 0 0   ; Size of Attribute Dictionary (here: empty)
       0 0 0 2   ; ID List Type: Int64
       0 0 0 0   ; ID List Size: Empty
       0 0 0 4   ; List of child nodes
       0 0 0 0   ; Empty

       ;; Third node:
       0 0 0 8    ; Collection-Type: Equals
       0 0 0 2    ; Size of Attribute Dictionary: 2
       0 0 0 6 102 105 101 108 100 0 ; Key: field
       0 0 0 3    ; Value Type: String
       0 0 0 7 97 114 116 105 115 116 0 ; Value: "artist"
       0 0 0 6 118 97 108 117 101 0 ; Key: value
       0 0 0 3    ; Value Type: String
       0 0 0 8 65 110 116 104 114 97 120 0 ; Value: "Anthrax"
       0 0 0 2    ; ID List Type: Int64
       0 0 0 0    ; ID List Size: Empty
       0 0 0 4    ; List of child nodes
       0 0 0 1    ; Number of entries: 1

       ;; Children of third node:
       0 0 0 1   ; Collection-Type: Universe
       0 0 0 0   ; Size of Attribute Dictionary (here: empty)
       0 0 0 2   ; ID List Type: Int64
       0 0 0 0   ; ID List Size: Empty
       0 0 0 4   ; List of child nodes
       0 0 0 0)) ; Empty

(define *big-three* (collection (∪ (artist = Slayer)
                                   (artist = Metallica)
                                   (artist = Anthrax))))

(with-fs-test-bundle
 (plan (+ (* 1 *tests-per-back-and-forth*)
          2))

 (define-test "manual-universe example payload gets parsed correctly"
   (pass-if-equal? (payload->collection *manual-universe*)
                   (make-universe)))

 (define-test "big-three example payload gets parsed correctly"
   (pass-if-equal? (payload->collection *big-three-payload*)
                   *big-three*))

 (test-><-collection? (collection (artist = Slayer))))

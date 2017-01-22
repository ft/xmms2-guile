;; -*- scheme -*-

;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (ice-9 iconv)
             (rnrs bytevectors)
             (xmms2 values))

(init-test-tap!)
(setlocale LC_ALL "")

(with-fs-test-bundle
 (plan 8)
 (define-test "Simple string encodes correctly"
   (pass-if-string=? (encode-url "foobar")
                     "foobar"))
 (define-test "Simple bytevector encodes correctly"
   (pass-if-string=? (encode-url (string->utf8 "foobar"))
                     "foobar"))
 (define-test "String with spaces"
   (pass-if-string=? (encode-url "foo bar")
                     "foo+bar"))
 (define-test "Bytevector with spaces"
   (pass-if-string=? (encode-url (string->utf8 "foo bar"))
                     "foo+bar"))
 (define-test "Bytevector some weird bytes, that don't really work in UTF-8"
   (pass-if-string=? (encode-url #vu8(#x66 #x6f #x6f #xf3 #x62 #x61 #x72 #xff))
                     "foo%f3bar%ff"))
 (define-test "Simple string decodes correctly"
   (pass-if-equal? (decode-url "foobar")
                   #vu8(#x66 #x6f #x6f #x62 #x61 #x72)))
 (define-test "String with + decodes correctly"
   (pass-if-equal? (decode-url "foo+bar")
                   #vu8(#x66 #x6f #x6f #x20 #x62 #x61 #x72)))
 (define-test "Decode to weird bytevector"
   (pass-if-equal? (decode-url "foo%f3bar%ff")
                   #vu8(#x66 #x6f #x6f #xf3 #x62 #x61 #x72 #xff))))

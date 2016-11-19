;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 constants playback)
  #:use-module (xmms2 constants)
  #:use-module (xmms2 enumeration))

(define-enum (<> xref-playback-cmds)
  (CMD-START FIRST-CMD-ID)
  CMD-STOP
  CMD-PAUSE
  CMD-KILL-DECODER
  CMD-GET-PLAYTIME
  CMD-SEEK/MILLISECONDS
  CMD-SEEK/SAMPLES
  CMD-GET-STATUS
  CMD-GET-CURRENT-IDENTIFIER
  CMD-SET-VOLUME
  CMD-GET-VOLUME)

(define-enum (<> xref-playback-states)
  PLAYBACK-STATUS-STOP
  PLAYBACK-STATUS-PLAY
  PLAYBACK-STATUS-PAUSE)

;; Yes, this one starts at one. I don't know why.
(define-enum (<> xref-playback-seek-modes)
  (PLAYBACK-SEEK-CURRENT 1)
  PLAYBACK-SEEK-SET)

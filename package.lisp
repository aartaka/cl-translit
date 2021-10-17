;;;; package.lisp

(defpackage #:cl-translit
  (:nicknames #:translit)
  (:use #:cl)
  (:export
   #:*consonants*
   #:*softeners*
   #:*vowels*
   #:*stop-char*
   #:*apostrophe-char*
   #:build-encode-dict
   #:build-decode-dict
   #:cyr2url
   #:url2cyr
   #:cyr-to-url
   #:url-to-cyr))

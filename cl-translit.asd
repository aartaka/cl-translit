;;;; cl-translit.asd

(asdf:defsystem #:cl-translit
  :description "Common Lisp Cyrillic transliteration library"
  :author "Artyom Bologov"
  :license "BSD-2 Clause"
  :version "0.0.1"
  :serial t
  :depends-on (alexandria)
  :components ((:file "package")
               (:file "cyr2url")))

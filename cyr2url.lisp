(in-package #:cl-translit)

(defvar *consonants*
  '(("б" ."b")
    ("в" ."v")
    ("г" ."gh")
    ("д" ."d")
    ("ж" ."zh")
    ("з" ."z")
    ("к" ."k")
    ("л" ."l")
    ("м" ."m")
    ("н" ."n")
    ("п" ."p")
    ("р" ."r")
    ("с" ."s")
    ("т" ."t")
    ("ц" ."c")
    ("ч" ."ch")
    ("ш" ."sh")
    ("щ" ."shh")
    ("ф" ."f")
    ("х" ."kh"))
  "An alist of Cyrillic consonants associated to the respective Latin
  transliterations.")

(defvar *softeners*
  '(("є" ."je")
    ("ю" ."ju")
    ("я" ."ja")
    ("ё" ."joh"))
  "An alist of Cyrillic special consonant-softening characters
  associated to the respective Latin transliterations.")

(defvar *vowels*
  '(("а" ."a")
    ("е" ."e")
    ("и" ."y")
    ("і" ."i")
    ("о" ."o")
    ("у" ."u")
    ("ы" ."yh")
    ("э" ."eh"))
  "An alist of Cyrillic special vowel characters associated to the
  respective Latin transliterations.")

(defvar *stop-char* #\.
  "A character to insert where some interjection is needed.")
;; REVIEW: Why, actually?
(defvar *apostrophe-char* #\’
  "A translation of apostrophe.")

(declaim (ftype (function (list list list list (or string character) (or string character))
                          (values hash-table list))
                build-encode-dict build-decode-dict))
(defun build-encode-dict (consonants softeners vowels additional-translations apostrophe-char stop-char)
  "Build a full encoding (cur -> url) dictionary, based on the arguments provided.
Some tricky combinations, like those with й and ь are added on top.
CONSONANTS, SOFTENERS, and VOWELS are additionally upcased.
ADDITIONAL-TRANSLATIONS are simmply added on top."
  (flet ((concat (&rest strings)
           (apply #'concatenate 'string strings))
         (cap (string)
           (format nil "~:(~a~)" string)))
    (let* ((dict
             (append
              ;; TODO: This should be handled in a prettier way.
              ;; Maybe extract into a variable?
              `(("йе" . ,(concat "j" (string stop-char) "e"))
                ("йу" . ,(concat "j" (string stop-char) "u"))
                ("йа" . ,(concat "j" (string stop-char) "a"))
                ("йі" . ,(concat "j" (string stop-char) "i"))
                ("ї" . "ji")
                ("й" . "j")
                ;; A standalone ь needs to be distinguished from й
                ("ь" . ,(concat (string stop-char) "j"))
                ("ў" ."w")
                ("ъ" . ,(concat (string apostrophe-char) "h"))
                ("’" . ,(string (string apostrophe-char))))
              consonants softeners vowels
                     ;; Dictionary pairs: to differentiate 'ь' and 'й' after a consonant,
                     (mapcar (lambda (pair) (cons (concat (first pair) "ь")
                                                  (concat (rest pair) "j")))
                             consonants)
                     (mapcar (lambda (pair) (cons (concat (first pair) "й")
                                                  (concat (rest pair) (string stop-char) "j")))
                             consonants)
                     ;; to differentiate 'тя' -> 'tja' from 'тьа' etc.
                     (mapcar (lambda (cons soft)
                               (cons (concat (first cons) (first soft))
                                     (concat (rest cons) (rest soft))))
                             consonants
                             softeners)
                     ;; to differentiate 'йа` -> 'j.a' from 'я' -> 'ja' etc.
                     (mapcar (lambda (cons vow)
                               (cons (concat (first cons) "й" (first vow))
                                     (concat (rest cons) (string stop-char) "j" (rest vow))))
                             consonants
                             vowels)
                     additional-translations))
           ;; Add capitalizations
           (dict (append
                  dict
                  (mapcar (lambda (pair)
                            (cons (cap (first pair))
                                  (cap (rest pair))))
                          dict))))
      (values (alexandria:alist-hash-table dict :test #'equal)
              dict))))

(defun build-decode-dict (consonants softeners vowels additional-translations apostrophe-char stop-char)
  "Build a dictionary for url -> cyr transliteration.
Arguments are basically the same as in `build-encode-dict'."
  (let* ((dict-alist (nth-value
                      1 (build-encode-dict consonants softeners vowels
                                           additional-translations apostrophe-char stop-char)))
         (reverse-alist (mapcar (lambda (pair) (cons (rest pair) (first pair))) dict-alist)))
    (values (alexandria:alist-hash-table reverse-alist :test #'equal)
            reverse-alist)))

(declaim (ftype (function (string &key (:consonants list)
                                  (:softeners list)
                                  (:vowels list)
                                  (:additional-translations list)
                                  (:apostrophe-char (or string character))
                                  (:stop-char (or string character))
                                  (:dictionary hash-table))
                          string)
                cyr2url cyr-to-url url2cyr url-to-cyr cyr->url url->cyr))
(defun cyr2url (string &key (consonants *consonants*) (softeners *softeners*) (vowels *vowels*)
                         (additional-translations nil)
                         (apostrophe-char *apostrophe-char*) (stop-char *stop-char*)
                         (dictionary (build-encode-dict consonants softeners vowels
                                                        additional-translations apostrophe-char stop-char)))
  "Translate a Cyrillic text to Latin URL-compatible text.

CONSONANTS, SOFTENERS, VOWELS, and ADDITIONAL-TRANSLATIONS should be
alists from Cyrillic characters to Latinic ones. Default values are
`*consonants*`, `*softeners*`, `*vowels*`, and nil respectively.
APOSTROPHE-CHAR and STOP-CHAR are characters/strings to insert in
special cases. Defaults to `*apostrophe-char*' and `*stop-char*'. See
the respective documentation for when those are inserted."
  (loop with max-substring-length
          = (apply #'max (mapcar #'length (alexandria:hash-table-keys dictionary)))
        for i below (length string)
        collect (loop for slice-length from (min
                                             (- (length string) 1 i)
                                             (1- max-substring-length))
                      downto 1
                      for transliteration = (gethash (subseq string i (+ i slice-length))
                                                     dictionary)
                      when transliteration
                        do (progn
                             (incf i (1- slice-length))
                             (return transliteration))
                      finally (return (subseq string i (1+ i))))
          into transliterated-list
        finally (return (apply #'concatenate 'string transliterated-list))))

(defun cyr->url (string &key (consonants *consonants*) (softeners *softeners*) (vowels *vowels*)
                          (additional-translations nil)
                          (apostrophe-char *apostrophe-char*) (stop-char *stop-char*)
                          (dictionary (build-encode-dict consonants softeners vowels
                                                         additional-translations apostrophe-char stop-char)))
  "Alias of `cur2url'."
  (cyr2url string :consonants consonants
                  :softeners softeners
                  :vowels vowels
                  :additional-translations additional-translations
                  :apostrophe-char apostrophe-char
                  :stop-char stop-char
                  :dictionary dictionary))

(defun cyr-to-url (string &key (consonants *consonants*) (softeners *softeners*) (vowels *vowels*)
                            (additional-translations nil)
                            (apostrophe-char *apostrophe-char*) (stop-char *stop-char*)
                            (dictionary (build-encode-dict consonants softeners vowels
                                                           additional-translations apostrophe-char stop-char)))
  "Alias of `cur2url'."
  (cyr2url string :consonants consonants
                  :softeners softeners
                  :vowels vowels
                  :additional-translations additional-translations
                  :apostrophe-char apostrophe-char
                  :stop-char stop-char
                  :dictionary dictionary))

(defun url2cyr (string &key (consonants *consonants*) (softeners *softeners*) (vowels *vowels*)
                         (additional-translations nil)
                         (apostrophe-char *apostrophe-char*) (stop-char *stop-char*)
                         (dictionary (build-decode-dict consonants softeners vowels
                                                        additional-translations apostrophe-char stop-char)))
  "Translate a Latin URL-compatible text to Cyrillic text.

Arguments are the same as for `cyr2url', except that DICTIONARY should
be a decoding dictionary (see `build-decode-dict')."
  (cyr2url string :consonants consonants
                  :softeners softeners
                  :vowels vowels
                  :additional-translations additional-translations
                  :apostrophe-char apostrophe-char
                  :stop-char stop-char
                  :dictionary dictionary))

(defun url->cyr (string &key (consonants *consonants*) (softeners *softeners*) (vowels *vowels*)
                          (additional-translations nil)
                          (apostrophe-char *apostrophe-char*) (stop-char *stop-char*)
                          (dictionary (build-decode-dict consonants softeners vowels
                                                         additional-translations apostrophe-char stop-char)))
  "Alias for `url2cyr'."
  (url2cyr string :consonants consonants
                  :softeners softeners
                  :vowels vowels
                  :additional-translations additional-translations
                  :apostrophe-char apostrophe-char
                  :stop-char stop-char
                  :dictionary dictionary))

(defun url-to-cyr (string &key (consonants *consonants*) (softeners *softeners*) (vowels *vowels*)
                            (additional-translations nil)
                            (apostrophe-char *apostrophe-char*) (stop-char *stop-char*)
                            (dictionary (build-decode-dict consonants softeners vowels
                                                           additional-translations apostrophe-char stop-char)))
  "Alias for `url2cyr'."
  (url2cyr string :consonants consonants
                  :softeners softeners
                  :vowels vowels
                  :additional-translations additional-translations
                  :apostrophe-char apostrophe-char
                  :stop-char stop-char
                  :dictionary dictionary))

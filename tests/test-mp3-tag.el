;;; test-mp3-tag.el --- Tests for mp3-tag -*- lexical-binding: t; -*-

(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(require 'ert)
(require 'mp3-tag)

(ert-deftest mp3-tag-get-marked-dired-files-filters-mp3s ()
  "Check marked Dired files are filtered to `.mp3' entries."
  (cl-letf (((symbol-function 'dired-get-marked-files)
             (lambda (_localp _arg filter &optional _distinguish-one-marked error)
               (should (functionp filter))
               (should (funcall filter "/tmp/a.mp3"))
               (should-not (funcall filter "/tmp/a.flac"))
               (unless error
                 '("/tmp/a.mp3")))))
    (should (equal (mp3-tag--get-marked-dired-files)
                   '("/tmp/a.mp3")))))

(ert-deftest mp3-tag-edit-uses-dired-filter-function ()
  "Check `mp3-tag-edit' passes a predicate to `dired-get-marked-files'."
  (cl-letf (((symbol-function 'derived-mode-p)
             (lambda (_mode) t))
            ((symbol-function 'dired-get-marked-files)
             (lambda (_localp _arg filter &optional _distinguish-one-marked error)
               (should (functionp filter))
               (should (funcall filter "/tmp/a.mp3"))
               (should-not (funcall filter "/tmp/a.flac"))
               (unless error
                 '("/tmp/a.mp3"))))
            ((symbol-function 'mp3-tag-read-files)
             (lambda (files)
               (should (equal files '("/tmp/a.mp3")))
               '(((filename . "/tmp/a.mp3")
                  (TIT2 . "Track 1")))))
            ((symbol-function 'mp3-tag--edit-table)
             (lambda (table) table)))
    (should (equal (mp3-tag-edit)
                   '(("Filename" "Title")
                     ("/tmp/a.mp3" "Track 1"))))))

(ert-deftest mp3-tag-read-file-includes-filename ()
  "Check `mp3-tag-read-file' adds FILE to the returned alist."
  (cl-letf (((symbol-function 'mp3-tag--read-json-with-mutagen)
             (lambda (_file) "{\"title\":\"Track 1\"}"))
            ((symbol-function 'json-parse-string)
             (lambda (_json &rest _args)
               '((title . "Track 1")))))
    (should (equal (mp3-tag-read-file "Track 1.mp3")
                   '((filename . "Track 1.mp3")
                     (title . "Track 1"))))))

(ert-deftest mp3-tag-id3-name-to-common-sense-converts-known-fields ()
  "Check raw ID3 field names are converted to readable names."
  (should (equal (mp3-tag--id3-name-to-common-sense "TALB")
                 "Album"))
  (should (equal (mp3-tag--id3-name-to-common-sense "TPE2")
                 "Album Artist"))
  (should (equal (mp3-tag--id3-name-to-common-sense "filename")
                 "Filename"))
  (should (equal (mp3-tag--id3-name-to-common-sense "TXXX:Custom")
                 "TXXX:Custom")))

(ert-deftest mp3-tag-common-sense-name-to-id3-converts-known-fields ()
  "Check readable names are converted back to raw ID3 field names."
  (should (equal (mp3-tag--common-sense-name-to-id3 "Album")
                 "TALB"))
  (should (equal (mp3-tag--common-sense-name-to-id3 "Album Artist")
                 "TPE2"))
  (should (equal (mp3-tag--common-sense-name-to-id3 "Filename")
                 "filename"))
  (should (equal (mp3-tag--common-sense-name-to-id3 "TXXX:Custom")
                 "TXXX:Custom")))

(ert-deftest mp3-tag-write-tags-with-mutagen-encodes-json-payload ()
  "Check write helper sends a JSON payload with string keys."
  (let (command program args)
    (cl-letf (((symbol-function 'call-process)
               (lambda (prog _in _dest _display &rest rest)
                 (setq program prog
                       args rest)
                 0))
              ((symbol-function 'delete-file)
               (lambda (&rest _) nil)))
      (mp3-tag--write-tags-with-mutagen
       "track.mp3"
       '((TALB . "Live")
         (TRCK . "1/2")
         (TXXX:Custom . "Value")))
      (setq command (car (last args)))
      (should (equal program mp3-tag-python-command))
      (should (equal (nth 0 args) "-c"))
      (should (equal (nth 1 args) mp3-tag--mutagen-write-script))
      (should (string-match-p "\"TALB\":\"Live\"" command))
      (should (string-match-p "\"TRCK\":\"1/2\"" command))
      (should (string-match-p "\"TXXX:Custom\":\"Value\"" command)))))

(ert-deftest mp3-tag-update-writes-tags-with-mutagen ()
  "Check `mp3-tag-update' routes writes through Mutagen."
  (let (writes)
    (cl-letf (((symbol-function 'mp3-tag-org-table-to-lisp)
               (lambda ()
                 '(("Filename" "Disc" "Title")
                   ("track.mp3" "1/2" "Track 1"))))
              ((symbol-function 'mp3-tag--write-tags-with-mutagen)
               (lambda (file tags)
                 (push (cons file tags) writes)))
              ((symbol-function 'kill-buffer)
               (lambda (&rest _) nil)))
      (mp3-tag-update)
      (should (equal writes
                     '(("track.mp3"
                        (TPOS . "1/2")
                        (TIT2 . "Track 1"))))))))

(ert-deftest mp3-tag-conversion-round-trip ()
  "Check round-trip conversion between alists and tables."
  (let* ((alist '(((TIT2 . "Track 1")
                   (TPE1 . "Artist")
                   (TALB . "Live")
                   (TRCK . "1"))
                  ((TIT2 . "Track 2")
                   (TPE1 . "Artist")
                   (TALB . "Live")
                   (TRCK . "2"))))
         (table '(("Title" "Artist" "Album" "Track")
                  ("Track 1" "Artist" "Live" "1")
                  ("Track 2" "Artist" "Live" "2"))))
    (should (equal (mp3-tag--alist-to-table alist) table))
    (should (equal (mp3-tag--table-to-alist table) alist))
    (should (equal (mp3-tag--table-to-alist
                    (mp3-tag--alist-to-table alist))
                   alist))
    (should (equal (mp3-tag--alist-to-table
                   (mp3-tag--table-to-alist table))
                   table))))

(ert-deftest mp3-tag-alist-to-table-handles-heterogeneous-tracks ()
  "Check heterogeneous track alists are aligned to a shared header row."
  (let ((tracks '(((filename . "1.mp3")
                   (TALB . "Album 1")
                   (TIT2 . "Track 1"))
                  ((filename . "2.mp3")
                   (TCON . "Other")
                   (TPE1 . "Happy")
                   (TRCK . "2"))
                  ((filename . "3.mp3")
                   (TALB . "Album 3")
                   (TCON . "Other")
                   (TIT2 . "Track 3")
                   (TPE1 . "Happy")
                   (TRCK . "3")
                   (TYER . "1994")))))
    (should (equal (mp3-tag--alist-to-table tracks)
                   '(("Filename" "Album" "Title" "Genre" "Artist" "Track" "Year")
                     ("1.mp3" "Album 1" "Track 1" "" "" "" "")
                     ("2.mp3" "" "" "Other" "Happy" "2" "")
                     ("3.mp3" "Album 3" "Track 3" "Other" "Happy" "3" "1994"))))))

(ert-deftest mp3-tag-drop-empty-columns-removes-all-empty-columns ()
  "Check columns with only empty data cells are removed."
  (should
   (equal
    (mp3-tag--drop-empty-columns
     '(("Filename" "Album" "Composer" "Title")
       ("1.mp3" "Album 1" "" "Track 1")
       ("2.mp3" "" "" "Track 2")))
    '(("Filename" "Album" "Title")
      ("1.mp3" "Album 1" "Track 1")
      ("2.mp3" "" "Track 2")))))

(ert-deftest mp3-tag-table-to-orgtbl-adds-hlines ()
  "Check Org table output includes hlines around the header and at the end."
  (let ((table '(("title" "artist")
                 ("Track 1" "Artist")
                 ("Track 2" "Artist"))))
    (should (equal (mp3-tag--table-to-orgtbl table)
                   "|---------+--------|\n| title   | artist |\n|---------+--------|\n| Track 1 | Artist |\n| Track 2 | Artist |\n|---------+--------|"))))

(ert-deftest mp3-tag-table-to-orgtbl-drops-all-empty-columns ()
  "Check Org table rendering omits columns whose values are all empty."
  (let ((table '(("Filename" "Album" "Composer" "Title")
                 ("1.mp3" "Album 1" "" "Track 1")
                 ("2.mp3" "" "" "Track 2"))))
    (should-not (string-match-p "Composer" (mp3-tag--table-to-orgtbl table)))
    (should (string-match-p "Filename" (mp3-tag--table-to-orgtbl table)))
    (should (string-match-p "Title" (mp3-tag--table-to-orgtbl table)))))

;;; test-mp3-tag.el ends here

;;; mp3-tag.el --- Package for tagging MP3 Files -*- lexical-binding: t; -*-

;; Author: Andrew Peck
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.7"))
;; Keywords: multimedia music mp3 org
;; URL: https://github.com/andrewpeck/mp3-tag.el

;;; Commentary:

;; Simple Org-table-based editing for MP3 ID3 tags.
;; Reads and writes tags via Python Mutagen.

;;; Code:

(require 'org-table)
(require 'json)
(require 'dired)

(defconst mp3-tag--mutagen-script
  (string-join
   '("import json, sys"
     "from mutagen.id3 import ID3"
     "tags = ID3(sys.argv[1])"
     "out = {}"
     "for key, val in tags.items():"
     "    out[key] = str(val)"
     "print(json.dumps(out))")
   "\n")
  "Python script used to read ID3 tags via Mutagen.")

(defconst mp3-tag--mutagen-write-script
  (string-join
   '("import json, sys"
     "from mutagen.id3 import ID3, Frames, ID3NoHeaderError, TXXX, TextFrame"
     "path = sys.argv[1]"
     "updates = json.loads(sys.argv[2])"
     "try:"
     "    tags = ID3(path)"
     "except ID3NoHeaderError:"
     "    tags = ID3()"
     "for key, value in updates.items():"
     "    if key == 'filename':"
     "        continue"
     "    if key.startswith('TXXX:'):"
     "        desc = key.split(':', 1)[1]"
     "        tags.delall(key)"
     "        if value:"
     "            tags.add(TXXX(encoding=3, desc=desc, text=[value]))"
     "        continue"
     "    frame_cls = Frames.get(key)"
     "    if frame_cls is None or not issubclass(frame_cls, TextFrame):"
     "        raise ValueError(f'Unsupported ID3 field: {key}')"
     "    if value:"
     "        tags.setall(key, [frame_cls(encoding=3, text=[value])])"
     "    else:"
     "        tags.delall(key)"
     "tags.save(path)")
   "\n")
  "Python script used to write ID3 tags via Mutagen.")

(defvar mp3-tag-python-command
  (or (and (file-executable-p "/usr/bin/python3") "/usr/bin/python3")
      (executable-find "python3"))
  "Python executable used for reading ID3 tags with Mutagen.")

(defvar mp3-tag--id3-common-sense-name-map
  '(("filename" . "Filename")
    ("TALB" . "Album")
    ("TIT2" . "Title")
    ("TPE1" . "Artist")
    ("TPE2" . "Album Artist")
    ("TRCK" . "Track")
    ("TPOS" . "Disc")
    ("TCON" . "Genre")
    ("TCOM" . "Composer")
    ("TDRC" . "Date")
    ("TYER" . "Year"))
  "Mapping of raw ID3 field names to readable table header names.")

(defun mp3-tag--id3-name-to-common-sense (name)
  "Convert raw ID3 NAME to a readable table header name."
  (or (cdr (assoc name mp3-tag--id3-common-sense-name-map))
      name))

(defun mp3-tag--common-sense-name-to-id3 (name)
  "Convert readable table header NAME back to a raw ID3 field name."
  (or (car (rassoc name mp3-tag--id3-common-sense-name-map))
      name))

(defun mp3-tag--read-json-with-mutagen (file)
  "Read ID3 metadata for FILE as JSON using Python and Mutagen."
  (unless mp3-tag-python-command
    (error "Executable for python3 not found"))
  (with-temp-buffer
    (let ((stderr-file (make-temp-file "mp3-tag-mutagen-stderr-")))
      (unwind-protect
          (let ((status (call-process mp3-tag-python-command
                                      nil (list (current-buffer) stderr-file) nil
                                      "-c" mp3-tag--mutagen-script
                                      (expand-file-name file))))
            (unless (eq status 0)
              (error "Mutagen read failed for %s: %s"
                     file
                     (string-trim
                      (with-temp-buffer
                        (insert-file-contents stderr-file)
                        (buffer-string)))))
            (buffer-string))
        (delete-file stderr-file)))))

(defun mp3-tag--write-tags-with-mutagen (file tags)
  "Write TAGS to FILE using Python and Mutagen."
  (unless mp3-tag-python-command
    (error "Executable for python3 not found"))
  (let ((payload
         (json-encode
          (mapcar (lambda (pair)
                    (cons (symbol-name (car pair)) (cdr pair)))
                  tags))))
    (with-temp-buffer
      (let ((stderr-file (make-temp-file "mp3-tag-mutagen-stderr-")))
        (unwind-protect
            (let ((status (call-process mp3-tag-python-command
                                        nil (list (current-buffer) stderr-file) nil
                                        "-c" mp3-tag--mutagen-write-script
                                        (expand-file-name file)
                                        payload)))
              (unless (eq status 0)
                (error "Mutagen write failed for %s: %s"
                       file
                       (string-trim
                        (with-temp-buffer
                          (insert-file-contents stderr-file)
                          (buffer-string))))))
          (delete-file stderr-file))))))

(defun mp3-tag-read-file (file)
  "Read ID3 tags from an MP3 FILE and return them as an alist.

Requires python3 and mutagen to be installed."
  (cons
   (cons 'filename file)
   (json-read-from-string
    (mp3-tag--read-json-with-mutagen file))))

(defun mp3-tag--strip-private-fields (tracks)
  "Remove all fields beginning with id3v2_priv from a list TRACKS."
  (mapcar (lambda (track)
            (cl-remove-if (lambda (pair)
                            (string-prefix-p "PRIV:"
                                             (symbol-name (car pair))))
                          track)) tracks))

(defun mp3-tag-read-files (files)
  "Read and process a list of mp3 FILES.

Private fields are removed.

FILES is a list of file paths to the MP3 files to be read. The function
returns a list of processed MP3 tag data."

  (mp3-tag--strip-private-fields
   (mapcar #'mp3-tag-read-file files)))

(defun mp3-tag--alist-to-table (tracks)
  "Reshape plist of mp3 TRACKS to something digestable by org table.

Extract the union of keys as a header row, then align each track's values
to that header order.


converts from:

    (((title . \"Track 1\") (artist . \"Artist\") (album . \"Live\") (track . \"1\"))
     ((title . \"Track 2\") (artist . \"Artist\") (album . \"Live\") (track . \"2\")))
to:

    ((\"title\" \"artist\" \"album\" \"track\")
     (\"Track 1\" \"Artist\" \"Live\" \"1\")
     (\"Track 2\" \"Artist\" \"Live\" \"2\"))"

  (let* ((id3-headers (delete-dups (mapcar #'car (apply #'append tracks))))
         (rows (mapcar (lambda (track) (mapcar (lambda (header) (if-let* ((cell (assq header track))) (cdr cell) "")) id3-headers)) tracks))
         (sensible-headers (mapcar (lambda (header) (mp3-tag--id3-name-to-common-sense (symbol-name header))) id3-headers)))
    (cons sensible-headers rows)))

(defun mp3-tag--table-to-alist (table)
  "Convert a TABLE of MP3 tag data into an alist.

TABLE is a list where the first element contains headers and the
subsequent elements are rows of tag values. The function transforms the
table from a format like:

from:

    ((\"title\" \"artist\" \"album\" \"track\")
     (\"Track 1\" \"Artist\" \"Live\" \"1\")
     (\"Track 2\" \"Artist\" \"Live\" \"2\"))

to:
    (((title . \"Track 1\") (artist . \"Artist\") (album . \"Live\") (track . \"1\"))
     ((title . \"Track 2\") (artist . \"Artist\") (album . \"Live\") (track . \"2\")))"

  (let* ((headers (car table))
         (headers (mapcar (lambda (header) (intern (mp3-tag--common-sense-name-to-id3 header))) headers))
         (rows (cdr table)))
    (mapcar (lambda (row) (cl-mapcar #'cons headers row)) rows)))

(defun mp3-tag--table-to-orgtbl (table)
  "Converts an mp3-tag TABLE to and Org-mode table."
  (orgtbl-to-orgtbl
   (append '(hline)
           (list (car table) 'hline)
           (cdr table)
           '(hline))
   nil))

(defun mp3-tag-make-buffer ()
  "Make a mp3-tag buffer."
  (generate-new-buffer (format "*mp3-tag: %s" default-directory)))

(defun mp3-tag--edit-files (files)
  "Make a mp3-tag buffer to edit FILES."
  (let ((edit-buffer   (generate-new-buffer (format "*mp3-tag: %s" default-directory))))
    (pop-to-buffer edit-buffer)
    (org-mode)
    (mp3-tag-mode)
    (insert (mp3-tag--table-to-orgtbl
             (mp3-tag--alist-to-table
              (mp3-tag-read-files files))))))

(defun mp3-tag--get-marked-dired-files ()
  "Retrieve a list of marked MP3 files from the current Dired buffer.
Returns a list of filenames that match the `.mp3` extension."
  (dired-get-marked-files nil 'marked
                          (lambda (filename)
                            (string-match-p "\\.mp3\\'" filename))))

(defun mp3-tag-edit ()
  "Open an editable tag buffer for MP3 files in the current context."
  (interactive)
  (mp3-tag--edit-files
   (or (mp3-tag--get-marked-dired-files)
       (mapcar #'file-name-nondirectory
               (directory-files default-directory t "\\.mp3$" nil)))))

(defun mp3-tag-update ()
  "Update MP3 tags based on the current org table.

This function converts the org table to a Lisp structure, processes each track,
and writes the corresponding tags using Mutagen."
  (interactive)
  (when-let* ((table (mp3-tag-org-table-to-lisp))
              (table (mp3-tag--table-to-alist table)))
    (dolist (track table)
      (let* ((filename (cdr (assq 'filename track)))
             (track (assoc-delete-all 'filename track)))
        (mp3-tag--write-tags-with-mutagen filename track))))
  (kill-buffer))

(defun mp3-tag-org-table-to-lisp ()
  "Return org table at point as a list of lists, stripped of text properties."
  (mapcar (lambda (row)
            (when (listp row)
              (mapcar #'substring-no-properties row)))
          (remove 'hline (org-table-to-lisp))))

(defvar mp3-tag-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'mp3-tag-update) map)
  "Keys used in `mp3-tag-mode' buffers.")

(define-minor-mode mp3-tag-mode
  "Install keybindings for a mp3-tag edit buffer."
  :init-value nil
  :lighter " mp3-tag"
  :keymap mp3-tag-mode-map)

(provide 'mp3-tag)
;;; mp3-tag.el ends here
;; LocalWords:

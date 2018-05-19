;; Experimenting with a different DB schema and functions

;;;; Functions

(defun org-rifle-indexer-index-file (file &optional kill-file-buffer)
  "Index FILE, or current buffer if nil.
If KILL-FILE-BUFFER is non-nil, close buffer after indexing."
  (interactive (list (buffer-file-name)))
  ;; FIXME: First, delete existing indexed entries for this file

  (with-current-buffer (or (find-buffer-visiting file)
                           (progn
                             (setq kill-file-buffer t)
                             (find-file-noselect file)))
    (org-with-wide-buffer
     (let* ((heading-positions (progn
                                 ;; Gather a list of heading positions, going through the file once.  This should
                                 ;; reduce the number of regexp searches needed.
                                 (goto-char (point-min))
                                 (when (org-before-first-heading-p)
                                   (outline-next-heading))
                                 (cl-loop until (eobp)
                                          collect (point)
                                          do (outline-next-heading))))
            (current-heading-pos (pop heading-positions))
            (next-heading-pos (pop heading-positions)))
       ;; Go to each heading and insert it into the database.
       (cl-loop do (goto-char current-heading-pos)
                for current-heading = (buffer-substring-no-properties (point) (line-end-position))
                for current-entry = (buffer-substring-no-properties (line-end-position) (or next-heading-pos
                                                                                            (point-max)))
                do (org-rifle-indexer-insert file current-heading-pos current-heading current-entry)
                do (setq current-heading-pos next-heading-pos
                         next-heading-pos (pop heading-positions))
                while current-heading-pos
                )))
    (when kill-file-buffer
      (kill-buffer))))

(defun org-rifle-indexer-index-file-with-tags (file &optional kill-file-buffer)
  "Index FILE, or current buffer if nil.
If KILL-FILE-BUFFER is non-nil, close buffer after indexing."
  (interactive (list (buffer-file-name)))
  ;; FIXME: First, delete existing indexed entries for this file

  (with-current-buffer (or (find-buffer-visiting file)
                           (progn
                             (setq kill-file-buffer t)
                             (find-file-noselect file)))
    (let ((org-trust-scanner-tags t)
          (org-use-tag-inheritance t))
      (org-with-wide-buffer
       (org-scan-tags (lambda ()
                        (let* ((position (point))
                               (heading (buffer-substring-no-properties (point) (line-end-position)))
                               (entry (buffer-substring-no-properties (line-end-position) (or (outline-next-heading)
                                                                                              (point-max))))
                               (tags (org-get-tags-at)))
                          (org-rifle-indexer-insert-with-tags file position heading tags entry)))
                      t nil)))
    (when kill-file-buffer
      (kill-buffer))))

(defun org-rifle-indexer-insert (file position heading entry)
  "Insert into current `db' connection."
  (emacsql db [:insert-into entries :values [$s1 $s2 $s3 $s4]]
           file position heading entry))

(defun org-rifle-indexer-insert-with-tags (file position heading tags entry)
  "Insert into current `db' connection."
  (emacsql db [:insert-into entries :values [$s1 $s2 $s3 $s4 $s5]]
           file position heading tags entry))

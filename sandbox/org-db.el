;;; org-db.el --- An org database

;;; Commentary:
;;

(require 'cl-lib)

(require 'emacsql-sqlite)
(require 's)

(defcustom org-db-root "~/.cache/emacs/"
  "Root directory for db files."
  :group 'org-db)

(defcustom org-db-index-content t
  "Controls if the content of headlines is saved.")

;; (unless (file-directory-p org-db-root)
;;   (make-directory org-db-root t))

(defvar org-db (emacsql-sqlite (expand-file-name "org-db.sqlite" org-db-root)))

(cl-defun org-db-create-db (&optional (fts-type 'fts5))
  (emacsql org-db [:PRAGMA (= foreign_keys 1)])


  (emacsql org-db [:create-table :if :not :exists
                                 files
                                 ([(rowid integer :primary-key)
                                   (filename :unique)
                                   md5])])

  (emacsql org-db [:create-table :if :not :exists tags
                                 ([(rowid integer :primary-key)
                                   (tag :unique)])])

  (emacsql org-db [:create-table :if :not :exists properties
                                 ([(rowid integer :primary-key)
                                   (property :unique)])])

  (emacsql org-db [:create-table :if :not :exists keywords
                                 ([(rowid integer :primary-key)
                                   (keyword :unique)])])

  (emacsql org-db [:create-table :if :not :exists
                                 headlines
                                 ([(rowid integer :primary-key)
                                   filename-id
                                   title
                                   level
                                   todo-keyword
                                   todo-type
                                   archivedp
                                   commentedp
                                   footnote-section-p
                                   begin]
                                  (:foreign-key [filename-id] :references files [rowid] :on-delete :cascade))])

  ;; no cascade delete ;(

  (emacsql org-db (vector :create :virtual :table :if :not :exists 'headline-content :using fts-type
                          '([headline-id content])))


  (emacsql org-db [:create-table :if :not :exists
                                 headline-tags
                                 ([(rowid integer :primary-key)
                                   headline-id
                                   tag-id]
                                  (:foreign-key [headline-id] :references headlines [rowid] :on-delete :cascade)
                                  (:foreign-key [tag-id] :references tags [rowid] :on-delete :cascade))])


  (emacsql org-db [:create-table :if :not :exists
                                 headline-properties
                                 ([(rowid integer :primary-key)
                                   headline-id
                                   property-id
                                   value]
                                  (:foreign-key [headline-id] :references headlines [rowid] :on-delete :cascade)
                                  (:foreign-key [property-id] :references properties [rowid] :on-delete :cascade))])


  (emacsql org-db [:create-table :if :not :exists
                                 file-keywords
                                 ([(rowid integer :primary-key)
                                   filename-id
                                   keyword-id
                                   value]
                                  (:foreign-key [filename-id] :references files [rowid] :on-delete :cascade)
                                  (:foreign-key [keyword-id] :references keywords [rowid] :on-delete :cascade))])


  (emacsql org-db [:create-table :if :not :exists
                                 links
                                 ([(rowid integer :primary-key)
                                   filename-id
                                   type
                                   path
                                   raw-link
                                   description
                                   search-option
                                   begin]
                                  (:foreign-key [filename-id] :references files [rowid] :on-delete :cascade))])
  )


(defun org-db-link-update ()
  "Update the database with links in the current buffer."
  (interactive)
  (message "Updating links in %s" (buffer-file-name))
  (setq filename-id
	(or (caar (emacsql org-db [:select rowid :from files
                                           :where (= filename $s1)]
			   (buffer-file-name)))
	    (emacsql org-db [:insert :into files :values [nil $s1 $s2]]
		     (buffer-file-name)
		     (md5 (current-buffer)))
	    (caar (emacsql org-db [:select (funcall last-insert-rowid)]))))

  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-any-link-re nil t)
      (let ((link (save-excursion (goto-char (match-beginning 0)) (org-element-context))))
	(emacsql org-db [:insert :into links :values $v1]
		 (list (vector
			nil
			filename-id
			(org-element-property :type link)
			(org-element-property :path link)
			(org-element-property :raw-link link)
			(if (org-element-property :contents-begin link)
			    (buffer-substring-no-properties
			     (org-element-property :contents-begin link)
			     (org-element-property :contents-end link))
			  "")
			(org-element-property :search-option link)
			(org-element-property :begin link))))))))


(defun org-db-keyword-update ()
  "Update the database with keyword-values for the current buffer."
  (message "Updating keywords in %s" (buffer-file-name))
  (save-excursion
    (goto-char (point-min))
    (save-restriction
      (widen)
      (let ((keywords '())
	    filename-id keyword-id
	    key val)
	(while (re-search-forward "^#\\+\\([^ ]*\\): +\\(.*\\)")
	  (setq key (match-string-no-properties 1)
		val (match-string-no-properties 2))
	  (add-to-list 'keywords (cons (upcase key) val)))

	(setq filename-id
	      (or (caar (emacsql org-db [:select rowid :from files
                                                 :where (= filename $s1)]
				 (buffer-file-name)))
		  (emacsql org-db [:insert :into files :values [nil $s1 $s2]]
			   (buffer-file-name)
			   (md5 (current-buffer)))
		  (caar (emacsql org-db [:select (funcall last-insert-rowid)]))))

	(cl-loop for (keyword . value) in keywords
                 do
                 (message "keyword: %s %s" keyword value)
                 (setq keyword-id
                       (or (caar (emacsql org-db [:select rowid :from keywords
                                                          :where (= keyword $s1)]
                                          keyword))
                           (emacsql org-db [:insert :into keywords :values [nil $s1]]
                                    keyword)
                           (caar (emacsql org-db [:select (funcall last-insert-rowid)]))))
                 (emacsql org-db [:insert :into file-keywords :values [nil $s1 $s2 $s3]]
                          filename-id keyword-id value))))))


(defun org-db-update (&optional force)
  "Update the database with the current buffer if needed."
  (interactive "P")
  (when (or force
	    (not (string= (md5 (current-buffer))
			  (caar (emacsql org-db [:select md5 :from files :where (= filename $s1)]
					 (buffer-file-name))))))
    (message "Updating database in %s" (buffer-file-name))

    (emacsql org-db [:begin-transaction])

    ;; no cascade delete in virtual tables, so we manually do it.

    ;; FIXME: This seems extremely slow.  The time for indexing my main.org file went from 27
    ;; seconds to 6 seconds when I commented out this part.
    (let ((filename-id (caar (emacsql org-db [:select rowid :from files :where (= filename $s1)]
				      (buffer-file-name))))
	  headline-ids)
      (when filename-id
	(cl-loop with headline-ids = (mapcar 'car (emacsql org-db [:select [rowid] :from headlines :where (= filename-id $s1)]
                                                           filename-id))
                 for hl-id in headline-ids
                 do (emacsql org-db [:delete :from headline-content
                                             :where (= headline-content:headline-id $s1)]
                             hl-id)))
      ;; now delete the file, which should cascade delete the rest
      (emacsql org-db [:delete :from files :where (= filename $s1)]
               (buffer-file-name)))

    ;; now add each headline and link.
    (org-with-wide-buffer
     ;; (org-db-link-update)
     ;; (org-db-keyword-update)
     (org-map-entries #'org-db-add-headline))

    (emacsql org-db [:commit])

    (message "done updating %s" (buffer-file-name))))

(defun org-db-update (&optional force)
  "Update the database with the current buffer if needed."
  (interactive "P")
  (when (or force
	    (not (string= (md5 (current-buffer))
			  (caar (emacsql org-db [:select md5 :from files :where (= filename $s1)]
					 (buffer-file-name))))))
    (message "Updating database in %s" (buffer-file-name))

    (emacsql org-db [:begin-transaction])

    ;; no cascade delete in virtual tables, so we manually do it.

    ;; FIXME: This seems extremely slow.  The time for indexing my main.org file went from 27
    ;; seconds to 6 seconds when I commented out this part.

    ;; Trying a subquery:
    (let ((filename-id (caar (emacsql org-db [:select rowid :from files :where (= filename $s1)]
				      (buffer-file-name))))
	  headline-ids)
      (when filename-id
	(emacsql org-db [:delete :from headline-content
                                 :where (in headline-content:headline-id
                                            [:select [rowid] :from headlines :where (= filename-id $s1)])]
                 filename-id))
      ;; now delete the file, which should cascade delete the rest
      (emacsql org-db [:delete :from files :where (= filename $s1)]
               (buffer-file-name))

      ;; Re-create file
      (setq filename-id (progn
                          (emacsql org-db [:insert :into files :values [nil $s1 $s2]]
                                   (buffer-file-name)
                                   (md5 (current-buffer)))
                          (caar (emacsql org-db [:select (funcall last-insert-rowid)]))))

      ;; now add each headline and link.
      (org-with-wide-buffer
       ;; (org-db-link-update)
       ;; (org-db-keyword-update)
       (org-map-entries (apply-partially #'org-db-add-headline filename-id))))

    (emacsql org-db [:commit])

    (message "done updating %s" (buffer-file-name))))

(defun org-db-add-headline (&optional filename-id)
  "add a headline."
  (interactive)
  (let ((hl (org-element-context))
	headline-id tag-id property-id)

    ;; filename
    (unless filename-id
      (setq filename-id (or (caar (emacsql org-db [:select rowid :from files
                                                           :where (= filename $s1)]
                                           (buffer-file-name)))
                            (emacsql org-db [:insert :into files :values [nil $s1 $s2]]
                                     (buffer-file-name)
                                     (md5 (current-buffer)))
                            (caar (emacsql org-db [:select (funcall last-insert-rowid)])))))
    ;; headline
    (emacsql org-db [:insert :into headlines :values [nil $s1 $s2 $s3 $s4 $s5 $s6 $s7 $s8 $s9]]
	     filename-id
	     (org-element-property :title hl)
	     (org-element-property :level hl)
	     (org-element-property :todo-keyword hl)
	     (org-element-property :todo-type hl)
	     (org-element-property :archivedp hl)
	     (org-element-property :commentedp hl)
	     (org-element-property :footnote-section-p hl)
	     (org-element-property :begin hl))
    (setq headline-id (caar (emacsql org-db [:select (funcall last-insert-rowid)])))

    ;; content for searching
    (when (and org-db-index-content
               (org-element-property :contents-begin hl))
      (emacsql org-db [:insert :into headline-content :values [$s1 $s2]]
               headline-id
               (s-trim (buffer-substring-no-properties (org-element-property :contents-begin hl)
                                                       (or (org-with-wide-buffer
                                                            (outline-next-heading)
                                                            (point))
                                                           (point-max))))))

    ;; tags
    ;; (cl-loop for tag in (mapcar 'org-no-properties (org-get-tags-at))
    ;;          do (setq tag-id (or (caar (emacsql org-db [:select rowid :from tags :where (= tag $s1)] tag))
    ;;                              (emacsql org-db [:insert :into tags :values [nil $s1]] tag)
    ;;                              (caar (emacsql org-db [:select (funcall last-insert-rowid)]))))
    ;;
    ;;          ;; Now add entries to headline_tags
    ;;          (emacsql org-db [:insert :into headline-tags :values [nil $s1 $s2]]
    ;;                   headline-id tag-id))
    ;; ;; properties
    ;; (cl-loop for (property . value) in (org-entry-properties)
    ;;          do (setq property-id (or (caar (emacsql org-db [:select rowid :from properties :where (= property $s1)] property))
    ;;                                   (emacsql org-db [:insert :into properties :values [nil $s1]] property)
    ;;                                   (caar (emacsql org-db [:select (funcall last-insert-rowid)]))))
    ;;
    ;;          ;; and the values
    ;;          (emacsql org-db [:insert :into headline-properties :values [nil $s1 $s2 $s3]]
    ;;                   headline-id property-id (org-no-properties value)))
    ))

(defun org-db-refresh ()
  "Update all the files in the database."
  (interactive)
  (let* ((files (emacsql org-db [:select [filename] :from files]))
	 (N (length files))
	 (enable-local-variables nil)
	 (org-mode-hook '())
	 buf)
    (cl-loop for (fname) in files for i from 0 to N
             do
             (if (and fname (file-exists-p fname))
                 (progn
                   (message "Refreshing %s of %s" i N)
                   (setq buf (find-file-noselect fname))
                   (with-current-buffer buf
                     (condition-case nil
                         (org-db-update t)
                       (error (message "Error updating %s" fname))))
                   (kill-buffer buf))
               ;; no fname exists. We need to delete it.
               (message "removing %s from database" fname)
               (let ((filename-id (caar (emacsql org-db [:select rowid :from files
                                                                 :where (= filename $s1)]
                                                 fname)))
                     headline-ids)
                 (when filename-id
                   (setq headline-ids
                         (mapcar 'car (emacsql org-db [:select [rowid] :from headlines
                                                               :where (= filename-id $s1)]
                                               filename-id)))
                   (cl-loop for hl-id in headline-ids do
                            (emacsql org-db [:delete :from headline-content
                                                     :where (= headline-content:headline-id $s1)]
                                     hl-id)))
                 ;; now delete the file, which should cascade delete the rest
                 (emacsql org-db [:delete :from files :where (= filename $s1)] fname))))))


(defun org-db-index (path  &optional recursive)
  "Index all the org-files in PATH."
  (interactive (list (read-directory-name "Path: ")
		     current-prefix-arg))
  (let ((enable-local-variables nil))
    (cl-loop for f in (f-files path (lambda (f) (and (or (f-ext? f "org")
                                                         (f-ext? f "org_archive"))
                                                     (not (string-match "\\.dropbox" f))))
                               recursive)
             do
             (let ((buf (find-file-noselect f)))
               (kill-buffer buf)))))

;; (add-hook
;;  'org-mode-hook
;;  (lambda ()
;;    ;; update on opening, in case it changed externally
;;    (org-db-update)
;;    (add-hook 'after-save-hook
;; 	     ;; update on saving.
;; 	     (lambda ()
;; 	       (org-db-update))
;; 	     nil t)))


(defun org-db-clean-db ()
  "Check all files in the db exist and delete those that don't."
  (cl-loop for (fname) in (emacsql org-db [:select :distinct [filename] :from links])
           do
           (unless (file-exists-p fname)
             ;; the headline content does not cascade delete, so we do it manually
             ;; here.
             (let ((filename-id (caar (emacsql org-db [:select rowid :from files
                                                               :where (= filename $s1)]
                                               fname)))
                   headline-ids)
               (when filename-id
                 (setq headline-ids
                       (mapcar 'car (emacsql org-db [:select [rowid] :from headlines
                                                             :where (= filename-id $s1)]
                                             filename-id)))
                 (cl-loop for hl-id in headline-ids do
                          (emacsql org-db [:delete :from headline-content
                                                   :where (= headline-content:headline-id $s1)]
                                   hl-id)))
               ;; now delete the file, which should cascade delete the rest
               (emacsql org-db [:delete :from files :where (= filename $s1)] fname)))))


(defun org-db-link-query (query)
  "Search for and open links using QUERY.
QUERY is an unquoted form that is used in a where clause of the
query. Here are some examples:
 (= type \"cite\")  find all links of type cite
 (glob path \"*kitchin*\") find all links whose path matches *kitchin*
 (and (= type \"cite\") (glob path \"mathias*\"))

Note: this function only works interactively. I don't know how to
enter the query as an unevaluated sexp in code.
"
  (interactive "xQuery: ")
  (let* ((results (eval `(emacsql org-db [:select [filename raw-link description begin]
                                                  :from links :where ,query])))
	 (candidates (cl-loop for result in results
                              collect
                              (list (format "%s | %s" (nth 1 result) (nth 0 result))
                                    (nth 0 result)
                                    (nth 3 result))))
	 (choice (completing-read "Open: " candidates))
	 (data (assoc choice candidates))
	 (fname (nth 1 data))
	 (pos (nth 2 data)))
    (find-file fname)
    (goto-char pos)))

(defun org-db-quit ()
  "Quit the database."
  (interactive)
  (emacsql-close org-db))

;; * End
(provide 'org-db)

;;; org-db.el ends here

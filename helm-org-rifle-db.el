(require 'dash)
(require 'helm)

(require 'org-rifle-lib)

(defun helm-org-rifle-db-search (words)
  "Show entries in `org-db' matching WORDS in a Helm buffer."
  ;; This function searches the entire database.  We probably also want one that only searches specific files.
  (let* ((fts-phrase (apply #'vector words))
         (where-clause `(match headline_content:content ,fts-phrase) )
         (query (vector :select :distinct [files:filename headlines:title headlines:level headlines:begin headline_content:content]
                        :from 'headlines
                        :inner-join 'files :on '(= headlines:filename-id files:rowid)
                        :inner-join 'headline_content :on '(= headlines:rowid headline_content:headline_id)
                        :where where-clause
                        :order-by 'rank))
         (grouped-results (-group-by #'car (emacsql org-db query)))
         (sources (--map (-let* (((file . entries) it))
                           (helm-build-sync-source file
                             :after-init-hook helm-org-rifle-after-init-hook
                             :candidates (--map (-let* (((file heading level position content) it)
                                                        (stars (make-string level ?*))
                                                        ;; FIXME: Use non-helm-prefixed fn
                                                        (entry (helm-org-rifle-fontify-like-in-org-mode
                                                                (concat stars " " heading "\n" content))))
                                                  (cons entry position))
                                                entries)
                             :candidate-transformer helm-org-rifle-transformer
                             :match 'identity
                             :multiline helm-org-rifle-multiline
                             :volatile t
                             :action (helm-make-actions
                                      "Show entry" 'helm-org-rifle--show-candidates
                                      "Show entry in indirect buffer" 'helm-org-rifle-show-entry-in-indirect-buffer
                                      "Show entry in real buffer" 'helm-org-rifle-show-entry-in-real-buffer
                                      "Clock in" 'helm-org-rifle--clock-in)
                             :keymap helm-org-rifle-map))
                         grouped-results) ))
    (helm sources)))

;; (helm-org-rifle-db-search (list "apartment" "neighbor"))

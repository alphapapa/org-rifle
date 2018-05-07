;;; org-rifle-backend-file.el --- Rifle through your Org files

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-rifle

;;; Commentary:

;; This file includes the standard, file-based backend.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Require

(require 'cl-lib)
(require 'dash)
(require 'org)
(require 's)

;;;;; The meat

(defun org-rifle--get-candidates-in-buffer (buffer input)
  "Return candidates in BUFFER for INPUT.

INPUT is a string.  Candidates are returned in this
format: (STRING . POSITION)

STRING begins with a fontified Org heading and optionally
includes further matching parts separated by newlines.

POSITION is the position in BUFFER where the candidate heading
begins.

This is how the sausage is made."
  (with-current-buffer buffer
    ;; Run this in the buffer so we can get its todo-keywords (i.e. `org-todo-keywords-1')
    (-let* ((buffer-name (buffer-name buffer))
            ((includes excludes include-tags exclude-tags todo-keywords) (org-rifle--parse-input input))
            (excludes-re (when excludes
                           ;; NOTE: Excludes only match against whole words.  This probably makes sense.
                           ;; TODO: Might be worth mentioning in docs.
                           (rx-to-string `(seq (or ,@excludes)) t)))
            (include-tags (--map (s-wrap it ":") include-tags))  ; Wrap include-tags in ":" for the regexp
            (positive-re (rx-to-string `(seq (or ,@(append includes include-tags todo-keywords)))))
            ;; NOTE: We leave todo-keywords out of the required-positive-re-list,
            ;; because that is used to verify that all positive tokens
            ;; are matched in an entry, and we want todo-keywords to
            ;; match OR-wise.
            (required-positive-re-list (mapcar #'regexp-quote (append includes include-tags)))
            (context-re (rx-to-string `(seq (repeat 0 ,org-rifle-context-characters not-newline)
                                            (or ,@(append includes include-tags todo-keywords))
                                            (repeat 0 ,org-rifle-context-characters not-newline))
                                      t))
            ;; TODO: Turn off case folding if tokens contains mixed case
            (case-fold-search t)
            (results nil))
      (save-excursion
        ;; Go to first heading
        (goto-char (point-min))
        (when (org-before-first-heading-p)
          (outline-next-heading))
        ;; Search for matching nodes
        (cl-loop while (re-search-forward positive-re nil t)
                 for result = (save-excursion
                                (org-rifle--test-entry))
                 when result
                 collect result
                 do (outline-next-heading))))))

(defun org-rifle--test-entry ()
  "Return list of entry data if entry at point matches.
This is to be called from `org-rifle--get-candidates-in-buffer',
because it uses variables in its outer scope."
  (-let* ((node-beg (org-entry-beginning-position))
          (node-end (org-entry-end-position))
          ((level reduced-level todo-keyword priority-char heading tags priority) (org-heading-components))
          (path (when org-rifle-show-path
                  (org-get-outline-path)))
          (priority (when priority-char
                      ;; TODO: Is there a better way to do this?  The
                      ;; s-join leaves an extra space when there's no
                      ;; priority.
                      (format "[#%c]" priority-char)))
          (heading (s-trim (if org-rifle-show-todo-keywords
                               (s-join " " (list todo-keyword priority heading))
                             heading)))
          (matching-positions-in-node nil)
          (matching-lines-in-node nil)
          (matched-words-with-context nil))
    ;; Goto beginning of node
    (goto-char node-beg)

    (unless
        (or  ;; Check to-do keywords and excludes
         (when todo-keywords
           (not (member todo-keyword todo-keywords)))
         (when (and exclude-tags tags)
           (cl-intersection (org-split-string tags ":") exclude-tags
                            :test #'string=))
         ;; Check normal excludes
         (when excludes
           ;; TODO: Maybe match against a heading's inherited tags, if it's not too slow.

           ;; FIXME: Partial excludes seem to put the partially
           ;; negated entry at the end of results.  Not sure why.
           ;; Could it actually be a good feature, though?
           (or (cl-loop for elem in (or path (org-get-outline-path))
                        thereis (string-match-p excludes-re elem))
               ;; FIXME: Doesn't quite match properly with
               ;; special chars, e.g. negating "!scratch"
               ;; properly excludes the "*scratch*" buffer,
               ;; but negating "!*scratch*" doesn't'.
               (string-match excludes-re buffer-name)
               (save-excursion
                 (re-search-forward excludes-re node-end t)))))

      ;; No excludes match; collect entry data
      (let (matching-positions-in-node matching-lines-in-node matched-words-with-context entry)
        (setq matching-positions-in-node
              ;; Get beginning-of-line positions for matching lines in node
              (save-excursion
                (cl-loop
                 while (re-search-forward positive-re node-end t)
                 collect (line-beginning-position) into result
                 do (end-of-line)
                 finally return (sort (delete-dups result) '<))))

        (setq matching-lines-in-node
              ;; Get list of line-strings containing any token
              (cl-loop with string
                       for pos in matching-positions-in-node
                       do (goto-char pos)
                       unless (org-at-heading-p) ; Leave headings out of list of matched lines
                       ;; Get text of each matching line
                       ;; (DISPLAY . REAL) format for Helm
                       collect (cons (buffer-substring-no-properties (line-beginning-position)
                                                                     (line-end-position))
                                     (cons buffer pos))))

        ;; Verify all tokens are contained in each matching node
        (when (cl-loop with targets = (-non-nil (append (list buffer-name
                                                              heading
                                                              tags)
                                                        (when org-rifle-show-path
                                                          path)
                                                        (mapcar 'car matching-lines-in-node)))
                       for re in required-positive-re-list
                       always (cl-loop for target in targets
                                       thereis (s-matches? re target)))

          ;; All tokens match; collect and return data
          (setq matched-words-with-context
                (if org-rifle-show-full-contents
                    ""  ; Don't collect context
                  (or (cl-loop for line in (mapcar 'car matching-lines-in-node)
                               ;; Matching entry text found
                               do (when org-rifle-unlinkify-entry-links
                                    (setq line (org-rifle-replace-links-in-string line)))
                               append (cl-loop with end
                                               for match = (string-match context-re line end)
                                               while match
                                               do (setq end (match-end 0))
                                               and collect (s-trim (match-string-no-properties 0 line))))
                      (when (> org-rifle-always-show-entry-contents-chars 0)
                        ;; No match in entry text; add desired entry text
                        (list (s-truncate org-rifle-always-show-entry-contents-chars
                                          (org-rifle--get-entry-text buffer node-beg)))))))

          (setq heading
                (if path
                    (if org-rifle-fontify-headings
                        (concat (org-format-outline-path
                                 ;; Replace links in path elements with plain text, otherwise
                                 ;; they will be truncated by `org-format-outline-path' and only
                                 ;; show part of the URL.  FIXME: Use org-link-display-format function
                                 (-map 'org-rifle-replace-links-in-string (append path (list heading))))
                                (if tags
                                    (concat " " (org-rifle-fontify-like-in-org-mode tags))
                                  ""))
                      ;; Not fontifying
                      (s-join "/" (append path (list heading) tags)))
                  ;; No path or not showing path
                  (if org-rifle-fontify-headings
                      (org-rifle-fontify-like-in-org-mode
                       (s-join " " (list (s-repeat level "*") heading (concat tags " "))))
                    ;; Not fontifying
                    (s-join " " (list (s-repeat level "*") heading tags)))))

          (setq entry
                (if org-rifle-show-full-contents
                    (s-join org-rifle-heading-contents-separator
                            (list heading
                                  (buffer-substring (save-excursion
                                                      (goto-char node-beg)
                                                      (org-end-of-meta-data)
                                                      (point))
                                                    node-end)))
                  ;; Show context strings
                  (s-join org-rifle-heading-contents-separator
                          (list heading
                                (s-join org-rifle-ellipsis-string
                                        matched-words-with-context)))))
          ;; Return list in format: text-for-display node-beg
          (cons entry (cons buffer node-beg)))))))

(defun org-rifle--parse-input (input)
  "Return list of token types in INPUT string.
Returns (INCLUDES EXCLUDES INCLUDE-TAGS EXCLUDE-TAGS TODO-KEYWORDS)."
  (let ((tags-regexp (rx bos (optional "!") (1+ ":" (1+ (char alnum "_"))) ":" eos))
        includes excludes include-tags exclude-tags todo-keywords)
    (dolist (token (split-string input " " t))
      (pcase token
        ;; Tags
        ((pred (string-match tags-regexp))
         (if (string-match "^!" token)
             ;; Exclude tag
             (setq exclude-tags (append exclude-tags (org-split-string (cl-subseq token 1) ":")))
           ;; Match tag
           (setq include-tags (append include-tags (org-split-string token ":")))))
        ;; Negation
        ((pred (string-match "^!")
               ;; Ignore bare "!"
               (guard (> (length token) 1)))
         (push (cl-subseq token 1) excludes))
        ;; TODO keyword
        ((guard (member token org-todo-keywords-1))
         (push token todo-keywords))
        ;; Positive terms
        (otherwise (push token includes))))
    (list includes excludes include-tags exclude-tags todo-keywords)))

(provide 'org-rifle-backend-file)

;;; org-rifle-backend-file.el ends here

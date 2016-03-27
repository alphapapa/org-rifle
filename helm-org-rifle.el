
;;; Code:

;; BUG: The major bug now is that immediately after typing, results in
;; the Helm buffer are duplicated strangely.  But after inserting a
;; trailing space, they disappear.  And after removing the trailing
;; space, the duplicates do not return.

(defcustom helm-org-rifle-context-words 10
  "The number of words around matched words to include results.")

(defcustom helm-org-rifle-fontify-headings t
  "Fontify Org headings.

For large result sets, this may be slow, although it doesn't seem
to be the bottleneck.")

(defcustom helm-org-rifle-show-path nil
  "Show the whole heading path instead of just the node's heading.")

(defun helm-org-rifle ()
  "Rifle!"
  (interactive)
  (helm :sources (helm-org-rifle-get-sources))

  (defun helm-org-rifle-get-sources ()
    "Return list of sources configured for helm-org-rifle,
one for each open Org buffer."
    (setq helm-org-rifle-result-count 0)
    (cl-loop for buffer in (org-buffer-list nil t)
             for source = (helm-build-sync-source (buffer-name buffer)
                            :candidates (lambda ()
                                          (when (s-present? helm-pattern)
                                            (helm-org-rifle-get-candidates-in-buffer (helm-attr 'buffer) helm-pattern)))
                            :match 'identity
                            :delayed 0.5
                            :multiline t
                            :volatile t
                            :action (lambda (candidate)
                                      (setq helm-org-rifle-result-count 0)
                                      (switch-to-buffer (helm-attr 'buffer))
                                      (goto-char (car candidate))
                                      (org-reveal))
                            :after-init-hook (lambda ()
                                               (with-current-buffer helm-buffer
                                                 (face-remap-set-base 'helm-selection
                                                                      :underline 'unspecified
                                                                      :weight 'unspecified
                                                                      :background "black"))))
             do (helm-attrset 'buffer buffer source)
             collect source))

  (defun helm-org-rifle-get-candidates-in-buffer (buffer input)
    "Return candidates in BUFFER for INPUT string.

Candidates are returned in this format: (STRING . POSITION)

STRING begins with a fontified Org heading and optionally includes further matching parts separated by newlines.
POSITION is the position in BUFFER where the candidate heading begins."
    (let* ((input (split-string (s-collapse-whitespace (s-trim input)) " " t))
           (match-all-tokens-re (when input
                                  (mapconcat (lambda (m)
                                               (concat "\\(" (regexp-quote m) "\\)"))
                                             input
                                             "\\|")))
           ;; TODO: Turn off case folding if input contains mixed case
           (case-fold-search t)
           results)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (while (and (< helm-org-rifle-result-count helm-org-rifle-max-results)
                      (re-search-forward match-all-tokens-re nil t))
            ;; Get matching lines in node
            (let* ((node-beg (save-excursion
                               (save-match-data
                                 (outline-previous-heading))))
                   (components (org-heading-components))
                   (path (org-get-outline-path))

                   ;; TODO: Some of these can be moved down to after the match is confirmed
                   ;; Heading text
                   ;; MAYBE: Optionally include TODO keyword and/or tags


                   ;; Note: org-fontify-like-in-org-mode uses temporary buffers that load
                   ;; org-mode and therefore org-mode-hook.  This could be a performance
                   ;; issue.

                   (heading (nth 4 components))
                   (node-end (save-match-data  ; This is confusing; should these be reversed here?  Does it matter?
                               (save-excursion
                                 (outline-next-heading)
                                 (point))))
                   matching-positions-in-node
                   matching-lines-in-node
                   matched-words-with-context)

              ;; Get list of beginning-of-line positions for
              ;; lines in node that match any token
              (setq matching-positions-in-node
                    (cl-loop for token in input
                             append (cl-loop initially (goto-char node-beg)
                                             while (search-forward token node-end t)
                                             collect (line-beginning-position)
                                             do (end-of-line))
                             into result
                             finally return (sort (delete-dups result) '<)))

              ;; Get list of line-strings containing any token
              (setq matching-lines-in-node
                    (cl-loop for pos in matching-positions-in-node
                             do (goto-char pos)
                             ;; Get text of each matching line
                             for string = (buffer-substring-no-properties (line-beginning-position)
                                                                          (line-end-position))
                             unless (org-at-heading-p) ; Leave headings out of list of matched lines
                             ;; (DISPLAY . REAL) format for Helm
                             collect `(,string . (,buffer ,pos))))

              ;; Verify all tokens are contained in each matching node
              (when (cl-loop for token in input
                             ;; Include the heading in the check, even
                             ;; though it's not included in the list of
                             ;; matching lines
                             always (cl-loop for m in (append (list heading) (map 'list 'car matching-lines-in-node))
                                             thereis (s-contains? token m t)))
                ;; Node matches all tokens

                ;; Reduce matching lines to matched word with context
                (setq matched-words-with-context
                      (cl-loop for line in (map 'list 'car matching-lines-in-node)
                               append (cl-loop for token in input
                                               for re = (rx-to-string
                                                         `(and (repeat 0 ,helm-org-rifle-context-words
                                                                       (and (1+ (not space))
                                                                            (or (1+ space)
                                                                                word-boundary)))
                                                               (group (eval token))
                                                               (repeat 0 ,helm-org-rifle-context-words
                                                                       (and (or word-boundary
                                                                                (1+ space))
                                                                            (1+ (not space))))))
                                               for m = (string-match re line end)
                                               for end = (match-end 1)
                                               when m
                                               collect (match-string-no-properties 0 line))))

                ;; Returning list in format: (string-joining-heading-and-lines-by-newlines node-beg)
                (push (list (s-join "\n" (list (if (and helm-org-rifle-show-path
                                                        path)
                                                   (if helm-org-rifle-fontify-headings
                                                       (org-format-outline-path (append path (list heading)))
                                                     (s-join "/" (append path (list heading))))
                                                 (if helm-org-rifle-fontify-headings
                                                     (org-fontify-like-in-org-mode (s-join " " (list
                                                                                                (s-pad-left (nth 0 components) "*" "")
                                                                                                (nth 4 components))))
                                                   (s-join " " (list
                                                                (s-pad-left (nth 0 components) "*" "")
                                                                (nth 4 components)))))
                                               (s-join "..." matched-words-with-context)))
                            node-beg)
                      results))
              ;; Go to end of node
              (goto-char node-end)
              (when (>= (point) (- (point-max) 1)))))))

      ;; Return results
      results))


;;; Test code for eval'ing

;;;; Open Helm session on current org buffers

(let ((helm-candidate-separator " "))
  (helm :sources (helm-org-rifle-get-sources)))

(let ((helm-candidate-separator " ")
      (helm-org-rifle-show-path t))
  (helm :sources (helm-org-rifle-get-sources)))

;;;;; Without fontification

(let ((helm-candidate-separator " ")
      (helm-org-rifle-fontify-headings nil))
  (helm :sources (helm-org-rifle-get-sources)))

(let ((helm-candidate-separator " ")
      (helm-org-rifle-show-path t)
      (helm-org-rifle-fontify-headings nil))
  (helm :sources (helm-org-rifle-get-sources)))

;;;; Get list of candidates for "test.org" buffer

(helm-org-rifle-get-candidates-in-buffer (get-file-buffer "test.org") "pomegr blueberry")
(helm-org-rifle-get-candidates-in-buffer (get-file-buffer "test.org") "green")
(helm-org-rifle-get-candidates-in-buffer (get-file-buffer "test.org") "green blue")
(helm-org-rifle-get-candidates-in-buffer (get-file-buffer "test.org") "pomegr")

(helm-org-rifle-get-candidates-in-buffer (get-file-buffer "test.org") "helm food")

(let ((helm-candidate-separator " ")
      (helm-org-rifle-show-path t))
  (helm-org-rifle-get-candidates-in-buffer (get-file-buffer "test.org") "green blue"))

;;;;; Other buffers

(helm-org-rifle-get-candidates-in-buffer (get-file-buffer "main.org") "emacs helm")

;;;; Context-splitting

(let* ((num-context-words 2)
       (needle "needle")
       (haystack "one two three needle four five six")
       (hay (s-split needle haystack))
       (left-hay (s-split-words (car hay)))
       (right-hay (s-split-words (nth 1 hay))))
  (concat "..."
          (s-join " " (subseq left-hay (- num-context-words)))
          " " needle " "
          (s-join " " (subseq right-hay 0 num-context-words))
          "..."))

;; Multiple needles
(let* ( (needles '("needle" "pin"))
        (haystack "one two three \" needle not pin four five six seven eight pin nine ten eleven twelve"))
  (cl-loop for needle in needles
           append (cl-loop for re = (rx-to-string `(and (repeat 1 ,helm-org-rifle-context-words (and (1+ (not space))
                                                                                                     (or (1+ space)
                                                                                                         word-boundary)))
                                                        (group (eval needle))
                                                        (repeat 1 ,helm-org-rifle-context-words (and (or word-boundary
                                                                                                         (1+ space))
                                                                                                     (1+ (not space))))))
                           for m = (string-match re haystack end)
                           for end = (match-end 1)
                           while m
                           collect (concat "..." (match-string-no-properties 0 haystack) "..."))))

;;;; Org headings

;; Build string for fontifying
(components (org-heading-components))
(level (nth 0 components))
(plain-heading (s-join " " (list
                            (s-pad-left level  "*" "")
                            (nth 4 components))))
;; Note: org-fontify-like-in-org-mode uses temporary buffers that load
;; org-mode and therefore org-mode-hook.  This could be a performance
;; issue.
(fontified-heading (org-fontify-like-in-org-mode plain-heading))

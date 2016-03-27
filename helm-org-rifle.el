;;; helm-org-rifle.el --- Rifle through your Org files

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/helm-org-rifle
;; Package-Requires: ((emacs "24.4") (helm "1.0"))
;; Keywords: hypermedia, outlines

;;; Commentary:

;; This is my rifle.  There are many like it, but this one is mine.
;; My rifle is my best friend. It is my life.  I must master it as I
;; must master my life.

;; What does my rifle do?  It searches rapidly through my Org files,
;; quickly bringing me the information I need to defeat the enemy.

;; This package is a continuation of the fantastic
;; org-search-goto/org-search-goto-ml packages, now with Helm
;; support. It searches both headings and contents of entries in Org
;; buffers, and it displays entries that match all search terms,
;; whether the terms appear in the heading, the contents, or both.
;; Matching portions of entries' contents are displayed with
;; surrounding context to make it easy to acquire your target.

;; Entries are fontified by default to match the appearance of an Org
;; buffer, and optionally the entire path can be displayed for each
;; entry, rather than just its own heading.

;;; Credits

;; This package is based on =org-search-goto= (specifically,
;; =org-search-goto-ml=).  Its unofficial-official home is [[https://www.emacswiki.org/emacs/org-search-goto-ml.el][on EmacsWiki]],
;; but I've mirrored it [[https://github.com/alphapapa/org-search-goto][on GitHub]].

;; It's a really great package, and the only thing that could make it
;; better is to make it work with Helm.  To avoid confusion, this package
;; has a completely different name.

;;; Development

;;;; Bugs

;;;;; TODO Initially duplicated results

;; The major bug now is that immediately after typing, results in the
;; Helm buffer are duplicated several times over, which is very strange.
;; But after inserting a trailing space, they disappear.  And after
;; removing the trailing space, the duplicates do not return.  I'm not
;; sure if it's a bug in this package or in Helm.  It might have
;; something to do with the way we're using multiple sources,
;; one-per-buffer.

;;;; Ideas

;;;;; TODO Match limit

;; =org-search-goto= had a match limit.  I removed it to simplify things,
;; but it might still be useful, depending on how big one's org files
;; are.

;;;;; MAYBE Optionally include TODO keyword and/or tags

;; It could be handy to be able to match against the TODO keyword and
;; tags.  Would make it more complicated, but probably not too hard, and
;; maybe worth it.

;;; Code:

(defcustom helm-org-rifle-context-characters 25
  "How many characters around each matched term to display.")

(defcustom helm-org-rifle-fontify-headings t
  "Fontify Org headings.

For large result sets this may be slow, although it doesn't seem
to be a major bottleneck.")

(defcustom helm-org-rifle-show-path nil
  "Show the whole heading path instead of just the entry's heading.")

(defun helm-org-rifle ()
  "This is my rifle. There are many like it, but this one is mine.

My rifle is my best friend. It is my life. I must master it as I
must master my life.

Without me, my rifle is useless. Without my rifle, I am
useless. I must fire my rifle true. I must shoot straighter than
my enemy who is trying to kill me. I must shoot him before he
shoots me. I will...

My rifle and I know that what counts in war is not the rounds we
fire, the noise of our burst, nor the smoke we make. We know that
it is the hits that count. We will hit...

My rifle is human, even as I, because it is my life. Thus, I will
learn it as a brother. I will learn its weaknesses, its strength,
its parts, its accessories, its sights and its barrel. I will
keep my rifle clean and ready, even as I am clean and ready. We
will become part of each other. We will...

Before God, I swear this creed. My rifle and I are the defenders
of my country. We are the masters of our enemy. We are the
saviors of my life.

So be it, until victory is ours and there is no enemy, but
peace!"
  (interactive)
  (let ((helm-candidate-separator " "))
    (helm :sources (helm-org-rifle-get-sources))))

(defun helm-org-rifle-show-in-indirect-buffer (candidate)
  (let ((buffer (helm-attr 'buffer))
        (pos (car candidate))
        (original-buffer (current-buffer)))
    (switch-to-buffer buffer)
    (goto-char pos)
    (org-tree-to-indirect-buffer)
    (unless (equal original-buffer (car (window-prev-buffers)))
      ;; The selected bookmark was in a different buffer.  Put the
      ;; non-indirect buffer at the bottom of the prev-buffers list
      ;; so it won't be selected when the indirect buffer is killed.
      (set-window-prev-buffers nil (append (cdr (window-prev-buffers))
                                           (car (window-prev-buffers)))))))

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
                          :action (helm-make-actions
                                   "Show entry" (lambda (candidate)
                                                  (setq helm-org-rifle-result-count 0)
                                                  (switch-to-buffer (helm-attr 'buffer))
                                                  (goto-char (car candidate))
                                                  (org-show-entry))
                                   "Show entry in indirect buffer" 'helm-org-rifle-show-in-indirect-buffer)
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
  (let* ((input (split-string input " " t))
         (match-all-tokens-re (when input
                                (mapconcat (lambda (m)
                                             (concat "\\(\\_<" (regexp-quote m) "\\_>\\)"))
                                           input
                                           "\\|")))
         ;; TODO: Turn off case folding if input contains mixed case
         (case-fold-search t)
         results)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward match-all-tokens-re nil t)
          ;; Get matching lines in node
          (let* ((node-beg (save-excursion
                             (save-match-data
                               (outline-previous-heading))))
                 (components (org-heading-components))
                 (path (org-get-outline-path))
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
                                           while (search-forward-regexp (concat "\\_<" token "\\_>") node-end t)
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
              (setq matched-words-with-context
                    (cl-loop for line in (map 'list 'car matching-lines-in-node)
                             append (cl-loop for token in input
                                             for re = (rx-to-string `(and (repeat 0 ,helm-org-rifle-context-characters not-newline)
                                                                          (eval token)
                                                                          (repeat 0 ,helm-org-rifle-context-characters not-newline)))
                                             for m = (string-match re line end)

                                             for end = (match-end 1)
                                             when m
                                             collect (match-string-no-properties 0 line))))

              ;; Return list in format: (string-joining-heading-and-lines-by-newlines node-beg)
              (push (list (s-join "\n" (list (if (and helm-org-rifle-show-path
                                                      path)
                                                 (if helm-org-rifle-fontify-headings
                                                     (org-format-outline-path (append path (list heading)))
                                                   (s-join "/" (append path (list heading))))
                                               (if helm-org-rifle-fontify-headings
                                                   (helm-org-rifle-fontify-like-in-org-mode (s-join " " (list
                                                                                                         (s-pad-left (nth 0 components) "*" "")
                                                                                                         (nth 4 components))))
                                                 (s-join " " (list
                                                              (s-pad-left (nth 0 components) "*" "")
                                                              (nth 4 components)))))
                                             (s-join "..." matched-words-with-context)))
                          node-beg)
                    results))
            ;; Go to end of node
            (goto-char node-end)))))
    ;; Return results
    results))

(defun helm-org-rifle-fontify-like-in-org-mode (s &optional odd-levels)
  "Fontify string S like in Org-mode.

`org-fontify-like-in-org-mode' is a very, very slow function
because it creates a new temporary buffer and runs `org-mode' for
every string it fontifies.  This function reuses a single
invisible buffer and only runs `org-mode' when the buffer is
created."
  (let ((buffer (get-buffer " *helm-org-rifle-fontify*")))
    (unless buffer
      (setq buffer (get-buffer-create " *helm-org-rifle-fontify*"))
      (with-current-buffer buffer
        (org-mode)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert s)
      (let ((org-odd-levels-only odd-levels))
        (font-lock-fontify-buffer)
        (buffer-string)))))

;;; Test code for eval'ing

;; These come in handy while coding.

;;;; Open Helm session on current org buffers

;; #+BEGIN_SRC elisp
;; (let ((helm-candidate-separator " "))
;;   (helm :sources (helm-org-rifle-get-sources)))

;; (let ((helm-candidate-separator " ")
;;       (helm-org-rifle-show-path t))
;;   (helm :sources (helm-org-rifle-get-sources)))
;; #+END_SRC

;;;;; Without fontification

;; #+BEGIN_SRC elisp
;; (let ((helm-candidate-separator " ")
;;       (helm-org-rifle-fontify-headings nil))
;;   (helm :sources (helm-org-rifle-get-sources)))

;; (let ((helm-candidate-separator " ")
;;       (helm-org-rifle-show-path t)
;;       (helm-org-rifle-fontify-headings nil))
;;   (helm :sources (helm-org-rifle-get-sources)))
;; #+END_SRC

;;;; Get list of candidates for "test.org" buffer

;; #+BEGIN_SRC elisp
;; (helm-org-rifle-get-candidates-in-buffer (get-file-buffer "test.org") "pomegr blueberry")
;; (helm-org-rifle-get-candidates-in-buffer (get-file-buffer "test.org") "green")
;; (helm-org-rifle-get-candidates-in-buffer (get-file-buffer "test.org") "green blue")
;; (helm-org-rifle-get-candidates-in-buffer (get-file-buffer "test.org") "pomegr")

;; (helm-org-rifle-get-candidates-in-buffer (get-file-buffer "test.org") "helm food")

;; (let ((helm-candidate-separator " ")
;;       (helm-org-rifle-show-path t))
;;   (helm-org-rifle-get-candidates-in-buffer (get-file-buffer "test.org") "green blue"))
;; #+END_SRC

;;;;; Other buffers

;; #+BEGIN_SRC elisp
;; (let ((helm-org-rifle-fontify-headings nil))
;;   (helm-org-rifle-get-candidates-in-buffer (get-buffer "reference.org") "emacs"))

;; (helm-org-rifle-get-candidates-in-buffer (get-buffer "reference.org") "emacs")

;; (helm-org-rifle-get-candidates-in-buffer (get-buffer "main.org") "tires")

;; #+END_SRC

;;;; Context-splitting

;; Prototype code, keeping for future reference.

;; #+BEGIN_SRC elisp
;; (let* ((num-context-words 2)
;;        (needle "needle")
;;        (haystack "one two three needle four five six")
;;        (hay (s-split needle haystack))
;;        (left-hay (s-split-words (car hay)))
;;        (right-hay (s-split-words (nth 1 hay))))
;;   (concat "..."
;;           (s-join " " (subseq left-hay (- num-context-words)))
;;           " " needle " "
;;           (s-join " " (subseq right-hay 0 num-context-words))
;;           "..."))

;; ;; Multiple needles
;; (let* ( (needles '("needle" "pin"))
;;         (haystack "one two three \" needle not pin four five six seven eight pin nine ten eleven twelve"))
;;   (cl-loop for needle in needles
;;            append (cl-loop for re = (rx-to-string `(and (repeat 1 ,helm-org-rifle-context-words (and (1+ (not space))
;;                                                                                                      (or (1+ space)
;;                                                                                                          word-boundary)))
;;                                                         (group (eval needle))
;;                                                         (repeat 1 ,helm-org-rifle-context-words (and (or word-boundary
;;                                                                                                          (1+ space))
;;                                                                                                      (1+ (not space))))))
;;                            for m = (string-match re haystack end)
;;                            for end = (match-end 1)
;;                            while m
;;                            collect (concat "..." (match-string-no-properties 0 haystack) "..."))))
;; #+END_SRC

;;;;; Slow code that splits on word boundaries

;; This code splits on word boundaries, but it's very slow.  Profiling it
;; showed the vast majority of the time was in =string-match=.  I'm
;; guessing the regexp is too complicated or unoptimized.

;; #+BEGIN_SRC elisp
;;   ;; Reduce matching lines to matched word with context
;;   (setq matched-words-with-context
;;         (cl-loop for line in (map 'list 'car matching-lines-in-node)
;;                  append (cl-loop for token in input
;;                                  for re = (rx-to-string
;;                                            `(and (repeat 0 ,helm-org-rifle-context-words
;;                                                          (and (1+ (not space))
;;                                                               (or (1+ space)
;;                                                                   word-boundary)))
;;                                                  (group (eval token))
;;                                                  (repeat 0 ,helm-org-rifle-context-words
;;                                                          (and (or word-boundary
;;                                                                   (1+ space))
;;                                                               (1+ (not space))))))

;;                                  This one line uses about 95% of the runtime of this function
;;                                  for m = (string-match re line end)

;;                                  for end = (match-end 1)
;;                                  when m
;;                                  collect (match-string-no-properties 0 line))))
;; #+END_SRC

;;;;; Faster version that cuts off mid-word

;; This version is much, much faster, but instead of matching on word
;; boundaries, it just matches so-many characters before and after the
;; token.  It's not quite as nice, but the speedup is worth it, and it
;; seems good enough.

;; This is the version currently in-use.

;; #+BEGIN_SRC elisp
;; (setq matched-words-with-context
;;                     (cl-loop for line in (map 'list 'car matching-lines-in-node)
;;                              append (cl-loop for token in input
;;                                              for re = (rx-to-string '(and (repeat 0 25 not-newline)
;;                                                                           (eval token)
;;                                                                           (repeat 0 25 not-newline)))
;;                                              for m = (string-match re line end)

;;                                              for end = (match-end 1)
;;                                              when m
;;                                              collect (match-string-no-properties 0 line))))
;; #+END_SRC

;;;; Org headings

;; #+BEGIN_SRC elisp
;;   ;; Build string for fontifying
;;   (components (org-heading-components))
;;   (level (nth 0 components))
;;   (plain-heading (s-join " " (list
;;                               (s-pad-left level  "*" "")
;;                               (nth 4 components))))
;;   ;; Note: org-fontify-like-in-org-mode uses temporary buffers that load
;;   ;; org-mode and therefore org-mode-hook.  This could be a performance
;;   ;; issue.
;;   (fontified-heading (org-fontify-like-in-org-mode plain-heading))
;; #+END_SRC

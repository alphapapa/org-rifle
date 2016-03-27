;;; helm-org-rifle.el --- Rifle through your Org files

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/helm-org-rifle
;; Package-Requires: ((emacs "24.4") (helm "1.9.3") (s "1.11.0"))
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

;;; Installation

;; Install the Helm and "s" (aka "s.el") packages.  Then require this
;; package in your init file:

;; (require 'helm-org-rifle)

;; Then you can customize the `helm-org-rifle' group if you like.

;;; Usage

;; Run the command `helm-org-rifle', type some words, and all current
;; `org-mode' buffers will be rifled through.  Hit "RET" to show the
;; selected entry.  You can also show the entry in an indirect buffer
;; by selecting that action from the Helm actions list.

;;; Credits:

;; This package is based on =org-search-goto= (specifically,
;; =org-search-goto-ml=).  Its unofficial-official home is
;; [[https://www.emacswiki.org/emacs/org-search-goto-ml.el][on
;; EmacsWiki]], but I've mirrored it
;; [[https://github.com/alphapapa/org-search-goto][on GitHub]].  It's
;; a really great package, and the only thing that could make it
;; better is to make it work with Helm.  To avoid confusion, this
;; package has a completely different name.

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

(require 'helm)
(require 's)

(defgroup helm-org-rifle nil
  "Settings for `helm-org-rifle'."
  :group 'helm
  :link '(url-link "http://github.com/alphapapa/helm-org-rifle"))

(defcustom helm-org-rifle-context-characters 25
  "How many characters around each matched term to display."
  :group 'helm-org-rifle :type 'integer)

(defcustom helm-org-rifle-fontify-headings t
  "Fontify Org headings.

For large result sets this may be slow, although it doesn't seem
to be a major bottleneck."
  :group 'helm-org-rifle :type 'boolean)

(defcustom helm-org-rifle-show-path nil
  "Show the whole heading path instead of just the entry's heading."
  :group 'helm-org-rifle :type 'boolean)

(defun helm-org-rifle ()
  "This is my rifle.  There are many like it, but this one is mine.

My rifle is my best friend.  It is my life.  I must master it as I
must master my life.

Without me, my rifle is useless.  Without my rifle, I am
useless.  I must fire my rifle true.  I must shoot straighter than
my enemy who is trying to kill me.  I must shoot him before he
shoots me.  I will...

My rifle and I know that what counts in war is not the rounds we
fire, the noise of our burst, nor the smoke we make.  We know that
it is the hits that count.  We will hit...

My rifle is human, even as I, because it is my life.  Thus, I will
learn it as a brother.  I will learn its weaknesses, its strength,
its parts, its accessories, its sights and its barrel.  I will
keep my rifle clean and ready, even as I am clean and ready.  We
will become part of each other.  We will...

Before God, I swear this creed.  My rifle and I are the defenders
of my country.  We are the masters of our enemy.  We are the
saviors of my life.

So be it, until victory is ours and there is no enemy, but
peace!"
  (interactive)
  (let ((helm-candidate-separator " "))
    (helm :sources (helm-org-rifle-get-sources))))

(defun helm-org-rifle-show-in-indirect-buffer (candidate)
  "Show CANDIDATE subtree in an indirect buffer."
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
  "Return list of sources configured for helm-org-rifle.
One source is returned for each open Org buffer."
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
  "Return candidates in BUFFER for INPUT.

INPUT is a string.  Candidates are returned in this
format: (STRING .  POSITION)

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

;;; helm-org-rifle.el ends here

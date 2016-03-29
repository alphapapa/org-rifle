;;; helm-org-rifle.el --- Rifle through your Org files

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/helm-org-rifle
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4") (helm "1.9.3") (org "8.2.4") (s "1.10.0"))
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

;; Run one of the rifle commands, type some words, and results will be
;; displayed, grouped by buffer.  Hit "RET" to show the selected
;; entry, or <C-return> to show it in an indirect buffer.

;; Commands:
;; + `helm-org-rifle': Shows results from all open Org buffers
;; + `helm-org-rifle-current-buffer': Shows results from current buffer

;;; Tips

;; + Show results from certain buffers by typing the name of the
;;   buffer (usually the filename).
;; + Show headings with certain todo keywords by typing the keyword,
;;   e.g. =TODO= or =DONE=.
;; + Show headings with certain priorities by typing, e.g. =#A= or
;;   =[#A]=.
;; + Show headings with certain tags by searching for, e.g. =:tag1:=.
;; + Exclude results with a =!=, e.g. =pepperoni !anchovies=.
;; + Show entries in an indirect buffer by selecting that action from
;;   the Helm actions list.

;;; Credits:

;; This package is based on org-search-goto (specifically,
;; org-search-goto-ml).  Its unofficial-official home is on
;; EmacsWiki[1] but I've mirrored it on GitHub[2].  It's a really
;; great package, and the only thing that could make it better is to
;; make it work with Helm.  To avoid confusion, this package has a
;; completely different name.
;;
;;  [1] https://www.emacswiki.org/emacs/org-search-goto-ml.el
;;  [2] https://github.com/alphapapa/org-search-goto

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

(require 'cl-lib)
(require 'helm)
(require 'org)
(require 's)
(require 'cl-lib)

;;;; Vars

(defconst helm-org-rifle-fontify-buffer-name " *helm-org-rifle-fontify*"
  "The name of the invisible buffer used to fontify `org-mode' strings.")

(defconst helm-org-rifle-tags-re (org-re ":\\([[:alnum:]_@#%:]+\\):[ \t]*$")
  "Regexp used to match Org tag strings.  From org.el.")

(defvar helm-org-rifle-map
  (let ((new-map (copy-keymap helm-map)))
    (define-key new-map (kbd "<C-return>") 'helm-org-rifle-show-entry-in-indirect-buffer-map-action)
    new-map)
  "Keymap for `helm-org-rifle'.")

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

(defcustom helm-org-rifle-show-entry-function 'helm-org-rifle-show-entry-in-real-buffer
  "Default function to use to show selected entries."
  :group 'helm-org-rifle
  :type '(radio (function :tag "Show entries in real buffers." helm-org-rifle-show-entry-in-real-buffer :value)
                (function :tag "Show entries in indirect buffers." helm-org-rifle-show-entry-in-indirect-buffer)
                (function :tag "Custom function")))

(defcustom helm-org-rifle-show-todo-keywords t
  "Show Org and match against todo keywords."
  :group 'helm-org-rifle :type 'boolean)

(defcustom helm-org-rifle-show-path nil
  "Show the whole heading path instead of just the entry's heading."
  :group 'helm-org-rifle :type 'boolean)

(defcustom helm-org-rifle-show-tags nil
  "Show and match against Org tags."
  :group 'helm-org-rifle :type 'boolean)

(defcustom helm-org-rifle-re-begin-part
  "\\(\\_<\\|[[:punct:]]\\)"
  "Regexp matched immediately before each search term.
\(Customize at your peril (but it's okay to experiment here,
because you can always revert your changes).)"
  :group 'helm-org-rifle :type 'regexp)

(defcustom helm-org-rifle-re-end-part
  "\\(\\_>\\|[[:punct:]]\\)"
  "Regexp matched immediately after each search term.
\(What, didn't you read the last warning?  Oh, nevermind.)"
  :group 'helm-org-rifle :type 'regexp)

;;;; Functions

;;;###autoload
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

;;;###autoload
(defun helm-org-rifle-current-buffer ()
  "Rifle through the current buffer."
  (interactive)
  (let ((helm-candidate-separator " "))
    (helm :sources (helm-org-rifle-get-source (current-buffer)))))

(defun helm-org-rifle-show-entry (candidate)
  "Show CANDIDATE using the default function."
  (funcall helm-org-rifle-show-entry-function candidate))

(defun helm-org-rifle-show-entry-in-real-buffer (candidate)
  "Show CANDIDATE in its real buffer."
  (switch-to-buffer (helm-attr 'buffer))
  (goto-char (car candidate))
  (org-show-entry))

(defun helm-org-rifle-show-entry-in-indirect-buffer (candidate)
  "Show CANDIDATE in an indirect buffer."
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
(defun helm-org-rifle-show-entry-in-indirect-buffer-map-action ()
  "Exit Helm buffer and call `helm-org-rifle-show-entry-in-indirect-buffer' with selected candidate."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org-rifle-show-entry-in-indirect-buffer)))

(defun helm-org-rifle-buffer-invisible-p (buffer)
  "Return non-nil if BUFFER is invisible.
That is, if its name starts with a space."
  (s-starts-with? " " (buffer-name buffer)))

(defun helm-org-rifle-get-source (buffer)
  "Get a rifle buffer for BUFFER."
  (let ((source (helm-build-sync-source (buffer-name buffer)
                  :candidates (lambda ()
                                (when (s-present? helm-pattern)
                                  (helm-org-rifle-get-candidates-in-buffer (helm-attr 'buffer) helm-pattern)))
                  :match 'identity
                  ;; Setting :delayed to a number causes
                  ;; strange behavior, duplicated results,
                  ;; causes the :candidates function to be
                  ;; called nearly once for every character
                  ;; entered, even though it is delayed for
                  ;; right amount of time.  But setting it to
                  ;; t works fine, and...fast...
                  :delayed t
                  :multiline t
                  :volatile t
                  :action (helm-make-actions
                           "Show entry" 'helm-org-rifle-show-entry
                           "Show entry in indirect buffer" 'helm-org-rifle-show-entry-in-indirect-buffer
                           "Show entry in real buffer" 'helm-org-rifle-show-entry-in-real-buffer)
                  :keymap helm-org-rifle-map)))
    (helm-attrset 'buffer buffer source)
    source))

(defun helm-org-rifle-get-sources ()
  "Return list of sources configured for helm-org-rifle.
One source is returned for each open Org buffer."
  (mapcar 'helm-org-rifle-get-source
          (cl-remove-if 'helm-org-rifle-buffer-invisible-p (org-buffer-list nil t))))

(defun helm-org-rifle-prep-token (token)
  "Apply regexp prefix and suffix for TOKEN."
  (if (string-match helm-org-rifle-tags-re token)
      ;; Tag
      (concat (concat "\\(" helm-org-rifle-tags-re "\\| \\)")
              (regexp-quote token)
              (concat "\\(" helm-org-rifle-tags-re "\\| \\|$\\)"))
    ;; Not a tag; use normal prefix/suffix
    (concat helm-org-rifle-re-begin-part
            (regexp-quote token)
            helm-org-rifle-re-end-part)))

(defun helm-org-rifle-get-candidates-in-buffer (buffer input)
  "Return candidates in BUFFER for INPUT.

INPUT is a string.  Candidates are returned in this
format: (STRING .  POSITION)

STRING begins with a fontified Org heading and optionally
includes further matching parts separated by newlines.

POSITION is the position in BUFFER where the candidate heading
begins."
  (let* ((input (split-string input " " t))
         (negations (delq nil (mapcar (lambda (token)
                                        (when (string-match "^!" token)
                                          (setq input (remove token input))
                                          (regexp-quote (s-chop-prefix "!" token))))
                                      input)))
         (all-tokens (append input negations))
         (negations (mapcar (lambda (token) (concat "\\b" token "\\b")) negations))
         (match-all-tokens-re (mapconcat 'helm-org-rifle-prep-token input "\\|"))
         ;; TODO: Turn off case folding if input contains mixed case
         (case-fold-search t)
         results)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward match-all-tokens-re nil t)
          ;; Get matching lines in node
          (catch 'negated
            (let* ((node-beg (save-excursion
                               (save-match-data
                                 (outline-previous-heading))))
                   (components (org-heading-components))
                   (path (when helm-org-rifle-show-path
                           (org-get-outline-path)))
                   (priority (when (nth 3 components)
                               ;; TODO: Is there a better way to do this?  The
                               ;; s-join leaves an extra space when there's no
                               ;; priority.
                               (concat "[#" (char-to-string (nth 3 components)) "]")))
                   (tags (nth 5 components))
                   (heading (if helm-org-rifle-show-todo-keywords
                                (s-join " " (list (nth 2 components) priority (nth 4 components)))
                              (nth 4 components)))
                   (node-end (save-match-data  ; This is confusing; should these be reversed here?  Does it matter?
                               (save-excursion
                                 (outline-next-heading)
                                 (point))))
                   (buffer-name (buffer-name buffer))
                   matching-positions-in-node
                   matching-lines-in-node
                   matched-words-with-context)

              ;; Get list of beginning-of-line positions for
              ;; lines in node that match any token
              (setq matching-positions-in-node
                    (or (cl-loop for token in all-tokens
                                 do (goto-char node-beg)
                                 while (search-forward-regexp (helm-org-rifle-prep-token token) node-end t)
                                 when negations
                                     when (cl-loop for negation in negations
                                                   thereis (s-matches? negation
                                                                       (buffer-substring-no-properties (line-beginning-position)
                                                                                                       (line-end-position))))
                                     return nil
                                 collect (line-beginning-position) into result
                                 do (end-of-line)
                                 finally return (sort (delete-dups result) '<))
                        ;; Negation found; skip node
                        (throw 'negated (goto-char node-end))))

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
              (when (cl-loop with targets = (append (delq nil (list buffer-name
                                                                    heading
                                                                    (when helm-org-rifle-show-tags tags)))
                                                    (cl-map 'list 'car matching-lines-in-node))
                             for token in input
                             always (and (cl-loop for target in targets
                                                  thereis (s-contains? token target t))))

                ;; Node matches all tokens
                (setq matched-words-with-context
                      (cl-loop for line in (cl-map 'list 'car matching-lines-in-node)
                               append (cl-loop with end
                                               for token in input
                                               for re = (rx-to-string `(and (repeat 0 ,helm-org-rifle-context-characters not-newline)
                                                                            (eval token)
                                                                            (repeat 0 ,helm-org-rifle-context-characters not-newline)))
                                               for match = (string-match re line end)
                                               if match
                                               do (setq end (match-end 0))
                                               and collect (match-string-no-properties 0 line)
                                               else do (setq end nil))))

                ;; Return list in format: (string-joining-heading-and-lines-by-newlines node-beg)
                (push (list (s-join "\n" (list (if (and helm-org-rifle-show-path
                                                        path)
                                                   (if helm-org-rifle-fontify-headings
                                                       (org-format-outline-path (append path
                                                                                        (list heading)
                                                                                        (when helm-org-rifle-show-tags
                                                                                          (concat tags " "))))
                                                     ;; Not fontifying
                                                     (s-join "/" (append path
                                                                         (list heading)
                                                                         (when helm-org-rifle-show-tags
                                                                           tags))))
                                                 ;; No path or not showing path
                                                 (if helm-org-rifle-fontify-headings
                                                     (helm-org-rifle-fontify-like-in-org-mode
                                                      (s-join " " (list (s-pad-left (nth 0 components) "*" "")
                                                                        heading
                                                                        (when helm-org-rifle-show-tags
                                                                          (concat tags " ")))))
                                                   ;; Not fontifying
                                                   (s-join " " (list (s-pad-left (nth 0 components) "*" "")
                                                                     heading
                                                                     (when helm-org-rifle-show-tags
                                                                       tags)))))
                                               (s-join "..." matched-words-with-context)))
                            node-beg)
                      results))
              ;; Go to end of node
              (goto-char node-end))))))
    ;; Return results
    results))

(defun helm-org-rifle-fontify-like-in-org-mode (s &optional odd-levels)
  "Fontify string S like in Org-mode.

`org-fontify-like-in-org-mode' is a very, very slow function
because it creates a new temporary buffer and runs `org-mode' for
every string it fontifies.  This function reuses a single
invisible buffer and only runs `org-mode' when the buffer is
created."
  (let ((buffer (get-buffer helm-org-rifle-fontify-buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create helm-org-rifle-fontify-buffer-name))
      (with-current-buffer buffer
        (org-mode)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert s)
      (let ((org-odd-levels-only odd-levels))
        (font-lock-fontify-buffer)
        (buffer-string)))))

(provide 'helm-org-rifle)

;;; helm-org-rifle.el ends here

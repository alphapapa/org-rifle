;;; helm-org-rifle.el --- Rifle through your Org files

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/helm-org-rifle
;; Version: 1.4.0-pre
;; Package-Requires: ((emacs "24.4") (dash "2.12") (f "0.18.1") (helm "1.9.4") (s "1.10.0"))
;; Keywords: hypermedia, outlines

;;; Commentary:

;; This is my rifle.  There are many like it, but this one is mine.
;; My rifle is my best friend. It is my life.  I must master it as I
;; must master my life.

;; What does my rifle do?  It searches rapidly through my Org files,
;; quickly bringing me the information I need to defeat the enemy.

;; This package is inspired by org-search-goto/org-search-goto-ml.  It
;; searches both headings and contents of entries in Org buffers, and
;; it displays entries that match all search terms, whether the terms
;; appear in the heading, the contents, or both.  Matching portions of
;; entries' contents are displayed with surrounding context to make it
;; easy to acquire your target.

;; Entries are fontified by default to match the appearance of an Org
;; buffer, and optionally the entire path can be displayed for each
;; entry, rather than just its own heading.

;;; Installation

;;;; MELPA

;; If you installed from MELPA, your rifle is ready.  Just run one of
;; the commands below.

;;;; Manual

;; Install the Helm, dash.el, f.el, and s.el packages.  Then require
;; this package in your init file:

;; (require 'helm-org-rifle)

;;; Usage

;; Run one of the rifle commands, type some words, and results will be
;; displayed, grouped by buffer.  Hit "RET" to show the selected
;; entry, or <C-return> to show it in an indirect buffer.

;; Helm commands:
;; + `helm-org-rifle': Show results from all open Org buffers
;; + `helm-org-rifle-agenda-files': Show results from Org agenda files
;; + `helm-org-rifle-current-buffer': Show results from current buffer
;; + `helm-org-rifle-directories': Show results from selected directories; with prefix, recursively
;; + `helm-org-rifle-files': Show results from selected files
;; + `helm-org-rifle-org-directory': Show results from Org files in =org-directory=

;; Occur commands:
;; + `helm-org-rifle-occur': Show results from all open Org buffers in occur-like persistent buffer
;; + `helm-org-rifle-occur-agenda-files': Show results from Org agenda files in occur-like persistent buffer
;; + `helm-org-rifle-occur-current-buffer': Show results from current buffer in occur-like persistent buffer
;; + `helm-org-rifle-occur-directories': Show results from selected directories in occur-like persistent buffer; with prefix, recursively
;; + `helm-org-rifle-occur-files': Show results from selected files in occur-like persistent buffer
;; + `helm-org-rifle-occur-org-directory': Show results from Org files in =org-directory= in occur-like persistent buffer

;;; Tips

;; + Show results from certain buffers by typing the name of the
;;   buffer (usually the filename).
;; + Show headings with certain todo keywords by typing the keyword,
;;   e.g. "TODO" or "DONE".
;; + Show headings with certain priorities by typing, e.g. "#A" or
;;   "[#A]".
;; + Show headings with certain tags by searching for, e.g. ":tag1:tag2:".
;; + Exclude results with a "!", e.g. "pepperoni !anchovies".
;; + Show entries in an indirect buffer by selecting that action from
;;   the Helm actions list, or by pressing <C-return>.
;; + The keymap for `helm-org-rifle-occur' results buffers imitates the =org-speed= keys, making it quicker to navigate. Results buffers are marked read-only so you cannot modify them by accidental keypresses.
;; + You can customize the `helm-org-rifle' group if you like.

;;; Credits:

;; This package is inspired by org-search-goto (specifically,
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

;;;; Require

(require 'dash)
(require 'f)
(require 'helm)
(require 'org)
(require 's)

;;;; Vars

(defconst helm-org-rifle-fontify-buffer-name " *helm-org-rifle-fontify*"
  "The name of the invisible buffer used to fontify `org-mode' strings.")

(defconst helm-org-rifle-occur-results-buffer-name "*helm-org-rifle-occur*"
  "The name of the results buffer for `helm-org-rifle-occur' commands.")

(defconst helm-org-rifle-tags-re (org-re "\\(?:[ \t]+\\(:[[:alnum:]_@#%%:]+:\\)\\)?")
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

(defcustom helm-org-rifle-after-init-hook '(helm-org-rifle-set-input-idle-delay)
  "`:after-init-hook' for the Helm buffer.
If you're thinking about changing this, you probably know what you're doing."
  :group 'helm-org-rifle :type 'hook)

(defcustom helm-org-rifle-always-show-entry-contents-chars 50
  "When non-zero, always show this many characters of entry text, even if none of it matches query."
  :group 'helm-org-rifle :type 'integer)

(defcustom helm-org-rifle-close-unopened-file-buffers t
  "Close buffers that were not already open.
After rifling through Org files that are not already open, close
the buffers if non-nil.  If nil, leave the buffers open.  Leaving
them open will speed up subsequent searches but clutter the
buffer list."
  :group 'helm-org-rifle :type 'boolean)

(defcustom helm-org-rifle-context-characters 25
  "How many characters around each matched term to display."
  :group 'helm-org-rifle :type 'integer)

(defcustom helm-org-rifle-directories-recursive t
  "Recurse into subdirectories by default in `helm-org-rifle-directories'.
When `helm-org-rifle-directories' is called with a prefix, this
option will be inverted."
  :group 'helm-org-rifle :type 'boolean)

(defcustom helm-org-rifle-ellipsis-string "..."
  "Shown between match context strings."
  :group 'helm-org-rifle :type 'string)

(defcustom helm-org-rifle-ellipsis-face 'font-lock-comment-delimiter-face
  "Face for ellipses between match context strings."
  :group 'helm-org-rifle :type 'face)

(defcustom helm-org-rifle-directories-filename-regexp "\.org$"
  "Regular expression to match Org filenames in `helm-org-rifle-directories'.
Files matching this regexp will be searched.  By default, \".org\" files are matched, but you may also select to include \".org_archive\" files, or use a custom regexp."
  :group 'helm-org-rifle
  :type '(radio (string :tag "Normal \".org\" files" :value "\.org$")
                (string :tag "Also include \".org_archive\" files" "\.org\\(_archive\\)?$")
                (string :tag "Custom regexp.  You know what you're doing.")))

(defcustom helm-org-rifle-fontify-headings t
  "Fontify Org headings.

For large result sets this may be slow, although it doesn't seem
to be a major bottleneck."
  :group 'helm-org-rifle :type 'boolean)

(defcustom helm-org-rifle-input-idle-delay 0.05
  "How long to wait to find results after the user stops typing, in seconds.
This helps prevent flickering in the Helm buffer, because the
default value for `helm-idle-input-delay' is 0.01, which runs the
search immediately after each keystroke.  You can adjust this to
get results more quickly (shorter delay) or further reduce
flickering (longer delay)."
  :group 'helm-org-rifle :type 'float)

(defcustom helm-org-rifle-show-entry-function 'helm-org-rifle-show-entry-in-real-buffer
  "Default function to use to show selected entries."
  :group 'helm-org-rifle
  :type '(radio (function :tag "Show entries in real buffers." helm-org-rifle-show-entry-in-real-buffer :value)
                (function :tag "Show entries in indirect buffers." helm-org-rifle-show-entry-in-indirect-buffer)
                (function :tag "Custom function")))

(defcustom helm-org-rifle-show-full-contents nil
  "Show all of each result's contents instead of just context around each matching word."
  :group 'helm-org-rifle :type 'boolean)

(defcustom helm-org-rifle-show-todo-keywords t
  "Show and match against Org todo keywords."
  :group 'helm-org-rifle :type 'boolean)

(defcustom helm-org-rifle-show-path nil
  "Show the whole heading path instead of just the entry's heading."
  :group 'helm-org-rifle :type 'boolean)

(defcustom helm-org-rifle-re-prefix
  "\\(\\_<\\|[[:punct:]]\\)"
  "Regexp matched immediately before each search term.
\(Customize at your peril (but it's okay to experiment here,
because you can always revert your changes).)"
  :group 'helm-org-rifle :type 'regexp)

(defcustom helm-org-rifle-re-suffix
  "\\(\\_>\\|[[:punct:]]\\)"
  "Regexp matched immediately after each search term.
\(What, didn't you read the last warning?  Oh, nevermind.)"
  :group 'helm-org-rifle :type 'regexp)

(defvar helm-org-rifle-occur-map (let ((map (copy-keymap org-mode-map)))
                                   (define-key map [mouse-1] 'helm-org-rifle-occur-goto-entry)
                                   (define-key map (kbd "<RET>") 'helm-org-rifle-occur-goto-entry)
                                   (define-key map (kbd "b") (lambda () (org-speed-move-safe 'org-backward-heading-same-level)))
                                   (define-key map (kbd "f") (lambda () (org-speed-move-safe 'org-forward-heading-same-level)))
                                   (define-key map (kbd "p") (lambda () (org-speed-move-safe 'outline-previous-visible-heading)))
                                   (define-key map (kbd "n") (lambda () (org-speed-move-safe 'outline-next-visible-heading)))
                                   (define-key map (kbd "u") (lambda () (org-speed-move-safe 'outline-up-heading)))
                                   (define-key map (kbd "o") (lambda () (org-speed-move-safe 'org-open-at-point)))
                                   (define-key map (kbd "c") (lambda () (org-speed-move-safe 'org-cycle)))
                                   (define-key map (kbd "C") (lambda () (org-speed-move-safe 'org-shifttab)))
                                   (define-key map (kbd "q") 'quit-window)
                                   map)
  "Keymap for helm-org-rifle-occur results buffers.  Imitates org-speed keys.")

(defvar helm-org-rifle-occur-minibuffer-map (let ((map (copy-keymap minibuffer-local-map)))
                                              (define-key map (kbd "C-g") 'helm-org-rifle-occur-cleanup-buffer)
                                              map)
  "Keymap for helm-org-rifle-occur minibuffers.")

(defvar helm-org-rifle-occur-last-input nil
  "Last input given, used to avoid re-running search when input hasn't changed.")


;;;; Functions

;;;;; Commands

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
    (helm :sources (helm-org-rifle-get-sources-for-open-buffers))))

;;;###autoload
(defun helm-org-rifle-agenda-files ()
  "Rifle through Org agenda files."
  (interactive)
  (helm-org-rifle-files org-agenda-files))

;;;###autoload
(defun helm-org-rifle-current-buffer ()
  "Rifle through the current buffer."
  (interactive)
  (let ((helm-candidate-separator " "))
    (helm :sources (helm-org-rifle-get-source-for-buffer (current-buffer)))))

;;;###autoload
(defun helm-org-rifle-files (&optional files)
  "Rifle through FILES, where FILES is a list of paths to Org files.
If FILES is nil, prompt with `helm-read-file-name'.  All FILES
are searched; they are not filtered with
`helm-org-rifle-directories-filename-regexp'."
  (interactive)
  (let ((files (or files (helm-read-file-name "Files: " :marked-candidates t)))
        (helm-candidate-separator " ")
        (helm-cleanup-hook (lambda ()
                             ;; Close new buffers if enabled
                             (when helm-org-rifle-close-unopened-file-buffers
                               (if (= 0 helm-exit-status)
                                   ;; Candidate selected; close other new buffers
                                   (let ((candidate-source (helm-attr 'name (helm-get-current-source))))
                                     (dolist (source (helm-get-sources))
                                       (unless (or (equal (helm-attr 'name source)
                                                          candidate-source)
                                                   (not (helm-attr 'new-buffer source)))
                                         (kill-buffer (helm-attr 'buffer source)))))
                                 ;; No candidates; close all new buffers
                                 (dolist (source (helm-get-sources))
                                   (when (helm-attr 'new-buffer source)
                                     (kill-buffer (helm-attr 'buffer source)))))))))
    (helm :sources (--map (helm-org-rifle-get-source-for-file it) files))))

;;;###autoload
(defun helm-org-rifle-directories (&optional directories toggle-recursion)
  "Rifle through Org files in DIRECTORIES.
If DIRECTORIES is nil, prompt with `helm-read-file-name'.  With
prefix or TOGGLE-RECURSION non-nil, toggle recursion from the
default.  Files in DIRECTORIES are filtered using
`helm-org-rifle-directories-filename-regexp'."
  (interactive)
  (let* ((recursive (if (or toggle-recursion current-prefix-arg)
                        (not helm-org-rifle-directories-recursive)
                      helm-org-rifle-directories-recursive))
         (directories (or directories
                          (-select 'f-dir? (helm-read-file-name "Directories: " :marked-candidates t))))
         (files (-flatten (--map (f-files it
                                          (lambda (file)
                                            (s-matches? helm-org-rifle-directories-filename-regexp (f-filename file)))
                                          recursive)
                                 directories))))
    (if files
        (helm-org-rifle-files files)
      (error "No org files found in directories: %s" (s-join " " directories)))))

;;;###autoload
(defun helm-org-rifle-org-directory ()
  "Rifle through Org files in `org-directory'."
  (interactive)
  (helm-org-rifle-directories (list org-directory)))

;;;;;; Occur commands

;;;###autoload
(cl-defmacro helm-org-rifle-define-occur-command (name args docstring &key buffers files directories preface)
  "Define `helm-org-rifle-occur' command to search BUFFERS."
  `(defun ,(intern (concat "helm-org-rifle-occur"
                           (when name (concat "-" name))))
       ,args
     ,docstring
     (interactive)
     (let (directories-collected files-collected buffers-collected)
       ;; FIXME: If anyone's reading this and can help me clean up this macro a bit, help would be appreciated.
       ,preface  ; Maybe not necessary
       ,(when directories
          ;; Is there a nicer way to do this?
          `(setq directories-collected (append directories-collected ,directories)))
       (when directories-collected
         (let ((recursive (if current-prefix-arg
                              (not helm-org-rifle-directories-recursive)
                            helm-org-rifle-directories-recursive)))
           (setq files-collected (append files-collected
                                         (-flatten
                                          (--map (f-files it
                                                          (lambda (file)
                                                            (s-matches? helm-org-rifle-directories-filename-regexp
                                                                        (f-filename file)))
                                                          recursive)
                                                 directories-collected))))))
       ,(when files
          ;; Is there a nicer way to do this?
          `(setq files-collected (append files-collected ,files)))
       (when files-collected
         (setq buffers-collected (append (cl-loop for file in files-collected
                                                  collect (-if-let (buffer (org-find-base-buffer-visiting file))
                                                              buffer
                                                            (find-file-noselect file)))
                                         buffers-collected)))
       ,(when buffers
          ;; Is there a nicer way to do this?
          `(setq buffers-collected (append buffers-collected ,buffers)))
       (let ((helm-org-rifle-show-full-contents t))
         (helm-org-rifle-occur-begin buffers-collected)))))

;;;###autoload
(helm-org-rifle-define-occur-command
 nil ()
 "Search all Org buffers, showing results in an occur-like, persistent buffer."
 :buffers (--remove (string= helm-org-rifle-occur-results-buffer-name (buffer-name it))
                    (-select 'helm-org-rifle-buffer-visible-p
                             (org-buffer-list nil t))))
;;;###autoload
(helm-org-rifle-define-occur-command
 "current-buffer" ()
 "Search current buffer, showing results in an occur-like, persistent buffer."
 :buffers (list (current-buffer)))

;;;###autoload
(helm-org-rifle-define-occur-command
 "directories" (&optional directories)
 "Search files in DIRECTORIES, showing results in an occur-like, persistent buffer.
Files are opened if necessary, and the resulting buffers are left open."
 :directories (or directories
                  (helm-read-file-name "Directories: " :marked-candidates t)))

;;;###autoload
(helm-org-rifle-define-occur-command
 "files" (&optional files)
 "Search FILES, showing results in an occur-like, persistent buffer.
Files are opened if necessary, and the resulting buffers are left open."
 :files (or files
            (helm-read-file-name "Files: " :marked-candidates t)))

;;;###autoload
(helm-org-rifle-define-occur-command
 "agenda-files" ()
 "Search Org agenda files, showing results in an occur-like, persistent buffer.
Files are opened if necessary, and the resulting buffers are left open."
 :files org-agenda-files)

;;;###autoload
(helm-org-rifle-define-occur-command
 "org-directory" ()
 "Search files in `org-directory', showing results in an occur-like, persistent buffer.
Files are opened if necessary, and the resulting buffers are left open."
 :directories (list org-directory))

;;;;; Sources

(defun helm-org-rifle-get-source-for-buffer (buffer)
  "Return Helm source for BUFFER."
  (let ((source (helm-build-sync-source (buffer-name buffer)
                  :after-init-hook helm-org-rifle-after-init-hook
                  :candidates (lambda ()
                                (when (s-present? helm-pattern)
                                  (helm-org-rifle-get-candidates-in-buffer (helm-attr 'buffer) helm-pattern)))

                  :match 'identity
                  :multiline t
                  :volatile t
                  :action (helm-make-actions
                           "Show entry" 'helm-org-rifle-show-entry
                           "Show entry in indirect buffer" 'helm-org-rifle-show-entry-in-indirect-buffer
                           "Show entry in real buffer" 'helm-org-rifle-show-entry-in-real-buffer)
                  :keymap helm-org-rifle-map)))
    (helm-attrset 'buffer buffer source)
    source))

(defun helm-org-rifle-get-source-for-file (file)
  "Return Helm source for FILE.
If the file is not already in an open buffer, it will be opened
with `find-file-noselect'."
  (let ((buffer (org-find-base-buffer-visiting file))
        new-buffer source)
    (unless buffer
      (if (f-exists? file)
          (progn
            (setq buffer (find-file-noselect file))
            (setq new-buffer t))
        (error "File not found: %s" file)))
    (setq source (helm-org-rifle-get-source-for-buffer buffer))
    (helm-attrset 'new-buffer new-buffer source)
    source))

(defun helm-org-rifle-get-sources-for-open-buffers ()
  "Return list of sources configured for helm-org-rifle.
One source is returned for each open Org buffer."
  (mapcar 'helm-org-rifle-get-source-for-buffer
          (-select 'helm-org-rifle-buffer-visible-p (org-buffer-list nil t))))

;;;;; Show entries

(defun helm-org-rifle-show-entry (candidate)
  "Show CANDIDATE using the default function."
  (funcall helm-org-rifle-show-entry-function candidate))

(defun helm-org-rifle-show-entry-in-real-buffer (candidate)
  "Show CANDIDATE in its real buffer."
  (helm-attrset 'new-buffer nil)  ; Prevent the buffer from being cleaned up
  (switch-to-buffer (helm-attr 'buffer))
  (goto-char (car candidate))
  (org-show-entry))

(defun helm-org-rifle-show-entry-in-indirect-buffer (candidate)
  "Show CANDIDATE in an indirect buffer."
  (let ((buffer (helm-attr 'buffer))
        (pos (car candidate))
        (original-buffer (current-buffer)))
    (helm-attrset 'new-buffer nil)  ; Prevent the buffer from being cleaned up
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

;;;;; The meat

(defun helm-org-rifle-get-candidates-in-buffer (buffer input)
  "Return candidates in BUFFER for INPUT.

INPUT is a string.  Candidates are returned in this
format: (STRING . POSITION)

STRING begins with a fontified Org heading and optionally
includes further matching parts separated by newlines.

POSITION is the position in BUFFER where the candidate heading
begins.

This is how the sausage is made."
  (let* ((tokens (helm-org-rifle-split-tags-in-input-list (split-string input " " t)))
         (negations (-keep (lambda (token)
                             (when (string-match "^!" token)
                               (setq tokens (remove token tokens))  ; Remove negations from tokens
                               (s-presence (regexp-quote (s-chop-prefix "!" token)))))
                           tokens))
         (negations-re (when negations
                         (rx-to-string `(seq bow (or ,@negations) eow) t)))
         (positive-re (mapconcat 'helm-org-rifle-prep-token tokens "\\|"))
         (positive-re-list (--map (helm-org-rifle-prep-token it) tokens))
         (context-re (s-wrap (s-join "\\|" tokens)
                             (rx-to-string `(seq (repeat 0 ,helm-org-rifle-context-characters not-newline)) t)))
         ;; TODO: Turn off case folding if tokens contains mixed case
         (case-fold-search t)
         results)
    (with-current-buffer buffer
      (save-excursion

        ;; Go to first heading
        (goto-char (point-min))
        (when (org-before-first-heading-p)
          (outline-next-heading))

        ;; Search for matching nodes
        (while (re-search-forward positive-re nil t)
          (catch 'negated  ; Skip node if negations found
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

              ;; Check negations
              (when (and negations
                         (re-search-forward negations-re node-end t))
                (throw 'negated (goto-char node-end)))

              ;; Get beginning-of-line positions for matching lines in node
              (setq matching-positions-in-node
                    (cl-loop initially (goto-char node-beg)
                             while (re-search-forward positive-re node-end t)
                             collect (line-beginning-position) into result
                             do (end-of-line)
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
              (when (cl-loop with targets = (append (-non-nil (list buffer-name
                                                                    heading
                                                                    tags))
                                                    (mapcar 'car matching-lines-in-node))
                             for re in positive-re-list
                             always (cl-loop for target in targets
                                             thereis (s-matches? re target)))

                ;; Node matches all tokens
                (unless helm-org-rifle-show-full-contents
                  (setq matched-words-with-context
                        (or (cl-loop for line in (mapcar 'car matching-lines-in-node)
                                     ;; Matching entry text found
                                     append (cl-loop with end
                                                     for match = (string-match context-re line end)
                                                     while match
                                                     do (setq end (match-end 0))
                                                     and collect (s-trim (match-string-no-properties 0 line))))
                            (when (> helm-org-rifle-always-show-entry-contents-chars 0)
                              ;; No match in entry text; add desired entry text
                              (list (s-collapse-whitespace (s-trim (s-truncate helm-org-rifle-always-show-entry-contents-chars (org-get-entry)))))))))

                ;; Return list in format: (text-for-display node-beg)
                (let* ((heading (if path
                                    (if helm-org-rifle-fontify-headings
                                        (concat (org-format-outline-path
                                                 ;; Replace links in path elements with plain text, otherwise
                                                 ;; they will be truncated by `org-format-outline-path' and only
                                                 ;; show part of the URL
                                                 (-map 'helm-org-rifle-replace-links-in-string path))
                                                "/"
                                                (helm-org-rifle-fontify-like-in-org-mode
                                                 (s-join " " (list (s-repeat (nth 0 components) "*")
                                                                   heading
                                                                   (concat tags " ")))))
                                      ;; Not fontifying
                                      (s-join "/" (append path
                                                          (list heading)
                                                          tags)))
                                  ;; No path or not showing path
                                  (if helm-org-rifle-fontify-headings
                                      (helm-org-rifle-fontify-like-in-org-mode
                                       (s-join " " (list (s-repeat (nth 0 components) "*")
                                                         heading
                                                         (concat tags " "))))
                                    ;; Not fontifying
                                    (s-join " " (list (s-repeat (nth 0 components) "*")
                                                      heading
                                                      tags)))))
                       (entry (if helm-org-rifle-show-full-contents
                                  (s-join "\n" (list heading (org-get-entry)))
                                ;; Show context strings
                                (s-join "\n" (list heading (s-join helm-org-rifle-ellipsis-string matched-words-with-context))))))
                  (push (list entry node-beg)
                        results)))
              ;; Go to end of node
              (goto-char node-end))))))
    ;; Return results in the order they appear in the org file
    (nreverse results)))

;;;;; Occur-style

(defun helm-org-rifle-occur-begin (source-buffers)
  "Begin occur-style command searching BUFFERS, opening results buffer, focusing minibuffer, and running timer to put results in buffer."
  (let ((inhibit-read-only t)
        ;; I can't figure out why the asterisks are causing the buffer
        ;; to not show up in my Helm buffer list, but it does show up
        ;; in ibuffer.
        (results-buffer (get-buffer-create helm-org-rifle-occur-results-buffer-name))
        helm-org-rifle-occur-last-input
        timer)

    ;; Prepare results buffer
    (with-current-buffer results-buffer
      (unless (eq major-mode 'org-mode)
        (read-only-mode)
        (visual-line-mode)
        (org-mode)
        (hi-lock-mode 1)
        (use-local-map helm-org-rifle-occur-map))
      (erase-buffer)
      (pop-to-buffer results-buffer))

    ;; Run input timer
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (setq timer (run-with-idle-timer
                           ;; FIXME: helm-org-rifle-input-idle-delay
                           ;; doesn't seem to work the same as in a
                           ;; Helm session, so a longer value is
                           ;; needed.  It'd be good to make this work
                           ;; with the same value...
                           0.25
                           'repeat
                           (lambda ()
                             (helm-org-rifle-occur-process-input (s-trim (minibuffer-contents)) source-buffers results-buffer)))))
          (read-from-minibuffer "pattern: " nil helm-org-rifle-occur-minibuffer-map nil nil nil nil))
      (when timer (cancel-timer timer) (setq timer nil)))))

(defun helm-org-rifle-occur-process-input (input source-buffers results-buffer)
  "Find results in SOURCE-BUFFERS for INPUT and insert into RESULTS-BUFFER."
  (when (and (s-present? input)
             (not (string= input helm-org-rifle-occur-last-input)))
    (setq helm-org-rifle-occur-last-input input)
    (let ((inhibit-read-only t)
          (results-by-buffer (cl-loop for source-buffer in source-buffers
                                      collect (helm-org-rifle-occur-get-results-in-buffer source-buffer input))))
      (with-current-buffer results-buffer
        (erase-buffer)
        (cl-loop for buffer-results in results-by-buffer
                 when buffer-results
                 do (let ((buffer-name (buffer-name (get-text-property 0 :buffer (car buffer-results)))))
                      (helm-org-rifle-insert-source-header buffer-name)
                      (cl-loop for entry in buffer-results
                               do (progn
                                    (insert entry)
                                    (insert "\n\n")))))
        (helm-org-rifle-occur-highlight-matches-in-buffer results-buffer input)))))

(defun helm-org-rifle-occur-highlight-matches-in-buffer (buffer input)
  "Highlight matches for INPUT in BUFFER using hi-lock-mode."
  (with-current-buffer buffer
    (unhighlight-regexp t)
    (dolist (token (helm-org-rifle-split-tags-in-input-list (s-split-words input)))
      (highlight-regexp token))))

(defun helm-org-rifle-occur-get-results-in-buffer (buffer input)
  "Return list of results for INPUT in BUFFER.
Results is a list of strings with text-properties :NODE-BEG and :BUFFER."
  (with-current-buffer buffer
    (unless (eq major-mode 'org-mode)
      (error "Buffer %s is not an Org buffer." buffer)))
  (cl-loop for entry in (helm-org-rifle-get-candidates-in-buffer buffer input)
           collect (-let (((text pos) entry))
                     (add-text-properties 0 (length text) (list :buffer buffer :node-beg pos) text)
                     text)))

(defun helm-org-rifle-occur-goto-entry ()
  "Go to node in source buffer that point in occur buffer is in."
  (interactive)
  (let* ((properties (text-properties-at (point)))
         (source-buffer (plist-get properties :buffer))
         (node-beg (plist-get properties :node-beg))
         ;; Get offset of point in node (not sure why +2 is necessary but it works)
         (offset (+ 2 (- (point) (previous-single-property-change (point) :node-beg)))))
    (pop-to-buffer source-buffer)
    (goto-char (+ node-beg offset))
    (org-reveal)
    (org-cycle)))

;;;;; Support functions

(defun helm-org-rifle-buffer-visible-p (buffer)
  "Return non-nil if BUFFER is visible.
That is, if its name does not start with a space."
  (not (s-starts-with? " " (buffer-name buffer))))

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

(defun helm-org-rifle-insert-source-header (name &optional display-string)
  "Insert header of source NAME into the current buffer.
If DISPLAY-STRING is non-`nil' and a string value then display
this additional info after the source name by overlay.
From `helm-insert-header'."
  (unless (bobp)
    (let ((start (point)))
      (insert "\n")
      (set-text-properties start (point) '(helm-header-separator t))))
  (let ((start (point)))
    (insert (concat " " name))
    (set-text-properties (point-at-bol) (point-at-eol) '(helm-header t))
    (when display-string
      (overlay-put (make-overlay (point-at-bol) (point-at-eol))
                   'display display-string))
    (insert "\n")
    (set-text-properties start (point) '(font-lock-face helm-source-header))))

(defun helm-org-rifle-occur-cleanup-buffer ()
  "Cleanup occur results buffer when search is aborted."
  ;; FIXME: It doesn't seem like this should have to be a command,
  ;; because it pollutes the command list, but it gave an error once
  ;; when it waasn't one, and it caused weird behavior...
  (interactive)
  (ignore-errors
    ;; Ignore errors to avoid any weirdness which may result in
    ;; infinite loops and being stuck in the minibuffer.  This has
    ;; never happened to me, of course...
    (kill-buffer helm-org-rifle-occur-results-buffer-name)
    ;; Not sure if this is absolutely necessary, but it seems to help
    ;; avoid a weird bug, so it's staying.
    (abort-recursive-edit)))

(defun helm-org-rifle-prep-token (token)
  "Apply regexp prefix and suffix for TOKEN."
  (if (string-match helm-org-rifle-tags-re token)
      ;; Tag
      (concat (concat "\\(" helm-org-rifle-tags-re "\\| \\)")
              (regexp-quote token)
              (concat "\\(" helm-org-rifle-tags-re "\\| \\|$\\)"))
    ;; Not a tag; use normal prefix/suffix
    (concat helm-org-rifle-re-prefix
            (regexp-quote token)
            helm-org-rifle-re-suffix)))

(defun helm-org-rifle-replace-links-in-string (string)
  "Replace `org-mode' links in STRING with their descriptions."
  (if (string-match org-bracket-link-regexp string)
      (replace-regexp-in-string org-bracket-link-regexp
                                (lambda (text) (match-string-no-properties 3 text))
                                string)
    ;; No links found; return original string
    string))

(defun helm-org-rifle-split-tags-in-input-list (input)
  "Split strings containing multiple Org tags in INPUT into separate tag strings.
i.e. a string like \":tag1:tag2:\" becomes two strings, \":tag1:\" and \":tag2:\"."
  (cl-loop for string in input
           for tags = (helm-org-rifle-split-tag-string string)
           if tags collect tags into result
           else collect string into result
           finally return (-flatten result)))

(defun helm-org-rifle-split-tag-string (s)
  "Return list containing Org tag strings for input string S containing Org tags.
i.e. for S \":tag1:tag2:\" a list '(\":tag1:\" \":tag2:\") is returned."
  (cl-loop for tag in (s-match-strings-all "\\(:[[:alnum:]_@#%%]+:\\)" s)
           collect (cdr tag)))

(defun helm-org-rifle-set-input-idle-delay ()
  "Set `helm-input-idle-delay' in Helm buffer."
  (with-helm-buffer
    (setq-local helm-input-idle-delay helm-org-rifle-input-idle-delay)))

(provide 'helm-org-rifle)

;;; helm-org-rifle.el ends here

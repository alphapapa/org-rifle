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

;;; Installation:

;;;; MELPA

;; If you installed from MELPA, your rifle is ready.  Just run one of
;; the commands below.

;;;; Manual

;; Install the Helm, dash.el, f.el, and s.el packages.  Then require
;; this package in your init file:

;; (require 'helm-org-rifle)

;;; Usage:

;; Run one of the rifle commands, type some words, and results will be
;; displayed, grouped by buffer.  Hit "RET" to show the selected
;; entry, or <C-return> to show it in an indirect buffer.

;; Helm commands: show results in a Helm buffer
;; + `helm-org-rifle': Show results from all open Org buffers
;; + `helm-org-rifle-agenda-files': Show results from Org agenda files
;; + `helm-org-rifle-current-buffer': Show results from current buffer
;; + `helm-org-rifle-directories': Show results from selected directories; with prefix, recursively
;; + `helm-org-rifle-files': Show results from selected files
;; + `helm-org-rifle-org-directory': Show results from Org files in `org-directory'

;; Occur commands: show results in an occur-like, persistent buffer
;; + `helm-org-rifle-occur': Show results from all open Org buffers
;; + `helm-org-rifle-occur-agenda-files': Show results from Org agenda files
;; + `helm-org-rifle-occur-current-buffer': Show results from current buffer
;; + `helm-org-rifle-occur-directories': Show results from selected directories; with prefix, recursively
;; + `helm-org-rifle-occur-files': Show results from selected files
;; + `helm-org-rifle-occur-org-directory': Show results from Org files in `org-directory'

;;;; Tips

;; + Select multiple entries in the Helm buffer to display selected
;;   entries in a read-only, `occur`-style buffer.
;; + Save all results in a Helm buffer to a `helm-org-rifle-occur`
;;   buffer by pressing `C-s` (like `helm-grep-save-results`).
;; + Show results from certain buffers by typing the name of the
;;   buffer (usually the filename).
;; + Show headings with certain to-do keywords by typing the keyword,
;;   e.g. `TODO` or `DONE`.
;; + Show headings with certain priorities by typing, e.g. `#A` or
;;   `[#A]`.
;; + Show headings with certain tags by searching for,
;;   e.g. `:tag1:tag2:`.
;; + Negate matches with a `!`, e.g. `pepperoni !anchovies`.
;; + Sort results by timestamp or buffer-order (the default) by
;;   calling commands with a universal prefix (`C-u`).
;; + Show entries in an indirect buffer by selecting that action from
;;   the Helm actions list, or by pressing `<C-return>`.
;; + The keymap for `helm-org-rifle-occur` results buffers imitates
;;   the `org-speed` keys, making it quicker to navigate. You can also
;;   collapse and expand headings and drawers with `TAB` and `S-TAB`,
;;   just like in regular Org buffers.  Results buffers are marked
;;   read-only so you cannot modify them by accidental keypresses.
;; + Delete the result at point in `helm-org-rifle-occur` buffers by
;;   pressing `d`.  This does not alter the source buffers but simply
;;   removes uninteresting results from view.
;; +  You can customize the `helm-org-rifle` group if you like.

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
    ;; FIXME: The C-s bind seems to only work when pressed twice;
    ;; before being pressed, it's not bound in the keymap, but after
    ;; pressing it once, it is, and then it works.  Weird.
    (define-key new-map (kbd "C-s") 'helm-org-rifle--save-results)
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

(defcustom helm-org-rifle-before-command-hook '(helm-org-rifle-set-sort-mode)
  "Hook that runs before each helm-org-rifle command."
  :group 'helm-org-rifle :type 'hook)

(defcustom helm-org-rifle-after-command-hook '(helm-org-rifle-reset-sort-mode)
  "Hook that runs after each helm-org-rifle command."
  :group 'helm-org-rifle :type 'hook)

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

(defcustom helm-org-rifle-heading-contents-separator "\n"
  "Separator inserted between entry's heading and contents.
Usually this should be a newline, but it may be useful to adjust
it when defining custom commands.  For example, by setting this
to a non-newline value and disabling `helm-org-rifle-multiline',
each result can be displayed on a single line."
  :type 'string)

(defcustom helm-org-rifle-input-idle-delay 0.05
  "How long to wait to find results after the user stops typing, in seconds.
This helps prevent flickering in the Helm buffer, because the
default value for `helm-idle-input-delay' is 0.01, which runs the
search immediately after each keystroke.  You can adjust this to
get results more quickly (shorter delay) or further reduce
flickering (longer delay)."
  :group 'helm-org-rifle :type 'float)

(defcustom helm-org-rifle-multiline t
  "Show entries on multiple lines, with the heading on the first line and a blank line between.
In most cases this should remain on, but it may be useful to
disable it when defining custom commands.  Note that if this is
disabled, usually `helm-org-rifle-heading-contents-separator'
should be set to a non-newline value, e.g. a space or something
like \": \"."
  :type 'boolean)

(defcustom helm-org-rifle-show-entry-function 'helm-org-rifle-show-entry-in-real-buffer
  "Default function to use to show selected entries."
  :group 'helm-org-rifle
  :type '(radio (function :tag "Show entries in real buffers." helm-org-rifle-show-entry-in-real-buffer)
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

(defcustom helm-org-rifle-sort-order nil
  "Sort results in this order by default.
The sort order may be changed temporarily by calling a command with a universal prefix (C-u).

This is a list of functions which may be called to transform results, typically by sorting them."
  ;; There seems to be a bug or at least inconsistency in the Emacs
  ;; customize system.  Setting :tag in an item in a choice or radio
  ;; list does not allow you to read the :tag from the choice as a
  ;; plist key, because the key is the second value in the list,
  ;; making the list not a plist at all.  Also, changing the order of
  ;; the elements in a list item seems to break the customize dialog,
  ;; e.g. causing the :tag description to not be shown at all.  It
  ;; seems like Emacs handles these as pseudo-plists, with special
  ;; code behind the scenes to handle plist keys that are not in
  ;; actual plists.
  :type '(radio (const :tag "Buffer order" nil)
                (function-item :tag "Latest timestamp" helm-org-rifle-transformer-sort-by-latest-timestamp)
                (function :tag "Custom function")))

(defcustom helm-org-rifle-unlinkify-entry-links t
  "Turn Org links in entry text into plain text so they look nicer in Helm buffers.
Just in case this is a performance issue for anyone, it can be disabled."
  :type 'boolean)

(defcustom helm-org-rifle-sort-order-persist nil
  "When non-nil, keep the sort order setting when it is changed by calling a command with a universal prefix."
  :group 'helm-org-rifle :type 'boolean)

(defcustom helm-org-rifle-occur-kill-empty-buffer t
  "Close occur results buffer after last result is deleted."
  :type 'boolean)

(defvar helm-org-rifle-occur-map (let ((map (copy-keymap org-mode-map)))
                                   (define-key map [remap undo] (lambda () (interactive) (let ((inhibit-read-only t)) (undo))))
                                   (define-key map [mouse-1] 'helm-org-rifle-occur-goto-entry)
                                   (define-key map (kbd "<RET>") 'helm-org-rifle-occur-goto-entry)
                                   (define-key map (kbd "d") 'helm-org-rifle-occur-delete-entry)
                                   (define-key map (kbd "b") (lambda () (interactive) (helm-org-rifle--speed-command 'org-backward-heading-same-level)))
                                   (define-key map (kbd "f") (lambda () (interactive) (helm-org-rifle--speed-command 'org-forward-heading-same-level)))
                                   (define-key map (kbd "p") (lambda () (interactive) (helm-org-rifle--speed-command 'outline-previous-visible-heading)))
                                   (define-key map (kbd "n") (lambda () (interactive) (helm-org-rifle--speed-command 'outline-next-visible-heading)))
                                   (define-key map (kbd "u") (lambda () (interactive) (helm-org-rifle--speed-command 'outline-up-heading)))
                                   (define-key map (kbd "o") (lambda () (interactive) (helm-org-rifle--speed-command 'org-open-at-point)))
                                   (define-key map (kbd "c") (lambda () (interactive) (helm-org-rifle--speed-command 'org-cycle)))
                                   (define-key map (kbd "C") (lambda () (interactive) (helm-org-rifle--speed-command 'org-shifttab)))
                                   (define-key map (kbd "q") 'quit-window)
                                   map)
  "Keymap for helm-org-rifle-occur results buffers.  Imitates org-speed keys.")

(defvar helm-org-rifle-occur-minibuffer-map (let ((map (copy-keymap minibuffer-local-map)))
                                              (define-key map (kbd "C-g") 'helm-org-rifle-occur-cleanup-buffer)
                                              map)
  "Keymap for helm-org-rifle-occur minibuffers.")

(defvar helm-org-rifle-occur-last-input nil
  "Last input given, used to avoid re-running search when input hasn't changed.")

(defvar helm-org-rifle-transformer nil
  "Function to transform results, usually for sorting.  Not intended to be user-set at this time.")

;;;; Functions

;;;;; Commands

;;;###autoload
(cl-defmacro helm-org-rifle-define-command (name args docstring &key sources (let nil) (transformer nil))
  "Define interactive helm-org-rifle command, which will run the appropriate hooks.
Helm will be called with vars in LET bound."
  `(cl-defun ,(intern (concat "helm-org-rifle" (when (s-present? name) (concat "-" name)))) ,args
     ,docstring
     (interactive)
     (unwind-protect
         (progn
           (run-hooks 'helm-org-rifle-before-command-hook)
           (let* ((helm-candidate-separator " ")
                  ,(if transformer
                       ;; I wish there were a cleaner way to do this,
                       ;; because if this `if' evaluates to nil, `let' will
                       ;; try to set `nil', which causes an error.  The
                       ;; choices seem to be to a) evaluate to a list and
                       ;; unsplice it (since unsplicing `nil' evaluates to
                       ;; nothing), or b) return an ignored symbol when not
                       ;; true.  Option B is less ugly.
                       `(helm-org-rifle-transformer ,transformer)
                     'ignore)
                  ,@let)
             (helm :sources ,sources)))
       (run-hooks 'helm-org-rifle-after-command-hook))))

;;;###autoload
(helm-org-rifle-define-command
 "" ()
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
 :sources (helm-org-rifle-get-sources-for-open-buffers))

;;;###autoload
(helm-org-rifle-define-command
 "current-buffer" ()
 "Rifle through the current buffer."
 :sources (helm-org-rifle-get-source-for-buffer (current-buffer)))

;;;###autoload
(helm-org-rifle-define-command
 "files" (&optional files)
 "Rifle through FILES, where FILES is a list of paths to Org files.
If FILES is nil, prompt with `helm-read-file-name'.  All FILES
are searched; they are not filtered with
`helm-org-rifle-directories-filename-regexp'."
 :sources (--map (helm-org-rifle-get-source-for-file it) files)
 :let ((files (helm-org-rifle--listify (or files
                                           (helm-read-file-name "Files: " :marked-candidates t))))
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
                                    (kill-buffer (helm-attr 'buffer source))))))))))

;;;###autoload
(helm-org-rifle-define-command
 "sort-by-latest-timestamp" ()
 "Rifle through open buffers, sorted by latest timestamp."
 :transformer 'helm-org-rifle-transformer-sort-by-latest-timestamp
 :sources (helm-org-rifle-get-sources-for-open-buffers))

;;;###autoload
(helm-org-rifle-define-command
 "current-buffer-sort-by-latest-timestamp" ()
 "Rifle through the current buffer, sorted by latest timestamp."
 :transformer 'helm-org-rifle-transformer-sort-by-latest-timestamp
 :sources (helm-org-rifle-get-source-for-buffer (current-buffer)))

;;;###autoload
(defun helm-org-rifle-agenda-files ()
  "Rifle through Org agenda files."
  ;; This does not need to be defined with helm-org-rifle-define-command because it calls helm-org-rifle-files which is.
  (interactive)
  (helm-org-rifle-files org-agenda-files))

;;;###autoload
(defun helm-org-rifle-directories (&optional directories toggle-recursion)
  "Rifle through Org files in DIRECTORIES.
DIRECTORIES may be a string or list of strings.  If DIRECTORIES
is nil, prompt with `helm-read-file-name'.  With prefix or
TOGGLE-RECURSION non-nil, toggle recursion from the default.
Files in DIRECTORIES are filtered using
`helm-org-rifle-directories-filename-regexp'."
  ;; This does not need to be defined with helm-org-rifle-define-command because it calls helm-org-rifle-files which is.
  (interactive)
  (let* ((recursive (if (or toggle-recursion current-prefix-arg)
                        (not helm-org-rifle-directories-recursive)
                      helm-org-rifle-directories-recursive))
         (directories (helm-org-rifle--listify
                       (or directories
                           (-select 'f-dir? (helm-read-file-name "Directories: " :marked-candidates t)))))
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
     (unwind-protect
         (progn
           (run-hooks 'helm-org-rifle-before-command-hook)
           (let (directories-collected files-collected buffers-collected)
             ;; FIXME: If anyone's reading this and can help me clean up this macro a bit, help would be appreciated.
             ,preface  ; Maybe not necessary
             ,(when directories
                ;; Is there a nicer way to do this?
                `(setq directories-collected (append directories-collected (helm-org-rifle--listify ,directories))))
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
                `(setq files-collected (append files-collected (helm-org-rifle--listify ,files))))
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
               (helm-org-rifle-occur-begin buffers-collected))))
       (run-hooks 'helm-org-rifle-after-command-hook))))

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
                                  (helm-org-rifle--get-candidates-in-buffer (helm-attr 'buffer) helm-pattern)))
                  :candidate-transformer helm-org-rifle-transformer
                  :match 'identity
                  :multiline helm-org-rifle-multiline
                  :volatile t
                  :action (helm-make-actions
                           "Show entry" 'helm-org-rifle--show-marked-entries
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

(defun helm-org-rifle--save-results ()
  "Save `helm-org-rifle' result in a `helm-org-rifle-occur' buffer.
In the spirit of `helm-grep-save-results'."
  (interactive)
  (helm-org-rifle--mark-all-candidates)
  (helm-exit-and-execute-action 'helm-org-rifle--show-candidates))

(defun helm-org-rifle--mark-all-candidates ()
  "Mark all candidates in Helm buffer.
`helm-mark-all' only marks in the current source, not all
sources, so we do it ourselves."
  ;; Based on `helm-mark-all'
  (with-helm-window
    (let ((follow (if (helm-follow-mode-p (helm-get-current-source)) 1 -1)))
      (helm-follow-mode -1)  ; Disable follow so we don't jump to every candidate
      (save-excursion
        (goto-char (point-min))
        ;; Mark first candidate
        (forward-line 1)  ; Skip header line
        (helm-mark-current-line)
        (helm-make-visible-mark)
        (while (ignore-errors (goto-char (next-single-property-change (point) 'helm-candidate-separator)))
          ;; Mark rest of candidates
          (forward-line 1)
          (helm-mark-current-line)
          (helm-make-visible-mark)))
      (helm-follow-mode follow))))

(defun helm-org-rifle--show-candidates (&optional candidates)
  "Show CANDIDATES (or, if nil, all candidates marked in Helm).
If one candidate is given, the default
`helm-org-rifle-show-entry-function' will be used.  If multiple
candidates, `helm-org-rifle--show-entries-as-occur' will be
used."
  (let ((candidates (or (helm-org-rifle--get-marked-candidates)
                        candidates)))
    (pcase (safe-length candidates)
      (1 (helm-org-rifle-show-entry candidates))
      (2 (helm-org-rifle--show-entries-as-occur candidates)))))

(defun helm-org-rifle--get-marked-candidates ()
  "Return list of all marked candidates in Helm.
`helm-marked-candidates' only returns results from the current
source, so we must gather them manually."
  ;; Based on `helm-revive-visible-mark'
  (with-current-buffer helm-buffer
    (save-excursion
      (cl-loop for o in helm-visible-mark-overlays
               collect (overlay-get o 'real) into res
               finally return (nreverse res)))))

(defun helm-org-rifle-show-entry (candidate)
  "Show CANDIDATE using the default function."
  (funcall helm-org-rifle-show-entry-function candidate))

(defun helm-org-rifle-show-entry-in-real-buffer (candidate)
  "Show CANDIDATE in its real buffer."
  (helm-attrset 'new-buffer nil)  ; Prevent the buffer from being cleaned up
  (-let (((buffer . pos) candidate))
    (switch-to-buffer buffer)
    (goto-char pos))
  (org-show-entry))

(defun helm-org-rifle-show-entry-in-indirect-buffer (candidate)
  "Show CANDIDATE in an indirect buffer."
  (-let (((buffer . pos) candidate)
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

(defun helm-org-rifle--get-candidates-in-buffer (buffer input)
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
                   (path (when (or helm-org-rifle-show-path
                                   negations)
                           (org-get-outline-path)))
                   (priority (when (nth 3 components)
                               ;; TODO: Is there a better way to do this?  The
                               ;; s-join leaves an extra space when there's no
                               ;; priority.
                               (concat "[#" (char-to-string (nth 3 components)) "]")))
                   (tags (nth 5 components))
                   (heading (s-trim (if helm-org-rifle-show-todo-keywords
                                        (s-join " " (list (nth 2 components) priority (nth 4 components)))
                                      (nth 4 components))))
                   (node-end (save-match-data  ; This is confusing; should these be reversed here?  Does it matter?
                               (save-excursion
                                 (outline-next-heading)
                                 (point))))
                   (buffer-name (buffer-name buffer))
                   matching-positions-in-node
                   matching-lines-in-node
                   matched-words-with-context)

              ;; Check negations
              (when negations
                ;; TODO: Maybe match against a heading's inherited tags, if it's not too slow.
                (when (or (cl-loop for elem in path
                                   thereis (string-match-p negations-re elem))
                          ;; FIXME: Doesn't quite match properly with
                          ;; special chars, e.g. negating "!scratch"
                          ;; properly excludes the "*scratch*" buffer,
                          ;; but negating "!*scratch*" doesn't'.
                          (string-match negations-re buffer-name)
                          (re-search-forward negations-re node-end t))
                  (throw 'negated (goto-char node-end))))

              ;; Get beginning-of-line positions for matching lines in node
              (setq matching-positions-in-node
                    (cl-loop initially (goto-char node-beg)
                             while (re-search-forward positive-re node-end t)
                             collect (line-beginning-position) into result
                             do (end-of-line)
                             finally return (sort (delete-dups result) '<)))

              ;; Get list of line-strings containing any token
              (setq matching-lines-in-node
                    (cl-loop with string
                             for pos in matching-positions-in-node
                             do (goto-char pos)
                             unless (org-at-heading-p) ; Leave headings out of list of matched lines
                             ;; Get text of each matching line
                             ;; (DISPLAY . REAL) format for Helm
                             collect `(,(buffer-substring-no-properties (line-beginning-position)
                                                                        (line-end-position))
                                       . (,buffer ,pos))))

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
                                     do (when helm-org-rifle-unlinkify-entry-links
                                          (setq line (helm-org-rifle-replace-links-in-string line)))
                                     append (cl-loop with end
                                                     for match = (string-match context-re line end)
                                                     while match
                                                     do (setq end (match-end 0))
                                                     and collect (s-trim (match-string-no-properties 0 line))))
                            (when (> helm-org-rifle-always-show-entry-contents-chars 0)
                              ;; No match in entry text; add desired entry text
                              (list (s-truncate helm-org-rifle-always-show-entry-contents-chars (helm-org-rifle--get-entry-text buffer node-beg)))))))

                ;; Return list in format: (text-for-display node-beg)
                (let* ((heading (if path
                                    (if helm-org-rifle-fontify-headings
                                        (concat (org-format-outline-path
                                                 ;; Replace links in path elements with plain text, otherwise
                                                 ;; they will be truncated by `org-format-outline-path' and only
                                                 ;; show part of the URL
                                                 (-map 'helm-org-rifle-replace-links-in-string (append path (list heading))))
                                                (if tags
                                                    (concat " " (helm-org-rifle-fontify-like-in-org-mode tags))
                                                  ""))
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
                                  (s-join helm-org-rifle-heading-contents-separator (list heading (org-get-entry)))
                                ;; Show context strings
                                (s-join helm-org-rifle-heading-contents-separator (list heading (s-join helm-org-rifle-ellipsis-string matched-words-with-context))))))
                  (push (cons entry (cons buffer node-beg))
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
        (results-buffer (helm-org-rifle--occur-prepare-results-buffer))
        helm-org-rifle-occur-last-input
        timer)
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

(defun helm-org-rifle--occur-prepare-results-buffer ()
  "Prepare and return results buffer."
  (let ((buffer (get-buffer-create helm-org-rifle-occur-results-buffer-name))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (unless (eq major-mode 'org-mode)
        (read-only-mode)
        (visual-line-mode)
        (org-mode)
        (hi-lock-mode 1)
        (use-local-map helm-org-rifle-occur-map))
      (erase-buffer)
      (pop-to-buffer buffer))
    buffer))

(defun helm-org-rifle-occur-process-input (input source-buffers results-buffer)
  "Find results in SOURCE-BUFFERS for INPUT and insert into RESULTS-BUFFER."
  (when (and (not (s-blank-str? input))
             (not (string= input helm-org-rifle-occur-last-input))
             ;; Very short input strings can return so many results
             ;; that Emacs hangs for a long time.  Let's try a minimum
             ;; size of 3.
             (>= (length input) 3))
    (setq helm-org-rifle-occur-last-input input)
    (let ((inhibit-read-only t)
          (results-by-buffer (cl-loop for source-buffer in source-buffers
                                      collect (list :buffer source-buffer
                                                    :results (helm-org-rifle-occur-get-results-in-buffer source-buffer input)))))
      (when (eq helm-org-rifle-transformer 'helm-org-rifle-transformer-sort-by-latest-timestamp)
        ;; FIXME: Ugly hack.  Need to refactor a consistent way to set sorting and transformers.
        (setq results-by-buffer (cl-loop for results-list in results-by-buffer
                                         collect (-let (((plist &as :buffer buffer :results results) results-list))
                                                   (list :buffer buffer
                                                         :results (with-current-buffer buffer
                                                                    (->> results
                                                                         (helm-org-rifle-add-timestamps-to-nodes)
                                                                         (helm-org-rifle-sort-nodes-by-latest-timestamp))))))))
      (with-current-buffer results-buffer
        (erase-buffer)
        (cl-loop for results-list in results-by-buffer
                 do (-let (((&plist :buffer buffer :results results) results-list))
                      (when results
                        ;; FIXME: The source headers don't play nice
                        ;; with Org mode, since Org considers them
                        ;; part of the parent heading's entry, so they
                        ;; get hidden when the parent heading is
                        ;; collapsed.  Disabling for now.  See TODO
                        ;; item in readme for ideas on fixing.
                        ;; (helm-org-rifle-insert-source-header (buffer-name buffer))
                        (cl-loop for entry in results
                                 do (-let (((plist &as :text text . rest) entry))
                                      (add-text-properties 0 (length text) rest text)
                                      (insert text)
                                      (insert "\n\n"))))))
        (helm-org-rifle-occur-highlight-matches-in-buffer results-buffer input)))))

(defun helm-org-rifle--show-entries-as-occur (entries)
  (let ((inhibit-read-only t))
    (with-current-buffer (helm-org-rifle--occur-prepare-results-buffer)
      (erase-buffer)
      (cl-loop for (buffer . node-beg) in entries
               for text = (helm-org-rifle--get-entry-text buffer node-beg :include-heading t :full-path helm-org-rifle-show-path)
               do (progn (add-text-properties 0 (length text) (list :buffer buffer :node-beg node-beg) text)
                         (insert text)
                         (insert "\n\n")))
      (helm-org-rifle-occur-highlight-matches-in-buffer (current-buffer) helm-input))))

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
  (cl-loop for (text pos) in (helm-org-rifle--get-candidates-in-buffer buffer input)
           collect (list :text text :buffer buffer :node-beg pos)))

(defun helm-org-rifle-occur-goto-entry ()
  "Go to node in source buffer that point in occur buffer is in."
  (interactive)
  (-let* ((properties (text-properties-at (point)))
          ((&plist :buffer buffer :node-beg node-beg) properties)
          ;; FIXME: Get offset of point in node (not sure why +2 is necessary but it works)...or does it?
          (offset (+ 2 (- (point) (or (previous-single-property-change (point) :node-beg)
                                      (point-min))))))
    (pop-to-buffer buffer)
    (goto-char (+ node-beg offset))
    (org-show-entry)))

(defun helm-org-rifle-occur-delete-entry ()
  "Remove entry at point from results buffer.
This helps the user remove unwanted results from the buffer."
  (interactive)
  ;; TODO: Test this more thoroughly.

  (with-current-buffer (get-buffer helm-org-rifle-occur-results-buffer-name)
    ;; Setting current buffer may be unnecessary, but good to be sure
    ;; that we never modify any other buffer by accident (i.e. future,
    ;; unintentional bugs, which never happen, of course...)
    (let ((entry-beg-pos (or (previous-single-property-change (point) :node-beg)
                             (point-min)))
          (entry-end-pos (or (next-single-property-change (point) :node-beg)
                             (point-max)))
          (next-heading-pos (save-excursion
                              (outline-next-heading)
                              (point)))
          (previous-heading-pos (save-excursion
                                  (outline-previous-heading)
                                  (point)))
          (inhibit-read-only t))
      ;; Headings in the results buffer are inserted without respect
      ;; to heading level.  This means that one result which has a
      ;; lower (i.e. higher numbered) heading level may appear to be
      ;; structurally a child node of a previously listed result that
      ;; has a higher (i.e. lower numbered) heading level.  Therefore,
      ;; using org-cut-subtree could cross the boundary between
      ;; results, removing multiple results that appear to be part of
      ;; the same subtree, even though they are not part of the same
      ;; subtree in their source buffers.  So we find the boundary
      ;; between results and only use org-cut-subtree when
      ;; appropriate; otherwise we remove the result text directly.
      ;; This way, the user can remove a sibling node that is part of
      ;; the same search result; but if he removes those siblings'
      ;; parent heading, it will remove them as well.  The end result
      ;; is that when this function is called, it removes the smallest
      ;; coherent part of the results buffer.
      (if (and (>= previous-heading-pos entry-beg-pos)
               (>= entry-end-pos next-heading-pos))
          ;; Previous displayed heading is part of same entry; remove only current subtree
          (org-cut-subtree)
        ;; Previous displayed heading is different entry; remove entry directly
        (delete-region entry-beg-pos entry-end-pos))
      (when (and helm-org-rifle-occur-kill-empty-buffer
                 (= (point-min) (point-max)))
        (kill-this-buffer)))))

;;;;; Timestamp functions

(defun helm-org-rifle-timestamps-in-node (&optional node-start node-end)
  "Return list of Org timestamp objects in node that begins at NODE-START or current point.
Objects are those provided by `org-element-timestamp-parser'."
  (save-excursion
    (goto-char (or node-start (org-entry-beginning-position)))
    (let ((node-end (or node-end (org-entry-end-position))))
      (cl-loop for ts-start = (cdr (org-element-timestamp-successor))
               while (and ts-start (< ts-start node-end))
               collect (progn
                         (goto-char ts-start)
                         (org-element-timestamp-parser))
               into result
               do (goto-char (plist-get (cadar (last result)) :end))
               finally return result))))

(defun helm-org-rifle-add-timestamps-to-nodes (nodes)
  "Add `:timestamps' and `:timestamp-floats' to NODES.
NODES is a list of plists as returned by `helm-org-rifle-transform-candidates-to-list-of-nodes'."
  (->> nodes
       ;; Add timestamp objects
       (--map (plist-put it :timestamps (helm-org-rifle-timestamps-in-node (plist-get it :node-beg))))
       ;; Add float-converted timestamps
       (-map (lambda (node)
               (let ((timestamps (cdar (plist-get node :timestamps))))
                 (plist-put node
                            :timestamp-floats (if timestamps
                                                  (--map (org-time-string-to-seconds (plist-get it :raw-value))
                                                         timestamps)
                                                (list 0))))))))

(defun helm-org-rifle-sort-nodes-by-latest-timestamp (nodes)
  "Sort list of node plists by latest timestamp in each node."
  (sort nodes
        (lambda (a b)
          (> (seq-max (plist-get a :timestamp-floats))
             (seq-max (plist-get b :timestamp-floats))))))

(defun helm-org-rifle-transformer-sort-by-latest-timestamp (candidates)
  "Sort CANDIDATES by latest timestamp in each candidate in SOURCE."
  (with-current-buffer (helm-attr 'buffer) ; This is necessary or it will try to use the "*helm*" buffer instead of the source.
    ;; FIXME: This caused a lot of hair-pulling when adding the occur
    ;; code, because the occur code doesn't use this transformer and
    ;; so wasn't running the timestamp-getting function in the right
    ;; buffer--it was running it in the minibuffer.  It would be good
    ;; to make them use a common format so they could always use the
    ;; transformer, but that wouldn't be as good for performance,
    ;; because then the transformer would ALWAYS have to run.  Maybe
    ;; it's worth it...
    (->> candidates
         (helm-org-rifle-transform-candidates-to-list-of-nodes)
         (helm-org-rifle-add-timestamps-to-nodes)
         (helm-org-rifle-sort-nodes-by-latest-timestamp)
         (helm-org-rifle-transform-list-of-nodes-to-candidates))))

;;;;; Support functions

(defun helm-org-rifle--listify (item)
  "If ITEM is an atom, return (list ITEM).  If ITEM is a list, return ITEM."
  (cl-typecase item
    (list item)
    (atom (list item))
    (otherwise (error "Not an atom or list: %s" item))))

(cl-defun helm-org-rifle--get-entry-text (buffer node-beg &key include-heading full-path)
  "Return Org entry text from node in BUFFER starting at NODE-BEG, skipping drawers.
If INCLUDE-HEADING is non-nil, heading line will be included.  If
FULL-PATH is non-nil, the full path to the heading will be
included, with filename prefix.  Whitespace in text will be
trimmed."
  ;; Modeled after `org-agenda-get-some-entry-text'
  (let (text)
    (with-current-buffer buffer
      ;; Get raw entry text
      (org-with-wide-buffer
       (goto-char node-beg)
       (unless (and include-heading
                    (not full-path))
         ;; If including the heading but not the full path, just get the heading line here
         (end-of-line 1))               ; Skip heading
       (setq text (buffer-substring
                   (point)
                   (or (save-excursion (outline-next-heading) (point))
                       (point-max))))
       (when (and full-path include-heading) ; If only `full-path' is specified, it's probably a mistake, but we'll check anyway
         ;; If not full path; just use the already-captured heading line in `text'
         (let* ((heading-text (nth 4 (org-heading-components)))
                (path (org-get-outline-path))
                (path-string (s-join "/" (append path
                                                 ;; If path is nil, heading-text must be a list for append
                                                 (list heading-text)))))
           ;; TODO: Unfortunately this cannot preserve the outline
           ;; path formatting (i.e. the different `org-level' faces),
           ;; because we're inserting the entry as raw text into the
           ;; result buffer, and since the result buffer is in
           ;; org-mode, it will reformat the whole heading line as a
           ;; top-level heading.  I guess this could be fixed with
           ;; overlays...but that is a project for another day.
           (setq text (concat "* " path-string "\n" text))))))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward org-drawer-regexp nil t)
        ;; Remove drawers
        (delete-region (match-beginning 0)
                       (progn (re-search-forward
                               "^[ \t]*:END:.*\n?" nil 'move)
                              (point))))
      ;; Return entry
      ;; NOTE: The order of these probably doesn't matter, but in case
      ;; it ever became a performance issue, might be worth fiddling
      ;; with.
      (s-trim
       (buffer-substring (point-min) (point-max))))))

(defun helm-org-rifle--speed-command (command)
  "Call COMMAND with `org-speed-move-safe', ignoring any errors."
  (ignore-errors (org-speed-move-safe command)))

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
                                (lambda (text) (or (match-string-no-properties 3 text)
                                                   (match-string-no-properties 1 text)))
                                string t t)
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

(defun helm-org-rifle-set-sort-mode ()
  "Set sorting mode by setting `helm-org-rifle-sort-order' if prefix given."
  (cond ((equal '(4) current-prefix-arg)
         ;; C-u; set sorting mode
         (setq helm-org-rifle-sort-order (helm-org-rifle-prompt-for-sort-mode))))
  (setq helm-org-rifle-transformer helm-org-rifle-sort-order))

(defun helm-org-rifle-reset-sort-mode ()
  "When `helm-org-rifle-sort-order-persist' is nil, reset sort order to the saved value."
  ;; FIXME: This is inelegant, to say the least, but using hooks to do
  ;; this means that a command can't simply be called inside a `let',
  ;; so the value has to be set and then reset.  A dispatcher function
  ;; which calls all commands might be a better way to do this.
  (unless helm-org-rifle-sort-order-persist
    (custom-reevaluate-setting 'helm-org-rifle-sort-order)))

(defun helm-org-rifle-prompt-for-sort-mode ()
  "Ask the user which sorting method to use.
Return sorting function corresponding to chosen description string."
  ;; This mess is required because of what seems like a bug or
  ;; inconsistency in the Emacs customize system.  See comments in the
  ;; defcustom.
  (let* ((choices (--remove (stringp (-last-item it)) ; Filter empty "Custom function"
                            (cdr (get 'helm-org-rifle-sort-order 'custom-type))))
         (choice-tag (helm-comp-read "Sort by: " (--map (third it)
                                                        choices))))
    (cl-loop for choice in choices
             when (string= (third choice) choice-tag)
             return (-last-item choice))))

(defun helm-org-rifle-transform-candidates-to-list-of-nodes (candidates)
  "Transform Helm-style CANDIDATES list to list of plists."
  (--map (list :node-beg (cadr it)
               :text (car it))
         candidates))

(defun helm-org-rifle-transform-list-of-nodes-to-candidates (nodes)
  "Transform list of node plists to Helm-style candidates."
  (--map (list (plist-get it :text)
               (plist-get it :node-beg))
         nodes))

(provide 'helm-org-rifle)

;;; helm-org-rifle.el ends here

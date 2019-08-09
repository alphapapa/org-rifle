;;; org-rifle.el --- Rifle through your Org files

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-rifle
;; Version: 2.0-pre
;; Package-Requires: ((emacs "24.4") (dash "2.12") (f "0.18.1") (s "1.10.0"))
;; Keywords: hypermedia, outlines

;;; Commentary:

;; This is my rifle.  There are many like it, but this one is mine.
;; My rifle is my best friend.  It is my life.  I must master it as I
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

;; Install packages `dash', `f', and `s'.  Then require
;; this package in your init file:

;;   (require 'org-rifle)

;;; Usage:

;; Run one of the rifle commands, type some words, and results will be
;; displayed, grouped by buffer.  Hit "RET" to show the selected
;; entry, or <C-return> to show it in an indirect buffer.

;; Occur commands: show results in an occur-like, persistent buffer
;; + `org-rifle': Show results from all open Org buffers
;; + `org-rifle-agenda-files': Show results from Org agenda files
;; + `org-rifle-current-buffer': Show results from current buffer
;; + `org-rifle-directories': Show results from selected directories; with prefix, recursively
;; + `org-rifle-files': Show results from selected files
;; + `org-rifle-org-directory': Show results from Org files in `org-directory'

;;;; Tips

;; FIXME: Update tips for org-rifle refactor.

;; + Select multiple entries in the Helm buffer to display selected
;;   entries in a read-only, `occur`-style buffer.
;; + Save all results in a Helm buffer to a `org-rifle`
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
;; + The keymap for `org-rifle` results buffers imitates
;;   the `org-speed` keys, making it quicker to navigate. You can also
;;   collapse and expand headings and drawers with `TAB` and `S-TAB`,
;;   just like in regular Org buffers.  Results buffers are marked
;;   read-only so you cannot modify them by accidental keypresses.
;; + Delete the result at point in `org-rifle` buffers by
;;   pressing `d`.  This does not alter the source buffers but simply
;;   removes uninteresting results from view.
;; +  You can customize the `org-rifle` group if you like.

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

;; Built-in
(require 'cl-lib)
(require 'org)

;; Third-party
(require 'dash)
(require 'f)
(require 's)

;;;; Vars

(defconst org-rifle-fontify-buffer-name " *org-rifle-fontify*"
  "The name of the invisible buffer used to fontify `org-mode' strings.")

(defconst org-rifle-results-buffer-name "*org-rifle-results*"
  "The name of the results buffer for `org-rifle' commands.")

(defconst org-rifle-tags-re "\\(?:[ \t]+\\(:[[:alnum:]_@#%%:]+:\\)\\)?"
  "Regexp used to match Org tag strings.  From org.el.")

(defgroup org-rifle nil
  "Settings for `org-rifle'."
  :group 'org
  :link '(url-link "https://github.com/alphapapa/org-rifle"))

(defcustom org-rifle-close-unopened-file-buffers t
  "Close buffers that were not already open.
After rifling through Org files that are not already open, close
the buffers if non-nil.  If nil, leave the buffers open.  Leaving
them open will speed up subsequent searches but clutter the
buffer list."
  :type 'boolean)

(defcustom org-rifle-unlinkify-entry-links t
  "Unlinkify Org links in entry text text.
This makes them look nicer in results buffers, because it avoids
truncating the link syntax.  In case this is a performance issue,
it can be disabled."
  ;; TODO: Make this an org-rifle option.
  :type 'boolean)

(defcustom org-rifle-directories-recursive t
  "Recurse into subdirectories by default in `org-rifle-directories'.
When `org-rifle-directories' is called with a prefix, this
option will be inverted."
  :type 'boolean)

(defcustom org-rifle-directories-filename-regexp "\.org$"
  "Regular expression to match Org filenames in `org-rifle-directories'.
Files matching this regexp will be searched.  By default, \".org\" files are matched, but you may also select to include \".org_archive\" files, or use a custom regexp."
  :type '(radio (string :tag "Normal \".org\" files" :value "\.org$")
                (string :tag "Also include \".org_archive\" files" "\.org\\(_archive\\)?$")
                (string :tag "Custom regexp.  You know what you're doing.")))

(defcustom org-rifle-fontify-headings t
  "Fontify Org headings.

For large result sets this may be slow, although it doesn't seem
to be a major bottleneck."
  :type 'boolean)

(defcustom org-rifle-show-todo-keywords t
  "Show and match against Org todo keywords."
  :type 'boolean)

(defcustom org-rifle-show-path t
  "Show the whole heading path instead of just the entry's heading."
  :type 'boolean)

(defcustom org-rifle-reverse-paths nil
  "When showing outline paths, show them in reverse order.
For example, with this outline tree:

* Computers
** Software
*** Emacs
**** Development
***** Libraries
****** HTML-related
******* elfeed

The path of that entry would normally be displayed as:

Computers/Software/Emacs/Development/Libraries/HTML-related/elfeed

And that might be fine.  But if there were a lot of other matches
around that place in the tree, seeing the first several elements
of the paths repeated might make the results appear more similar
than they are, and when the path is long, the most unique
part (the last element) might be obscured by wrapping (depending
on Org faces).  So it might be better to show the paths reversed,
like:

elfeed\\HTML-related\\Libraries\\Development\\Emacs\\Software\\Computers

Note that the separator is also \"reversed\" to indicate that the
paths are reversed.  Also, when `org-rifle-fontify-headings'
is enabled, the fontification typically makes it obvious that the
paths are reversed, depending on your Org faces."
  :type 'boolean)

(defcustom org-rifle-re-prefix
  "\\(\\_<\\|[[:punct:]]\\)"
  "Regexp matched immediately before each search term.
\(Customize at your peril (but it's okay to experiment here,
because you can always revert your changes).)"
  :type 'regexp)

(defcustom org-rifle-re-suffix
  "\\(\\_>\\|[[:punct:]]\\)"
  "Regexp matched immediately after each search term.
\(What, didn't you read the last warning?  Oh, nevermind.)"
  :type 'regexp)

(defcustom org-rifle-sort-order nil
  "Sort results in this order by default.
The sort order may be changed temporarily by calling a command
with a universal prefix.

This is a list of functions which may be called to transform
results, typically by sorting them."
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
                (function-item :tag "Latest timestamp" org-rifle-transformer-sort-by-latest-timestamp)
                (function :tag "Custom function")))

(defcustom org-rifle-sort-order-persist nil
  "When non-nil, keep the sort order setting when it is changed by calling a command with a universal prefix."
  :type 'boolean)

(defcustom org-rifle-test-against-path nil
  "Test search terms against entries' outline paths.
When non-nil, search terms will be tested against each element of
each entry's outline path.  This requires looking up the outline
path of every entry that might be a match.  This significantly
slows down searching, so it is disabled by default, and most
users will probably prefer to leave it disabled.  Also, when it
is disabled, the outline paths of matching entries can still be
displayed (if `org-rifle-show-path' is enabled), without the
performance penalty of looking up the outline paths of even
non-matching entries.

However, enabling it may provide more comprehensive results.  For
example, consider the following outline:

* Emacs
** Org-mode
** Tips
*** Foo
Bar baz.

If the search terms were \"Org foo\", and this option were
disabled, the entry \"Bar baz\" would not be found, because the
term \"Org\" would not be tested against the path element
\"Org-mode\".  With this option enabled, the entry would be
found, because \"Org\" is part of the entry's outline path.

In comparison, consider this outline:

* Emacs
** Org-mode
** Tips
*** Foo
Bar baz Org-mode.

With the same search terms and this option disabled, the entry
\"Bar baz Org-mode\" would be found, because it contains \"Org\".

Perhaps a helpful way to think about this option is to think of
it as a search engine testing against a Web page's URL.  When
disabled, search terms must be contained in pages' contents.
When enabled, the URL itself is considered as part of pages'
contents.  Of course, one would usually want to leave it enabled
for a Web search--but imagine that looking up pages' URLs
required an additional, very slow database query: it might be
better to leave it disabled by default."
  :type 'boolean)

(defcustom org-rifle-always-test-excludes-against-path t
  "Always test excluded terms against entries' outline paths.
Similarly to `org-rifle-test-against-path', this option may
cause a significant performance penalty.  However, unlike that
option, this one only takes effect when exclude patterns are
used (ones starting with \"!\") to negate potential matches.  It
is probably more important to check excluded terms against all
possible parts of an entry, and excluded terms are not used most
of the time, so this option is enabled by default, in the hope
that it will provide the most useful behavior by default.

Consider this outline:

* Food
** Fruits
*** Strawberry
Sweet and red in color.
** Vegetables
*** Chili pepper
Spicy and red in color.

If the search terms were \"red !fruit\", and this option were
enabled, the entry \"Strawberry\" would be excluded, because the
word \"Fruit\" is in its outline path.  But if this option were
disabled, the entry would be included, because its outline path
would be ignored."
  :type 'boolean)

(defcustom org-rifle-kill-empty-buffer t
  "Close results buffer after last result is deleted."
  :type 'boolean)

(defvar org-rifle-map
  (let ((map (copy-keymap org-mode-map)))
    (define-key map [remap org-cycle] 'org-rifle--org-cycle)
    (define-key map [remap undo] (lambda () (interactive) (let ((inhibit-read-only t)) (undo))))
    (define-key map [mouse-1] 'org-rifle-goto-entry)
    (define-key map (kbd "<RET>") 'org-rifle-goto-entry)
    (define-key map (kbd "C-c C-w") #'org-rifle--refile)
    (define-key map (kbd "d") 'org-rifle-delete-entry)
    (define-key map (kbd "b") (lambda () (interactive) (org-rifle--speed-command 'org-backward-heading-same-level)))
    (define-key map (kbd "f") (lambda () (interactive) (org-rifle--speed-command 'org-forward-heading-same-level)))
    (define-key map (kbd "p") (lambda () (interactive) (org-rifle--speed-command 'outline-previous-visible-heading)))
    (define-key map (kbd "n") (lambda () (interactive) (org-rifle--speed-command 'outline-next-visible-heading)))
    (define-key map (kbd "u") (lambda () (interactive) (org-rifle--speed-command 'outline-up-heading)))
    (define-key map (kbd "o") (lambda () (interactive) (org-rifle--speed-command 'org-open-at-point)))
    (define-key map (kbd "c") (lambda () (interactive) (org-rifle--speed-command 'org-cycle)))
    (define-key map (kbd "C") (lambda () (interactive) (org-rifle--speed-command 'org-shifttab)))
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for `org-rifle' results buffers.  Imitates `org-speed' keys.")

(defvar org-rifle-minibuffer-map (let ((map (copy-keymap minibuffer-local-map)))
                                   (define-key map (kbd "C-g") 'org-rifle-cleanup-buffer)
                                   map)
  "Keymap for `org-rifle' mini buffers.")

(defvar org-rifle-last-input nil
  "Last input given, used to avoid re-running search when input hasn't changed.")

(defvar org-rifle-separator
  (let ((text "\n"))
    (set-text-properties 0 (length text) '(org-rifle-result-separator t font-lock-face org-rifle-separator) text)
    text)
  "Propertized separator for results.")

(defvar org-rifle-transformer nil
  "Function to transform results, usually for sorting.  Not intended to be user-set at this time.")

(defcustom org-rifle-always-show-entry-contents-chars 50
  "When non-zero, always show this many characters of entry text, even if none of it matches query."
  ;; TODO: Make this an org-rifle option.
  :type 'integer)

(defcustom org-rifle-heading-contents-separator "\n"
  "Separator inserted between entry's heading and contents.
Usually this should be a newline, but it may be useful to adjust
it when defining custom commands.  For example, by setting this
to a non-newline value and disabling `org-rifle-multiline',
each result can be displayed on a single line."
  :type 'string)

(defcustom org-rifle-before-command-hook '(org-rifle-set-sort-mode)
  "Hook that runs before each `org-rifle' command."
  ;; TODO: Sort these defs.
  :type 'hook)

(defcustom org-rifle-after-command-hook '(org-rifle-reset-sort-mode)
  "Hook that runs after each `org-rifle' command."
  ;; TODO: Should this be an org-rifle setting?
  :type 'hook)

(defcustom org-rifle-context-characters 25
  "How many characters around each matched term to display."
  ;; TODO: Make this an org-rifle option.
  :type 'integer)

(defcustom org-rifle-ellipsis-string "..."
  "Shown between match context strings."
  ;; TODO: Make this an org-rifle option.
  :type 'string)

(defcustom org-rifle-ellipsis-face 'font-lock-comment-delimiter-face
  "Face for ellipses between match context strings."
  ;; TODO: Make this an org-rifle option.
  :type 'face)

;;;; Functions

;;;;; Commands

(cl-defmacro org-rifle-defcommand (name args docstring &key buffers files directories preface)
  "Define `org-rifle' command to search BUFFERS."
  `(defun ,(intern (concat "org-rifle"
                           (when name (concat "-" name))))
       ,args
     ,docstring
     (interactive)
     (unwind-protect
         (progn
           (run-hooks 'org-rifle-before-command-hook)
           (let (directories-collected files-collected buffers-collected)
             ;; FIXME: If anyone's reading this and can help me clean up this macro a bit, help would be appreciated.
             ,preface                   ; Maybe not necessary
             ,(when directories
                ;; Is there a nicer way to do this?
                `(setq directories-collected (append directories-collected (org-rifle--listify ,directories))))
             (when directories-collected
               (let ((recursive (if current-prefix-arg
                                    (not org-rifle-directories-recursive)
                                  org-rifle-directories-recursive)))
                 (setq files-collected (append files-collected
                                               (-flatten
                                                (--map (f-files it
                                                                (lambda (file)
                                                                  (s-matches? org-rifle-directories-filename-regexp
                                                                              (f-filename file)))
                                                                recursive)
                                                       directories-collected))))))
             ,(when files
                ;; Is there a nicer way to do this?
                `(setq files-collected (append files-collected (org-rifle--listify ,files))))
             (when files-collected
               (setq buffers-collected (append (cl-loop for file in files-collected
                                                        collect (-if-let (buffer (org-find-base-buffer-visiting file))
                                                                    buffer
                                                                  (find-file-noselect file)))
                                               buffers-collected)))
             ,(when buffers
                ;; Is there a nicer way to do this?
                `(setq buffers-collected (append buffers-collected ,buffers)))
             (let ((org-rifle-show-full-contents t))
               (org-rifle-begin buffers-collected))))
       (run-hooks 'org-rifle-after-command-hook))))

;; FIXME: All these autoloads.

;;;###autoload (autoload 'org-rifle "helm-org-rifle" nil t)
(org-rifle-defcommand
 nil ()
 "Search all Org buffers."
 :buffers (--remove (string= org-rifle-results-buffer-name (buffer-name it))
                    (-select #'org-rifle-buffer-visible-p
                             (org-buffer-list nil t))))

;;;###autoload (autoload 'org-rifle-current-buffer "helm-org-rifle" nil t)
(org-rifle-defcommand
 "current-buffer" ()
 "Search current Org buffer."
 :buffers (list (current-buffer)))

;;;###autoload (autoload 'org-rifle-directories "helm-org-rifle" nil t)
(org-rifle-defcommand
 "directories" (&optional directories)
 "Search Org files in DIRECTORIES.
Files are opened if necessary, and the resulting buffers are left open."
 :directories (or directories
                  (read-file-name "Directories: " :marked-candidates t)))

;;;###autoload (autoload 'org-rifle-files "helm-org-rifle" nil t)
(org-rifle-defcommand
 "files" (&optional files)
 "Search Org FILES.
Files are opened if necessary, and the resulting buffers are left open."
 :files (or files
            (read-file-name "Files: " :marked-candidates t)))

;;;###autoload (autoload 'org-rifle-agenda-files "helm-org-rifle" nil t)
(org-rifle-defcommand
 "agenda-files" ()
 "Search Org agenda files.
Files are opened if necessary, and the resulting buffers are left open."
 :files (org-agenda-files))

;;;###autoload (autoload 'org-rifle-org-directory "helm-org-rifle" nil t)
(org-rifle-defcommand
 "org-directory" ()
 "Search Org files in `org-directory'.
Files are opened if necessary, and the resulting buffers are left open."
 :directories (list org-directory))

(defun org-rifle--refile (candidate)
  "Refile CANDIDATE."
  ;; This needs to be an interactive command because it's bound in `helm-org-rifle-map'.
  (interactive)
  (-let (((buffer . pos) candidate))
    (with-current-buffer buffer
      (goto-char pos)
      (org-refile))))

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
  "Return list of entry data if entry at point is a match.
This is to be called from `org-rifle--get-candidates-in-buffer',
because it uses variables in its outer scope."
  ;; TODO: Separate matching and entry-data-gathering.
  (-let* ((node-beg (org-entry-beginning-position))
          (node-end (org-entry-end-position))
          ((level reduced-level todo-keyword priority-char heading tags priority) (org-heading-components))
          (path nil)
          (priority (when priority-char
                      ;; TODO: Is there a better way to do this?  The
                      ;; s-join leaves an extra space when there's no
                      ;; priority.
                      (format "[#%c]" priority-char)))
          (todo-keyword (when todo-keyword
                          (if org-rifle-show-todo-keywords
                              (propertize todo-keyword
                                          'face (org-get-todo-face todo-keyword))
                            todo-keyword)))
          (heading (if org-rifle-show-todo-keywords
                       (s-join " " (list priority heading))
                     heading))
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
           ;; NOTE: It would be nice to be able to match against inherited tags, but that would mean
           ;; testing every node in the buffer, rather than using a regexp to go directly to
           ;; potential matches.  It would also essentially require using the org tags cache,
           ;; otherwise it would mean looking up the tree for the inherited tags for every node,
           ;; repeating a lot of work.  So it would mean using a different "mode" of matching for
           ;; queries that include inherited tags.  Maybe that mode could be using the Org Agenda
           ;; searching code (which efficiently caches tags), and reprocessing its results into our
           ;; form and presenting them with Helm.  Or maybe it could be just finding matches for
           ;; inherited tags, and then searching those matches for other keywords.  In that case,
           ;; maybe this function could remain the same, and simply be called from a different
           ;; function than --get-candidates-in-buffer.

           ;; FIXME: Partial excludes seem to put the partially
           ;; negated entry at the end of results.  Not sure why.
           ;; Could it actually be a good feature, though?
           ;; TODO: Collect outline paths recursively in stages to avoid calling `org-get-outline-path' on every node.
           (or (cl-loop for elem in (when (or org-rifle-test-against-path
                                              org-rifle-always-test-excludes-against-path)
                                      (setq path (org-get-outline-path)))
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
                                                        (when org-rifle-test-against-path
                                                          (or path (setq path (org-get-outline-path))))
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
                (if org-rifle-show-path
                    (progn
                      ;; Trim the heading here.  Trying to avoid calling trim too much, to avoid slowing down.
                      (setq heading (s-trim heading))
                      (if org-rifle-fontify-headings
                          (let ((path (if org-rifle-reverse-paths
                                          (--> (org-get-outline-path t)
                                               (org-format-outline-path it 1000 nil "")
                                               (org-split-string it "")
                                               (nreverse it)
                                               (s-join "\\" it)
                                               (org-link-display-format it)
                                               (if (> (length it) (window-width))
                                                   (concat (substring it 0 (- (window-width) 2))
                                                           "..")
                                                 it))
                                        (--> (or path (org-get-outline-path))
                                             (append it (list heading))
                                             (org-format-outline-path it)
                                             (org-link-display-format it))))
                                (tags (if tags
                                          (concat " " (org-rifle-fontify-like-in-org-mode tags))
                                        "")))
                            (when (and org-rifle-show-todo-keywords
                                       todo-keyword)
                              (if org-rifle-reverse-paths
                                  ;; Funnily enough, the "else" block works almost completely
                                  ;; correctly in both cases, but we might as well do it "correctly"
                                  ;; in this case, which should also be slightly faster.
                                  (setq path (concat (propertize todo-keyword 'face (org-get-todo-face todo-keyword))
                                                     " " path))
                                ;; This is kind of ugly, but we don't seem to have much choice.  It
                                ;; seems fast enough, though.
                                (let* ((parts (s-split "/" path))
                                       (last (car (last parts)))
                                       (parts (butlast parts))
                                       (keyword (propertize todo-keyword 'face (org-get-todo-face todo-keyword)))
                                       (last (concat keyword " " last)))
                                  ;; `-snoc' is cool.
                                  (setq path (s-join "/" (-snoc parts last))))))
                            (concat path tags))
                        ;; Not fontifying
                        (s-join "/" (list (or path (org-get-outline-path)) heading))))
                  ;; No path or not showing path
                  (if org-rifle-fontify-headings
                      (org-rifle-fontify-like-in-org-mode
                       (s-join " " (-non-nil
                                    ;; NOTE: Using `-non-nil' isn't essential here, so we might
                                    ;; consider removing it to increase performance, but it means
                                    ;; leaving an extra space between the heading starts and the
                                    ;; heading text when there is no to-do keyword.
                                    (list (s-repeat level "*")
                                          (when (and org-rifle-show-todo-keywords
                                                     todo-keyword)
                                            (propertize todo-keyword
                                                        'face (org-get-todo-face todo-keyword)))
                                          heading
                                          (concat tags " ")))))
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

;;;;; Occur-style

(defun org-rifle--org-cycle (&optional arg)
  "Cycle folding of Org entries in a results buffer.
ARG is passed to `org-cycle'.  This folds first at boundaries
defined by `helm-header' and `org-rifle-result-separator'
text-properties, and then normally, by outline headings."
  ;; FIXME: Shouldn't be using `helm-header' in this.
  (interactive)
  (cl-letf (((symbol-function 'org-end-of-subtree)
             'org-rifle--org-end-of-subtree)
            ((symbol-function 'outline-next-heading)
             'org-rifle--outline-next-heading))
    (if (text-property-any (line-beginning-position) (line-end-position) 'helm-header t)
        ;; On a header line; cycle all entries in this source
        (let ((start (line-beginning-position))
              (end (save-excursion
                     (while (text-property-any (line-beginning-position) (line-end-position) 'helm-header t)
                       (forward-line 1))
                     (forward-char)
                     (let ((char (or (next-single-property-change (point) 'helm-header nil)
                                     (point-max))))
                       (when char
                         (goto-char char)))
                     (point))))
          (cl-loop for char = (next-single-property-change (point) 'org-rifle-result-separator nil)
                   while (and char
                              (< char end))
                   do (progn
                        (goto-char char)
                        (outline-next-heading)
                        (outline-hide-subtree))))
      ;; Cycle this entry normally
      (org-cycle arg))))

(defun org-rifle--outline-next-heading ()
  "Move to the next heading.
Moves to the next header, result separator, outline heading, or
`point-max', whichever is smallest.  This is intended to override
`outline-next-heading' in occur results buffers."
  (interactive)
  (cl-flet ((min (&rest args)
                 (apply 'min (-non-nil args)))
            (next-outline-heading ()
                                  ;; This is basically a copy of `outline-next-heading'
                                  (when (re-search-forward (concat "^\\(?:" outline-regexp "\\)")
                                                           nil 'move)
                                    (match-beginning 0))))
    ;; FIXME: Shouldn't be using `helm-header' here.
    (cond ((or (memq 'helm-header (text-properties-at (point)))
               (memq 'org-rifle-result-separator (text-properties-at (point))))
           ;; At a source header or result separator; go to first outline heading
           (goto-char (next-outline-heading)))

          ((org-at-heading-p)
           ;; At a heading; go to next header or separator or end-of-buffer
           (goto-char (1- (min (next-single-property-change (point) 'org-rifle-result-separator)
                               (next-single-property-change (point) 'helm-header)
                               (point-max)))))

          (t
           (let ((char (min (next-single-property-change (point) 'org-rifle-result-separator)
                            (next-single-property-change (point) 'helm-header)
                            (next-outline-heading))))
             (when char
               (goto-char char)))))))

(defun org-rifle--org-end-of-subtree (&optional invisible-ok to-heading)
  "Goto to the end of a subtree.
Arguments INVISIBLE-OK and TO-HEADING are like in
`org-end-of-subtree'.  This is a copy of `org-end-of-subtree',
but it respects headers and separators."
  ;; This contains an exact copy of the original function, but it uses
  ;; `org-back-to-heading', to make it work also in invisible
  ;; trees.  And is uses an invisible-ok argument.
  ;; Under Emacs this is not needed, but the old outline.el needs this fix.
  ;; Furthermore, when used inside Org, finding the end of a large subtree
  ;; with many children and grandchildren etc, this can be much faster
  ;; than the outline version.
  (org-back-to-heading invisible-ok)
  (let ((first t)
	(level (funcall outline-level)))
    (if (and (derived-mode-p 'org-mode) (< level 1000))
	;; A true heading (not a plain list item), in Org
	;; This means we can easily find the end by looking
	;; only for the right number of stars.  Using a regexp to do
	;; this is so much faster than using a Lisp loop.
        (cl-flet ((min (&rest args)
                       (apply 'min (-non-nil args))))
          (let* ((re (concat "^\\*\\{1," (int-to-string level) "\\} "))
                 (re-pos (save-excursion
                           (forward-char 1)
                           (and (re-search-forward re nil 'move) (beginning-of-line 1))))
                 (char (min (next-single-property-change (point) 'org-rifle-result-separator)
                            ;; FIXME: Shouldn't be using helm-header here.
                            (next-single-property-change (point) 'helm-header)
                            re-pos
                            (point-max))))
            (goto-char char)))
      ;; something else, do it the slow way
      (while (and (not (eobp))
		  (or first (> (funcall outline-level) level)))
	(setq first nil)
	(outline-next-heading)))
    (unless to-heading
      (when (memq (preceding-char) '(?\n ?\^M))
	;; Go to end of line before heading
	(forward-char -1)
	(when (memq (preceding-char) '(?\n ?\^M))
	  ;; leave blank line before heading
	  (forward-char -1)))))
  (point))

(defun org-rifle-begin (source-buffers)
  "Begin occur-style command searching BUFFERS, opening results buffer, focusing minibuffer, and running timer to put results in buffer."
  (let ((inhibit-read-only t)
        ;; I can't figure out why the asterisks are causing the buffer
        ;; to not show up in my Helm buffer list, but it does show up
        ;; in ibuffer.
        (results-buffer (org-rifle--prepare-results-buffer))
        org-rifle-last-input
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
                             (org-rifle-process-input (s-trim (minibuffer-contents)) source-buffers results-buffer)))))
          (read-from-minibuffer "pattern: " nil org-rifle-minibuffer-map nil nil nil nil))
      (when timer (cancel-timer timer) (setq timer nil)))))

(defun org-rifle--prepare-results-buffer ()
  "Prepare and return results buffer."
  (let ((buffer (get-buffer-create org-rifle-results-buffer-name))
        (inhibit-read-only t)
        ;; Prevent source headers from being indented.
        ;; FIXME: This works on my config, but it needs wider testing.
        (org-startup-indented  nil))
    (with-current-buffer buffer
      (unless (eq major-mode 'org-mode)
        (read-only-mode)
        (visual-line-mode)
        (org-mode)
        (hi-lock-mode 1)
        (use-local-map org-rifle-map))
      (erase-buffer)
      (pop-to-buffer buffer))
    buffer))

(defun org-rifle-process-input (input source-buffers results-buffer)
  "Find results in SOURCE-BUFFERS for INPUT and insert into RESULTS-BUFFER."
  (when (and (not (s-blank-str? input))
             (not (string= input org-rifle-last-input))
             ;; Very short input strings can return so many results
             ;; that Emacs hangs for a long time.  Let's try a minimum
             ;; size of 3.
             (>= (length input) 3))
    (setq org-rifle-last-input input)
    (let ((inhibit-read-only t)
          (results-by-buffer (cl-loop for source-buffer in source-buffers
                                      collect (list :buffer source-buffer
                                                    :results (org-rifle-get-results-in-buffer source-buffer input)))))
      (when (eq org-rifle-transformer 'org-rifle-transformer-sort-by-latest-timestamp)
        ;; FIXME: Ugly hack.  Need to refactor a consistent way to set sorting and transformers.
        (setq results-by-buffer (cl-loop for results-list in results-by-buffer
                                         collect (-let (((plist &as :buffer buffer :results results) results-list))
                                                   (list :buffer buffer
                                                         :results (with-current-buffer buffer
                                                                    (->> results
                                                                         (org-rifle-add-timestamps-to-nodes)
                                                                         (org-rifle-sort-nodes-by-latest-timestamp))))))))
      (with-current-buffer results-buffer
        (erase-buffer)
        (cl-loop for results-list in results-by-buffer
                 do (-let (((&plist :buffer buffer :results results) results-list))
                      (when results
                        (org-rifle-insert-source-header (buffer-name buffer))
                        (cl-loop for entry in results
                                 do (-let (((plist &as :text text . rest) entry))
                                      (add-text-properties 0 (length text) rest text)
                                      (insert org-rifle-separator)
                                      (insert text))))))
        (org-rifle-highlight-matches-in-buffer results-buffer input)))))

(defun org-rifle-highlight-matches-in-buffer (buffer input)
  "Highlight matches for INPUT in BUFFER using hi-lock-mode."
  (with-current-buffer buffer
    (unhighlight-regexp t)
    (dolist (token (org-rifle-split-tags-in-input-list (s-split-words input)))
      (highlight-regexp token))))

(defun org-rifle-get-results-in-buffer (buffer input)
  "Return list of results for INPUT in BUFFER.
Results is a list of strings with text-properties :NODE-BEG and :BUFFER."
  (with-current-buffer buffer
    (unless (derived-mode-p 'org-mode)
      (error "Buffer %s is not an Org buffer" buffer)))
  (cl-loop for (text . (_ .  pos)) in (org-rifle--get-candidates-in-buffer buffer input)
           collect (list :text text :buffer buffer :node-beg pos)))

(defun org-rifle-goto-entry ()
  "Go to node in source buffer that point in occur buffer is in."
  (interactive)
  (-let* ((properties (text-properties-at (point)))
          ((&plist :buffer buffer :node-beg node-beg) properties)
          ;; FIXME: Get offset of point in node (not sure why +2 is necessary but it works)...or does it?
          (offset (+ 2 (- (point) (or (previous-single-property-change (point) :node-beg)
                                      (point-min))))))
    (when node-beg
      ;; If node-beg is nil, point is on something like a source
      ;; header, in which case we do nothing
      (pop-to-buffer buffer)
      (goto-char (+ node-beg offset))
      (org-show-entry))))

(defun org-rifle-delete-entry ()
  "Remove entry at point from results buffer.
This helps the user remove unwanted results from the buffer."
  (interactive)
  ;; TODO: Test this more thoroughly.

  ;; FIXME: This needs to be updated to use the source headers and entry separators.

  (with-current-buffer (get-buffer org-rifle-results-buffer-name)
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
      (when (and org-rifle-kill-empty-buffer
                 (= (point-min) (point-max)))
        (kill-this-buffer)))))

;;;;; Timestamp functions

(defun org-rifle-timestamps-in-node (&optional node-start node-end)
  "Return list of Org timestamp objects in node.
Node begins at NODE-START or current point and ends at NODE-END.
Objects are those provided by `org-element-timestamp-parser'."
  ;; TODO: Use ts.el.
  (save-excursion
    (goto-char (or node-start (org-entry-beginning-position)))
    (let ((node-end (or node-end (org-entry-end-position))))
      ;; FIXME: `org-element-timestamp-successor' doesn't exist anymore?
      (cl-loop for ts-start = (cdr (org-element-timestamp-successor))
               while (and ts-start (< ts-start node-end))
               collect (progn
                         (goto-char ts-start)
                         (org-element-timestamp-parser))
               into result
               do (goto-char (plist-get (cadar (last result)) :end))
               finally return result))))

(defun org-rifle-add-timestamps-to-nodes (nodes)
  "Add `:timestamps' and `:timestamp-floats' to NODES.
NODES is a list of plists as returned by `org-rifle-transform-candidates-to-list-of-nodes'."
  (->> nodes
       ;; Add timestamp objects
       (--map (plist-put it :timestamps (org-rifle-timestamps-in-node (plist-get it :node-beg))))
       ;; Add float-converted timestamps
       (-map (lambda (node)
               (let ((timestamps (cdar (plist-get node :timestamps))))
                 (plist-put node
                            :timestamp-floats (if timestamps
                                                  (--map (org-time-string-to-seconds (plist-get it :raw-value))
                                                         timestamps)
                                                (list 0))))))))

(defun org-rifle-sort-nodes-by-latest-timestamp (nodes)
  "Sort NODES by latest timestamp in each node.
NODES is a list of node plists."
  (sort nodes
        (lambda (a b)
          (> (seq-max (plist-get a :timestamp-floats))
             (seq-max (plist-get b :timestamp-floats))))))

;;;;; Support functions

(defun org-rifle--listify (item)
  "If ITEM is an atom, return (list ITEM).  If ITEM is a list, return ITEM."
  (cl-typecase item
    (list item)
    (atom (list item))
    (otherwise (error "Not an atom or list: %s" item))))

(cl-defun org-rifle--get-entry-text (buffer node-beg &key include-heading full-path)
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

(defun org-rifle--speed-command (command)
  "Call COMMAND with `org-speed-move-safe', ignoring any errors."
  (ignore-errors (org-speed-move-safe command)))

(defun org-rifle-buffer-visible-p (buffer)
  "Return non-nil if BUFFER is visible.
That is, if its name does not start with a space."
  (not (s-starts-with? " " (buffer-name buffer))))

(defun org-rifle-fontify-like-in-org-mode (s &optional odd-levels)
  "Fontify string S like in Org-mode.
`org-odd-levels-only' is bound to ODD-LEVELS.

`org-fontify-like-in-org-mode' is a very, very slow function
because it creates a new temporary buffer and runs `org-mode' for
every string it fontifies.  This function reuses a single
invisible buffer and only runs `org-mode' when the buffer is
created."
  (let ((buffer (get-buffer org-rifle-fontify-buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create org-rifle-fontify-buffer-name))
      (with-current-buffer buffer
        (org-mode)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert s)
      (let ((org-odd-levels-only odd-levels))
        ;; FIXME: "Warning: ‘font-lock-fontify-buffer’ is for interactive use only; use
        ;; ‘font-lock-ensure’ or ‘font-lock-flush’ instead."
        (font-lock-fontify-buffer)
        (buffer-string)))))

(defun org-rifle-insert-source-header (text)
  "Insert header containing TEXT into the current buffer.
From `helm-insert-header'."
  (unless (bobp)
    (let ((start (point)))
      (insert "\n")
      ;; FIXME: Shouldn't be using helm-header stuff here.
      (set-text-properties start (point) '(helm-header-separator t))))
  (setq text (concat " " text "\n"))
  ;; Only set the font-lock-face on a single line
  (add-text-properties 0 (length text) '(font-lock-face helm-source-header) text)
  ;; Apply the `helm-header' property to the whole thing, including
  ;; newlines
  (setq text (concat "\n" text ))
  (add-text-properties 0 (length text) '(helm-header t) text)
  (insert text))

(defun org-rifle-cleanup-buffer ()
  "Cleanup occur results buffer when search is aborted."
  ;; FIXME: It doesn't seem like this should have to be a command,
  ;; because it pollutes the command list, but it gave an error once
  ;; when it waasn't one, and it caused weird behavior...
  (interactive)
  (ignore-errors
    ;; Ignore errors to avoid any weirdness which may result in
    ;; infinite loops and being stuck in the minibuffer.  This has
    ;; never happened to me, of course...
    (kill-buffer org-rifle-results-buffer-name)
    ;; Not sure if this is absolutely necessary, but it seems to help
    ;; avoid a weird bug, so it's staying.
    (abort-recursive-edit)))

(defun org-rifle-prep-token (token)
  "Apply regexp prefix and suffix for TOKEN."
  (if (string-match org-rifle-tags-re token)
      ;; Tag
      (rx-to-string `(seq (optional (regexp ,org-rifle-tags-re))
                          ,token
                          (or (regexp ,org-rifle-tags-re) space eol)))
    ;; Not a tag; use normal prefix/suffix
    (rx-to-string `(seq (regexp ,org-rifle-re-prefix)
                        ,token
                        (regexp ,org-rifle-re-suffix)))))

(defun org-rifle-replace-links-in-string (string)
  "Replace `org-mode' links in STRING with their descriptions."
  ;; FIXME: Use `org-link-display-format'.
  (if (string-match org-bracket-link-regexp string)
      (replace-regexp-in-string org-bracket-link-regexp
                                (lambda (text) (or (match-string-no-properties 3 text)
                                                   (match-string-no-properties 1 text)))
                                string t t)
    ;; No links found; return original string
    string))

(defun org-rifle-split-tags-in-input-list (input)
  "Split strings containing multiple Org tags in INPUT.
A string like \":tag1:tag2:\" becomes two strings, \":tag1:\" and
\":tag2:\"."
  (cl-loop for string in input
           for tags = (org-rifle-split-tag-string string)
           if tags append tags into result
           else collect string into result
           finally return result))

(defun org-rifle-split-tag-string (s)
  "Return list containing Org tag strings for input string S.
For S \":tag1:tag2:\" a list '(\":tag1:\" \":tag2:\") is
returned."
  (cl-loop with skip
           ;; FIXME: There is probably a nicer way to handle negated
           ;; tags.  `s-match-strings-all' returns a list containing
           ;; both "!:negatedtag:" and ":negatedtag:", so we have to
           ;; skip the extra, non-negated one
           for tag in (s-match-strings-all "\\(!?\\)\\(:[[:alnum:]_@#%%]+:\\)" s)
           if skip
           do (setq skip nil)
           else if (string= "!" (second tag))
           do (setq skip t)
           and collect (car tag)
           else collect (car tag)))

(defun org-rifle-set-sort-mode ()
  "Set sorting mode by setting `org-rifle-sort-order' if prefix given."
  (cond ((equal '(4) current-prefix-arg)
         ;; C-u; set sorting mode
         (setq org-rifle-sort-order (org-rifle-prompt-for-sort-mode))))
  (setq org-rifle-transformer org-rifle-sort-order))

(defun org-rifle-reset-sort-mode ()
  "When `org-rifle-sort-order-persist' is nil, reset sort order to the saved value."
  ;; FIXME: This is inelegant, to say the least, but using hooks to do
  ;; this means that a command can't simply be called inside a `let',
  ;; so the value has to be set and then reset.  A dispatcher function
  ;; which calls all commands might be a better way to do this.
  (unless org-rifle-sort-order-persist
    (custom-reevaluate-setting 'org-rifle-sort-order)))

(defun org-rifle-prompt-for-sort-mode ()
  "Ask the user which sorting method to use.
Return sorting function corresponding to chosen description string."
  ;; This mess is required because of what seems like a bug or
  ;; inconsistency in the Emacs customize system.  See comments in the
  ;; defcustom.
  (let* ((choices (--remove (stringp (-last-item it)) ; Filter empty "Custom function"
                            (cdr (get 'org-rifle-sort-order 'custom-type))))
         (choice-tag (comp-read "Sort by: " (--map (third it)
                                                   choices))))
    (cl-loop for choice in choices
             when (string= (third choice) choice-tag)
             return (-last-item choice))))

(provide 'org-rifle)

;;; org-rifle.el ends here

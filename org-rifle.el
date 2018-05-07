;;; org-rifle.el --- Rifle through your Org files

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-rifle
;; Version: 2.0.0-pre
;; Package-Requires: ((emacs "24.4") (dash "2.12") (f "0.18.1") (s "1.10.0"))
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

;; Install the dash.el, f.el, and s.el packages.  Then require this package
;; in your init file:

;; (require 'org-rifle)

;;; Usage:

;; Run one of the rifle commands, type some words, and results will be
;; displayed, grouped by buffer.  Hit "RET" to show the selected
;; entry, or <C-return> to show it in an indirect buffer.

;; + `org-rifle': Show results from all open Org buffers
;; + `org-rifle-agenda-files': Show results from Org agenda files
;; + `org-rifle-current-buffer': Show results from current buffer
;; + `org-rifle-directories': Show results from selected directories; with prefix, recursively
;; + `org-rifle-files': Show results from selected files
;; + `org-rifle-org-directory': Show results from Org files in `org-directory'

;;;; Tips

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

;; This file includes the customization settings and loads the default frontend.

;;;; Require

(require 'org-rifle-ui)

;;;; Vars

(defconst org-rifle-fontify-buffer-name " *org-rifle-fontify*"
  "The name of the invisible buffer used to fontify `org-mode' strings.")

(defconst org-rifle-results-buffer-name "*org-rifle*"
  "The name of the results buffer for `org-rifle' commands.")

(defconst org-rifle-tags-re (org-re "\\(?:[ \t]+\\(:[[:alnum:]_@#%%:]+:\\)\\)?")
  "Regexp used to match Org tag strings.  From org.el.")

(defgroup org-rifle nil
  "Settings for `org-rifle'."
  :group 'helm
  :link '(url-link "http://github.com/alphapapa/org-rifle"))

(defcustom org-rifle-always-show-entry-contents-chars 50
  "When non-zero, always show this many characters of entry text, even if none of it matches query."
  :group 'org-rifle :type 'integer)

(defcustom org-rifle-before-command-hook '(org-rifle-set-sort-mode)
  "Hook that runs before each org-rifle command."
  :group 'org-rifle :type 'hook)

(defcustom org-rifle-after-command-hook '(org-rifle-reset-sort-mode)
  "Hook that runs after each org-rifle command."
  :group 'org-rifle :type 'hook)

(defcustom org-rifle-close-unopened-file-buffers t
  "Close buffers that were not already open.
After rifling through Org files that are not already open, close
the buffers if non-nil.  If nil, leave the buffers open.  Leaving
them open will speed up subsequent searches but clutter the
buffer list."
  :group 'org-rifle :type 'boolean)

(defcustom org-rifle-context-characters 25
  "How many characters around each matched term to display."
  :group 'org-rifle :type 'integer)

(defcustom org-rifle-directories-recursive t
  "Recurse into subdirectories by default in `org-rifle-directories'.
When `org-rifle-directories' is called with a prefix, this
option will be inverted."
  :group 'org-rifle :type 'boolean)

(defcustom org-rifle-ellipsis-string "..."
  "Shown between match context strings."
  :group 'org-rifle :type 'string)

(defcustom org-rifle-ellipsis-face 'font-lock-comment-delimiter-face
  "Face for ellipses between match context strings."
  :group 'org-rifle :type 'face)

(defcustom org-rifle-directories-filename-regexp "\.org$"
  "Regular expression to match Org filenames in `org-rifle-directories'.
Files matching this regexp will be searched.  By default, \".org\" files are matched, but you may also select to include \".org_archive\" files, or use a custom regexp."
  :group 'org-rifle
  :type '(radio (string :tag "Normal \".org\" files" :value "\.org$")
                (string :tag "Also include \".org_archive\" files" "\.org\\(_archive\\)?$")
                (string :tag "Custom regexp.  You know what you're doing.")))

(defcustom org-rifle-fontify-headings t
  "Fontify Org headings.

For large result sets this may be slow, although it doesn't seem
to be a major bottleneck."
  :group 'org-rifle :type 'boolean)

(defcustom org-rifle-heading-contents-separator "\n"
  "Separator inserted between entry's heading and contents.
Usually this should be a newline, but it may be useful to adjust
it when defining custom commands.  For example, by setting this
to a non-newline value and disabling `org-rifle-multiline',
each result can be displayed on a single line."
  :type 'string)

(defcustom org-rifle-multiline t
  "Show entries on multiple lines, with the heading on the first line and a blank line between.
In most cases this should remain on, but it may be useful to
disable it when defining custom commands.  Note that if this is
disabled, usually `org-rifle-heading-contents-separator'
should be set to a non-newline value, e.g. a space or something
like \": \"."
  :type 'boolean)

(defcustom org-rifle-show-full-contents nil
  "Show all of each result's contents instead of just context around each matching word."
  :group 'org-rifle :type 'boolean)

(defcustom org-rifle-show-todo-keywords t
  "Show and match against Org todo keywords."
  :group 'org-rifle :type 'boolean)

(defcustom org-rifle-show-path nil
  "Show the whole heading path instead of just the entry's heading."
  :group 'org-rifle :type 'boolean)

(defcustom org-rifle-re-prefix
  "\\(\\_<\\|[[:punct:]]\\)"
  "Regexp matched immediately before each search term.
\(Customize at your peril (but it's okay to experiment here,
because you can always revert your changes).)"
  :group 'org-rifle :type 'regexp)

(defcustom org-rifle-re-suffix
  "\\(\\_>\\|[[:punct:]]\\)"
  "Regexp matched immediately after each search term.
\(What, didn't you read the last warning?  Oh, nevermind.)"
  :group 'org-rifle :type 'regexp)

(defcustom org-rifle-sort-order nil
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
                (function-item :tag "Latest timestamp" org-rifle-transformer-sort-by-latest-timestamp)
                (function :tag "Custom function")))

(defcustom org-rifle-unlinkify-entry-links t
  "Turn Org links in entry text into plain text so they look nicer in Helm buffers.
Just in case this is a performance issue for anyone, it can be disabled."
  :type 'boolean)

(defcustom org-rifle-sort-order-persist nil
  "When non-nil, keep the sort order setting when it is changed by calling a command with a universal prefix."
  :group 'org-rifle :type 'boolean)

(defvar org-rifle-last-input nil
  "Last input given, used to avoid re-running search when input hasn't changed.")

(defvar org-rifle-transformer nil
  "Function to transform results, usually for sorting.  Not intended to be user-set at this time.")

(provide 'org-rifle)

;;; org-rifle.el ends here

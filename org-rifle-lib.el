;;; org-rifle-lib.el --- Rifle through your Org files

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-rifle

;;; Commentary:

;; This file includes library functions that will be used by front- and back-ends.

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

;;;; Actions

(defun org-rifle-show-entry (candidate)
  "Show CANDIDATE using the default function."
  (funcall org-rifle-show-entry-function candidate))

(defun org-rifle-show-entry-in-real-buffer (candidate)
  "Show CANDIDATE in its real buffer."
  (attrset 'new-buffer nil)  ; Prevent the buffer from being cleaned up
  (-let (((buffer . pos) candidate))
    (switch-to-buffer buffer)
    (goto-char pos))
  (org-show-entry))

(defun org-rifle-show-entry-in-indirect-buffer (candidate)
  "Show CANDIDATE in an indirect buffer."
  (-let (((buffer . pos) candidate)
         (original-buffer (current-buffer)))
    (attrset 'new-buffer nil)  ; Prevent the buffer from being cleaned up
    (switch-to-buffer buffer)
    (goto-char pos)
    (org-tree-to-indirect-buffer)
    (unless (equal original-buffer (car (window-prev-buffers)))
      ;; The selected bookmark was in a different buffer.  Put the
      ;; non-indirect buffer at the bottom of the prev-buffers list
      ;; so it won't be selected when the indirect buffer is killed.
      (set-window-prev-buffers nil (append (cdr (window-prev-buffers))
                                           (car (window-prev-buffers)))))))

(defun org-rifle--clock-in (candidate)
  "Clock into CANDIDATE."
  (-let (((buffer . pos) candidate))
    (with-current-buffer buffer
      (goto-char pos)
      (org-clock-in))))

;;;; Timestamp functions

(defun org-rifle-timestamps-in-node (&optional node-start node-end)
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
  "Sort list of node plists by latest timestamp in each node."
  (sort nodes
        (lambda (a b)
          (> (seq-max (plist-get a :timestamp-floats))
             (seq-max (plist-get b :timestamp-floats))))))

(defun org-rifle-transformer-sort-by-latest-timestamp (candidates)
  "Sort CANDIDATES by latest timestamp in each candidate in SOURCE."
  (with-current-buffer (attr 'buffer) ; This is necessary or it will try to use the "*helm*" buffer instead of the source.
    ;; FIXME: This caused a lot of hair-pulling when adding the occur
    ;; code, because the occur code doesn't use this transformer and
    ;; so wasn't running the timestamp-getting function in the right
    ;; buffer--it was running it in the minibuffer.  It would be good
    ;; to make them use a common format so they could always use the
    ;; transformer, but that wouldn't be as good for performance,
    ;; because then the transformer would ALWAYS have to run.  Maybe
    ;; it's worth it...
    (->> candidates
         (org-rifle-transform-candidates-to-list-of-nodes)
         (org-rifle-add-timestamps-to-nodes)
         (org-rifle-sort-nodes-by-latest-timestamp)
         (org-rifle-transform-list-of-nodes-to-candidates))))

;;;; Support functions

(defun org-rifle--listify (item)
  "If ITEM is an atom, return (list ITEM).  If ITEM is a list, return ITEM."
  ;; TODO: This could simply be e.g. (defun listify (&rest args) args)
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

(defun org-rifle-buffer-visible-p (buffer)
  "Return non-nil if BUFFER is visible.
That is, if its name does not start with a space."
  (not (s-starts-with? " " (buffer-name buffer))))

(defun org-rifle-fontify-like-in-org-mode (s &optional odd-levels)
  "Fontify string S like in Org-mode.

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
        (font-lock-fontify-buffer)
        (buffer-string)))))

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
  "Split strings containing multiple Org tags in INPUT into separate tag strings.
i.e. a string like \":tag1:tag2:\" becomes two strings, \":tag1:\" and \":tag2:\"."
  (cl-loop for string in input
           for tags = (org-rifle-split-tag-string string)
           if tags append tags into result
           else collect string into result
           finally return result))

(defun org-rifle-split-tag-string (s)
  "Return list containing Org tag strings for input string S containing Org tags.
i.e. for S \":tag1:tag2:\" a list '(\":tag1:\" \":tag2:\") is returned."
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

(defun org-rifle-transform-candidates-to-list-of-nodes (candidates)
  "Transform style CANDIDATES list to list of plists."
  (--map (list :node-beg (cadr it)
               :text (car it))
         candidates))

(defun org-rifle-transform-list-of-nodes-to-candidates (nodes)
  "Transform list of node plists to style candidates."
  (--map (list (plist-get it :text)
               (plist-get it :node-beg))
         nodes))

(provide 'org-rifle-lib)

;;; org-rifle-lib.el ends here

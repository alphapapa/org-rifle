;;; org-rifle-ui.el --- Rifle through your Org files

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-rifle

;;; Commentary:

;; This file includes the default (non-Helm) UI.

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

;;;; Customization

(defface org-rifle-separator
  ;; FIXME: Pick better default color.  Black is probably too harsh.
  '((((background dark))
     :background "black")
    (((background light))
     :foreground "black"))
  "Face for `org-rifle-separator', which is displayed between results.")

(defcustom org-rifle-kill-empty-buffer t
  "Close occur results buffer after last result is deleted."
  :type 'boolean)

(defvar org-rifle-map
  (let ((map (copy-keymap org-mode-map)))
    (define-key map [remap org-cycle] 'org-rifle--org-cycle)
    (define-key map [remap undo] (lambda () (interactive) (let ((inhibit-read-only t)) (undo))))
    (define-key map [mouse-1] 'org-rifle-goto-entry)
    (define-key map (kbd "<RET>") 'org-rifle-goto-entry)
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
  "Keymap for org-rifle results buffers.  Imitates org-speed keys.")

(defvar org-rifle-minibuffer-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-g") 'org-rifle-cleanup-buffer)
    map)
  "Keymap for org-rifle minibuffers.")

(defvar org-rifle-separator
  (let ((text "\n"))
    (set-text-properties 0 (length text) '(org-rifle-result-separator t font-lock-face org-rifle-separator) text)
    text)
  "Propertized separator for results in occur buffers.")

;;;; Functions

(defun org-rifle--org-cycle (&optional arg)
  "Cycle folding of Org entries in an occur buffer.
This folds first at boundaries defined by `header' and
`org-rifle-result-separator' text-properties, and then
normally, by outline headings."
  (interactive)
  (cl-letf (((symbol-function 'org-end-of-subtree)
             'org-rifle--org-end-of-subtree)
            ((symbol-function 'outline-next-heading)
             'org-rifle--outline-next-heading))
    (if (text-property-any (line-beginning-position) (line-end-position) 'header t)
        ;; On a header line; cycle all entries in this source
        (let ((start (line-beginning-position))
              (end (save-excursion
                     (while (text-property-any (line-beginning-position) (line-end-position) 'header t)
                       (forward-line 1))
                     (forward-char)
                     (let ((char (or (next-single-property-change (point) 'header nil)
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
  "Move to the next Helm header, result separator, outline heading, or point-max, whichever is smallest.
This is intended to override `outline-next-heading' in occur
results buffers."
  (interactive)
  (cl-flet ((min (&rest args)
                 (apply 'min (-non-nil args)))
            (next-outline-heading ()
                                  ;; This is basically a copy of `outline-next-heading'
                                  (when (re-search-forward (concat "^\\(?:" outline-regexp "\\)")
                                                           nil 'move)
                                    (match-beginning 0))))
    (cond ((or (memq 'header (text-properties-at (point)))
               (memq 'org-rifle-result-separator (text-properties-at (point))))
           ;; At a source header or result separator; go to first outline heading
           (goto-char (next-outline-heading)))

          ((org-at-heading-p)
           ;; At a heading; go to next header or separator or end-of-buffer
           (goto-char (1- (min (next-single-property-change (point) 'org-rifle-result-separator)
                               (next-single-property-change (point) 'header)
                               (point-max)))))

          (t
           (let ((char (min (next-single-property-change (point) 'org-rifle-result-separator)
                            (next-single-property-change (point) 'header)
                            (next-outline-heading))))
             (when char
               (goto-char char)))))))

(defun org-rifle--org-end-of-subtree (&optional invisible-ok to-heading)
  "Goto to the end of a subtree.
This function is a copy of `org-end-of-subtree', but it respects
headers and separators."
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
                            (next-single-property-change (point) 'header)
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
                           ;; FIXME: org-rifle-input-idle-delay
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

(defun org-rifle--show-entries-as (entries)
  "Display a buffer showing ENTRIES.
ENTRIES is a list of (BUFFER . NODE-BEG) pairs from the REAL of
the (DISPLAY . REAL) pair from
`org-rifle--get-candidates-in-buffer'."
  (let ((inhibit-read-only t))
    (with-current-buffer (org-rifle--prepare-results-buffer)
      (erase-buffer)
      (cl-loop for (buffer . node-beg) in entries
               for text = (org-rifle--get-entry-text buffer node-beg :include-heading t :full-path org-rifle-show-path)
               do (progn (add-text-properties 0 (length text) (list :buffer buffer :node-beg node-beg) text)
                         (insert org-rifle-separator)
                         (insert text)))
      (org-rifle-highlight-matches-in-buffer (current-buffer) input))))

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
    (unless (eq major-mode 'org-mode)
      (error "Buffer %s is not an Org buffer." buffer)))
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

(defun helm-org-rifle--speed-command (command)
  "Call COMMAND with `org-speed-move-safe', ignoring any errors."
  (ignore-errors (org-speed-move-safe command)))

(defun org-rifle-insert-source-header (text)
  "Insert header containing TEXT into the current buffer.
From `insert-header'."
  (unless (bobp)
    (let ((start (point)))
      (insert "\n")
      (set-text-properties start (point) '(header-separator t))))
  (setq text (concat " " text "\n"))
  ;; Only set the font-lock-face on a single line
  (add-text-properties 0 (length text) '(font-lock-face source-header) text)
  ;; Apply the `header' property to the whole thing, including
  ;; newlines
  (setq text (concat "\n" text ))
  (add-text-properties 0 (length text) '(header t) text)
  (insert text))

;;;;; Commands

(cl-defmacro org-rifle-define-command (name args docstring &key buffers files directories preface)
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
             ,preface  ; Maybe not necessary
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

;;;###autoload (autoload 'helm-org-rifle "helm-org-rifle" nil t)
(org-rifle-define-command
 nil ()
 "Search all Org buffers, showing results in an occur-like, persistent buffer."
 :buffers (--remove (string= org-rifle-results-buffer-name (buffer-name it))
                    (-select 'org-rifle-buffer-visible-p
                             (org-buffer-list nil t))))

;;;###autoload (autoload 'org-rifle-current-buffer "org-rifle" nil t)
(org-rifle-define-command
 "current-buffer" ()
 "Search current buffer, showing results in an occur-like, persistent buffer."
 :buffers (list (current-buffer)))

;;;###autoload (autoload 'org-rifle-directories "org-rifle" nil t)
(org-rifle-define-command
 "directories" (&optional directories)
 "Search files in DIRECTORIES, showing results in an occur-like, persistent buffer.
Files are opened if necessary, and the resulting buffers are left open."
 :directories (or directories
                  (read-file-name "Directories: " :marked-candidates t)))

;;;###autoload (autoload 'org-rifle-files "org-rifle" nil t)
(org-rifle-define-command
 "files" (&optional files)
 "Search FILES, showing results in an occur-like, persistent buffer.
Files are opened if necessary, and the resulting buffers are left open."
 :files (or files
            (read-file-name "Files: " :marked-candidates t)))

;;;###autoload (autoload 'org-rifle-agenda-files "org-rifle" nil t)
(org-rifle-define-command
 "agenda-files" ()
 "Search Org agenda files, showing results in an occur-like, persistent buffer.
Files are opened if necessary, and the resulting buffers are left open."
 :files (org-agenda-files))

;;;###autoload (autoload 'org-rifle-org-directory "org-rifle" nil t)
(org-rifle-define-command
 "org-directory" ()
 "Search files in `org-directory', showing results in an occur-like, persistent buffer.
Files are opened if necessary, and the resulting buffers are left open."
 :directories (list org-directory))

(provide 'org-rifle-ui)

;;; org-rifle-ui.el ends here

;;; -*- lexical-binding: t -*-

;;; Code:

;;;; Requirements

(require 'helm-org-rifle)

;;;; Helper functions

(defun helm-org-rifle--test-helper-process-candidates (candidates)
  "Remove text properties and convert buffers to buffer names in CANDIDATES."
  (->> candidates
       (helm-org-rifle--test-helper-remove-properties-from-candidates)
       (helm-org-rifle--test-helper-convert-buffers-to-buffer-names-in-candidates)))

(defun helm-org-rifle--test-helper-remove-properties-from-candidates (candidates)
  "Remove text properties from CANDIDATES display values.
Since the properties could vary from one user to another
depending on settings, active packages, etc, we should remove
them."
  (cl-loop for entry in candidates
           do (setf (car entry) (substring-no-properties (car entry)))
           finally return candidates))

(defun helm-org-rifle--test-helper-convert-buffers-to-buffer-names-in-candidates (candidates)
  "Convert buffer objects to buffer names in CANDIDATES."
  ;; FIXME: Is there a way to represent buffer objects directly to
  ;; avoid having to do this?
  (cl-loop for (display . real) in candidates
           do (setf (car real) (buffer-name (car real)))
           finally return candidates))

;;;; Tests

(describe "helm-org-rifle"

  (before-all
    ;; TODO: Make sure customizable options are set uniformly, here and/or in the steps
    ;; TODO: Use assess's buffer-related functions to do this more cleanly

    ;; (setq test-buffer (find-buffer-visiting "data.org"))
    (setq test-buffer (find-file-noselect (concat default-directory "test/data.org"))))

  (before-each
    ;; Set options consistently
    ;; TODO: Can we get these defaults from the other file?
    (setq helm-org-rifle-always-show-entry-contents-chars 50
          helm-org-rifle-before-command-hook '(helm-org-rifle-set-sort-mode)
          helm-org-rifle-after-command-hook '(helm-org-rifle-reset-sort-mode)
          helm-org-rifle-close-unopened-file-buffers t
          helm-org-rifle-context-characters 25
          helm-org-rifle-directories-recursive t
          helm-org-rifle-ellipsis-string "..."
          helm-org-rifle-ellipsis-face 'font-lock-comment-delimiter-face
          helm-org-rifle-directories-filename-regexp "\.org$"
          helm-org-rifle-fontify-headings t
          helm-org-rifle-heading-contents-separator "\n"
          helm-org-rifle-input-idle-delay 0.05
          helm-org-rifle-multiline t
          helm-org-rifle-show-entry-function 'helm-org-rifle-show-entry-in-real-buffer
          helm-org-rifle-show-full-contents nil
          helm-org-rifle-show-todo-keywords t
          helm-org-rifle-show-path nil
          helm-org-rifle-re-prefix "\\(\\_<\\|[[:punct:]]\\)"
          helm-org-rifle-re-suffix "\\(\\_>\\|[[:punct:]]\\)"
          helm-org-rifle-sort-order nil
          helm-org-rifle-unlinkify-entry-links t
          helm-org-rifle-sort-order-persist nil
          helm-org-rifle-occur-kill-empty-buffer t))

  (describe "helm-org-rifle-split-tag-string"
    (it "Can split a string of search terms and return a list of tags"
      (expect (helm-org-rifle-split-tag-string "notatag :tag1:tag2: :tag3: notatageither !:negatedtag:")
              :to-equal '(":tag1:" ":tag2:" ":tag3:" "!:negatedtag:"))))

  (describe "helm-org-rifle-split-tags-in-input-list"

    (it "Can split a list of strings containing search terms into a list of tags"
      (expect (helm-org-rifle-split-tags-in-input-list (split-string "notatag :tag1:tag2: :tag3: notatageither" " " t))
              :to-equal '("notatag" ":tag1:" ":tag2:" ":tag3:" "notatageither")))

    (it "Can split a list of strings containing search terms, including negated tags, into a list of tags"
      (expect (helm-org-rifle-split-tags-in-input-list (split-string "notatag :tag1:tag2: :tag3: notatageither !:negatedtag:" " " t))
              :to-equal '("notatag" ":tag1:" ":tag2:" ":tag3:" "notatageither" "!:negatedtag:"))))

  (describe "helm-org-rifle--get-candidates-in-buffer"

    ;; FIXME: For some reason the REAL cons in (DISPLAY . REAL) gets
    ;; flattened into the parent cons.  The test works, but
    ;; I guess it's not exactly the same as the non-test, so
    ;; these tests should probably be fixed.

    (it "Can match against headings"
      (expect (helm-org-rifle--test-helper-process-candidates
               (helm-org-rifle--get-candidates-in-buffer test-buffer "berry"))
              :to-equal '(("**** Banana  
le fruit – botanically a berry^[1]^[2] – produced by se" "data.org" . 174)
                          ("**** Blueberry  
#+BEGIN_QUOTE
Blueberries are perennial floweri..." "data.org" . 1143)
                          ("**** Strawberry  
The garden strawberry (or simply strawberry; F...which is not a botanical berry, but an aggregate access...colates. Artificial strawberry flavorings and aromas ar" "data.org" . 1640))))

    (it "Can match against entry text"
      (expect (helm-org-rifle--test-helper-process-candidates
               (helm-org-rifle--get-candidates-in-buffer test-buffer "food"))
              :to-equal '(("* Food :food: 
" "data.org" . 2)
                          ("**** Strawberry  
resh or in such prepared foods as preserves, fruit jui" "data.org" . 1640)
                          ("**** Chicken :barbecue:fowl: 
primarily as a source of food, consuming both their me" "data.org" . 3114)
                          ("***** Fries  
he menus of diners, fast food restaurants, pubs, and b" "data.org" . 4375))))

    (describe "Can match against tags..."
      (it "...in any order"
        (expect (helm-org-rifle--test-helper-process-candidates
                 (helm-org-rifle--get-candidates-in-buffer test-buffer ":fowl:barbecue:"))
                :to-equal '(("**** Chicken :barbecue:fowl: 
#+BEGIN_QUOTE
The chicken (Gallus gallus domest..." "data.org" . 3114)))))

    (describe "Can negate matches..."
      (it "...on headings"
        (expect (helm-org-rifle--test-helper-process-candidates
                 (helm-org-rifle--get-candidates-in-buffer test-buffer "meat !chicken"))
                :to-equal '(("*** Meat :meat: 
" "data.org" . 2400)
                            ("***** Brisket :barbecue: 
Brisket is a cut of meat from the breast or lower...tissue, so the resulting meat must be cooked correctly" "data.org" . 2488)
                            ("**** Pork  
is the culinary name for meat from the domestic pig (S...e most commonly consumed meat worldwide, with evidence" "data.org" . 3541)
                            ("***** Pulled pork :barbecue: 
erwise be a tough cut of meat is cooked slowly at low...mperatures, allowing the meat to become tender enough" "data.org" . 3942))))

      (it "...on entry text"
        (expect (helm-org-rifle--test-helper-process-candidates
                 (helm-org-rifle--get-candidates-in-buffer test-buffer "fruit !edible"))
                :to-equal '(("*** Fruit :fruit: 
" "data.org" . 97)
                            ("**** Blueberry  
are the most common^[1] fruits sold as \"blueberries\" a" "data.org" . 1143)
                            ("**** Strawberry  
ivated worldwide for its fruit. The fruit (which is not...t an aggregate accessory fruit) is widely appreciated f...ared foods as preserves, fruit juice, pies, ice creams," "data.org" . 1640))))

      (it "...on tags in individual headings"
        (expect (helm-org-rifle--test-helper-process-candidates
                 (helm-org-rifle--get-candidates-in-buffer test-buffer "meat !:fowl:"))
                :to-equal '(("*** Meat :meat: 
" "data.org" . 2400) ("***** Brisket :barbecue: 
Brisket is a cut of meat from the breast or lower...tissue, so the resulting meat must be cooked correctly" "data.org" . 2488) ("**** Chicken :barbecue:fowl: 
od, consuming both their meat and their eggs." "data.org" . 3114) ("**** Pork  
is the culinary name for meat from the domestic pig (S...e most commonly consumed meat worldwide, with evidence" "data.org" . 3541) ("***** Pulled pork :barbecue: 
erwise be a tough cut of meat is cooked slowly at low...mperatures, allowing the meat to become tender enough" "data.org" . 3942)))))

    (it "Can match against TODO keywords"
      (expect (helm-org-rifle--test-helper-process-candidates
               (helm-org-rifle--get-candidates-in-buffer test-buffer "[#B]"))
              :to-equal '(("***** [#B] Mashed potatoes  
#+BEGIN_QUOTE
Mashed potato (British English) o..." "data.org" . 5066))))

    (it "Can match against priorities"
      (setq expected-result '(("***** [#B] Mashed potatoes  
#+BEGIN_QUOTE
Mashed potato (British English) o..." "data.org" . 5066)))
      (expect (helm-org-rifle--test-helper-process-candidates
               (helm-org-rifle--get-candidates-in-buffer test-buffer "[#B]"))
              :to-equal expected-result)
      (expect (helm-org-rifle--test-helper-process-candidates
               (helm-org-rifle--get-candidates-in-buffer test-buffer "#B"))
              :to-equal expected-result))))

;;;; Update-results functions

(defun helm-org-rifle--test-update-result ()
  "Update result in next `expect'."
  (interactive)
  (re-search-forward "(expect")
  (forward-sexp)
  (eval-print-last-sexp)
  (transpose-sexps 1)
  (kill-sexp)
  (backward-sexp)
  (cycle-spacing -1))

;;; Config

;; Remove `delete-trailing-whitespace' from local `before-save-hook',
;; because it breaks the strings in the tests

;; Local Variables:
;; eval: (when (memq 'delete-trailing-whitespace before-save-hook) (remove-hook 'before-save-hook 'delete-trailing-whitespace t))
;; End:

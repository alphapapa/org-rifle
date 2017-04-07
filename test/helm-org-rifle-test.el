;;; -*- lexical-binding: t -*-

;;; Code:

;;;; Requirements

(require 'helm-org-rifle)

;;;; Helper functions

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
    (setq test-buffer (find-file-noselect "test/data.org")))

  (describe "helm-org-rifle--get-candidates-in-buffer"

    (it "Can match against headings"
      (expect (->> (helm-org-rifle--get-candidates-in-buffer test-buffer "berry")
                   (helm-org-rifle--test-helper-remove-properties-from-candidates)
                   (helm-org-rifle--test-helper-convert-buffers-to-buffer-names-in-candidates))
              ;; FIXME: For some reason the REAL cons in (DISPLAY . REAL)
              ;; gets flattened into the parent cons.  The test works,
              ;; but I guess it's not exactly the same as the
              ;; non-test, so this should probably be fixed.
              :to-equal '(("**** Banana  
le fruit – botanically a berry^[1]^[2] – produced by se" "data.org" . 173)
                          ("**** Blueberry  
#+BEGIN_QUOTE
Blueberries are perennial floweri..." "data.org" . 1142)
                          ("**** Strawberry  
The garden strawberry (or simply strawberry; F...which is not a botanical berry, but an aggregate access...colates. Artificial strawberry flavorings and aromas ar" "data.org" . 1639))))))

;;; Config

;; Remove `delete-trailing-whitespace' from local `before-save-hook',
;; because it breaks the strings in the tests

;; Local Variables:
;; eval: (when (memq 'delete-trailing-whitespace before-save-hook) (remove-hook 'before-save-hook 'delete-trailing-whitespace t))
;; End:

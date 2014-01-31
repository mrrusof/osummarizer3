;; Color words PASSED and FAILED in prolog interpreter
(font-lock-add-keywords 'prolog-inferior-mode '(("PASSED" . font-lock-type-face)))
(font-lock-add-keywords 'prolog-inferior-mode '(("Positive" . font-lock-type-face)))
(font-lock-add-keywords 'prolog-inferior-mode '(("FAILED" . font-lock-warning-face)))
(font-lock-add-keywords 'prolog-inferior-mode '(("Negative" . font-lock-warning-face)))

;; Do unit testing on save of *.pl
(defun osum-unit-test ()
  "Perform unit testing for OSUMMARIZER"
  (interactive)
  (if (and (eq 'prolog-mode major-mode) (get-buffer "unit.pl"))
      (let ((cb (current-buffer)))
        (set-buffer "unit.pl")
        (prolog-consult-buffer)
        (recenter-top-bottom 'top)
        (switch-to-buffer-other-window cb))))
(add-hook 'after-save-hook 'osum-unit-test)
;; (remove-hook 'after-save-hook 'osum-unit-test)

;; Prolog mode for the following files
(setq auto-mode-alist (append '(("\\.qarmc$" . prolog-mode)
                                ("\\.hsf$" . prolog-mode)
                                ("\\.of" . prolog-mode)
                                ("\\.qscript" . prolog-mode)) auto-mode-alist))
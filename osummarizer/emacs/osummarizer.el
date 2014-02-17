;; Color words PASSED and FAILED in prolog interpreter
(font-lock-add-keywords 'prolog-inferior-mode '(("PASSED" . font-lock-type-face)))
(font-lock-add-keywords 'prolog-inferior-mode '(("Positive" . font-lock-type-face)))
(font-lock-add-keywords 'prolog-inferior-mode '(("FAILED" . font-lock-warning-face)))
(font-lock-add-keywords 'prolog-inferior-mode '(("Negative" . font-lock-warning-face)))

(defun osum-unit-test ()
  "Perform unit testing for OSUMMARIZER"
  (interactive)
  ;; (if (and (eq 'prolog-mode major-mode) (get-buffer "unit.pl"))
  (let ((cb (current-buffer)))
    (ignore-errors
      (set-buffer "*prolog*")
      (ignore-errors (comint-send-eof))
      (beginning-of-buffer)
      (ignore-errors  (delete-matching-lines ".*")))
    (set-buffer "unit.pl")
    (add-hook 'comint-output-filter-functions 'osum-list-failed)
    (prolog-consult-buffer)
    (comint-send-eof)
    (switch-to-buffer-other-window cb) ))
(global-set-key (kbd "C-c u") 'osum-unit-test)

(defun osum-list-failed (x)
  "Catch failing tests for OSUMMARIZER"
  (interactive)
  (list-matching-lines "FAILED") )
;; (remove-hook 'comint-output-filter-functions 'osum-list-failed)

;; Prolog mode for the following files
(setq auto-mode-alist (append '(("\\.of$" . prolog-mode)
                                ("\\.named$" . prolog-mode)
                                ("\\.hsf$" . prolog-mode)
                                ("\\.qarmc$" . prolog-mode)
                                ("\\.qscript$" . prolog-mode)) auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.normal" . tuareg-mode))

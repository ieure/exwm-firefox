
(defvar exwm-firefox--intercept nil
  "The function to call when a new Firefox window is intercepted.")

 ;; DELETEME

(defun exwm-firefox-core-window-new ()
  "New window."
  (interactive)
  (exwm-input--fake-key ?\C-n))

;;; History
;;;###autoload
(defun exwm-firefox-core-history-forward ()
  "Forward in history."
  (interactive)
  (exwm-input--fake-key 'M-right))

;;;###autoload
(defun exwm-firefox-core-history-back ()
  "Back in history."
  (interactive)
  (exwm-input--fake-key 'M-left))

;;;###autoload
(defun exwm-firefox-core-focus-search-bar ()
  "Toggle focus between the search bar and the page."
  (interactive)
  (exwm-input--fake-key ?\M-d))

 ;; END DELETEME

(defvar exwm-firefox-keymap
  (let ((map (copy-keymap exwm-mode-map)))
    (define-key map "\C-c\C-f" 'exwm-firefox-core-history-forward)
    (define-key map "\C-c\C-b" 'exwm-firefox-core-history-back)
    (define-key map "\C-c\C-n" 'exwm-firefox-split-window)
    (define-key map "\C-c\C-p" 'exwm-firefox-split-private)
    (define-key map "\C-c\C-d" 'exwm-firefox-split-detach)
    (define-key map "\C-c\C-g" 'exwm-firefox-merge)
    map)
  "Keymap applied in Firefox windows.")

(defun exwm-firefox? ()
  "Does the current buffer contain a Firefox window?"
  (thread-first (string-match "^Firefox" (or exwm-class-name ""))
    (and t)
    (save-match-data)))

(defun exwm-firefox--setup-keymap-hook ()
  "Configure Firefox keymap for EXWM."
  (when (exwm-firefox?)
    (use-local-map exwm-firefox-keymap)))

(defun exwm-firefox--split (old-window-config)
  "Move a new Firefox window into a split."
  (let ((new-firefox (current-buffer)))
    (set-window-configuration old-window-config)
    (switch-to-buffer-other-window new-firefox)
    (exwm-firefox-core-focus-search-bar)
    (setq exwm-firefox--intercept nil)))

(defun exwm-firefox--workspace (workspace)
  "Move a new Firefox window into a workspace."
  (let ((new-firefox (current-buffer)))
    (exwm-workspace-move-window workspace)))

(defun exwm-firefox--intercept-hook ()
  "Run an action the next time a Firefox window is created."
  (if-let ((callback (and (exwm-firefox-p) exwm-firefox--intercept)))
      (funcall callback)))

(defun exwm-firefox--intercept-next (data callback)
  "Perform an action the next time a Firefox window is created.

   Calls function CALLBACK with DATA as its only argument."

  (unless (exwm-firefox?)
    (error "Not a Firefox window"))
  (when exwm-firefox--intercept
    (warn "Already intercepting"))
  (setq exwm-firefox--intercept (apply-partially callback data))
  ;; If nothing happens in 3 seconds, reset the state
  (run-with-timer 3 nil
                  (lambda ()
                    (setq exwm-firefox--intercept nil))))

(defun exwm-firefox-split-window ()
  "Create a new Firefox window in a split."
  (interactive)
  (exwm-firefox--intercept-next (current-window-configuration)
                                'exwm-firefox--split)
  (exwm-input--fake-key ?\C-n))

(defun exwm-firefox-split-private (&optional arg)
  "Create a new Firefox private window in a split.

   With no ARG, create the new window in a split in the current workspace.

   With ARG prefix, display the window in that workspace."
  (interactive "P")
  (if arg
      (exwm-firefox--workspace arg)
    (exwm-firefox--intercept-next (current-window-configuration)
                                  'exwm-firefox--split))
  (exwm-input--fake-key 'C-S-p))

(defun exwm-firefox-split-detach (&optional arg)
  "Detach the current tab into a new split window.

   With no ARG, create the new window in a split in the current workspace.

   With ARG prefix, display the window in that workspace.

   This requires the tabdetach extension to work."
  (interactive "P")
  (if arg
      (exwm-firefox--workspace arg)
    (exwm-firefox--intercept-next (current-window-configuration)
                                  'exwm-firefox--split))
  (exwm-input--fake-key ?\M-\S-d))

(defun exwm-firefox-merge ()
  "Merge the current tab into its parent window.

   This requires the tabdetach extension to work."
  (interactive)
  (exwm-input--fake-key ?\M-\S-m))

(define-minor-mode exwm-firefox-mode
  "Minor mode to enhance Firefox in EXWM."
  nil nil nil
  :global t
  (setq exwm-firefox--intercept nil)
  (if exwm-firefox-mode
      (progn
        (add-hook 'exwm-manage-finish-hook 'exwm-firefox--setup-keymap-hook)
        (add-hook 'exwm-manage-finish-hook 'exwm-firefox--intercept-hook))
    (remove-hook 'exwm-manage-finish-hook 'exwm-firefox--setup-keymap-hook)
    (remove-hook 'exwm-manage-finish-hook 'exwm-firefox--intercept-hook)))

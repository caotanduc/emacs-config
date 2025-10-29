;;; vscode.el --- VSCode-like UX and keybindings -*- lexical-binding: t; -*-
;; Author: Duc Cao
;; Version: 0.6
;; Keywords: convenience, vscode
;; Package-Requires: ((emacs "29.1") (consult "1.0"))
;;; Commentary:
;; Provides a VSCode-like experience for Emacs:
;;  - Command palette, buffer switching, explorer
;;  - Go Back / Forward (Ctrl+- / Ctrl+Shift+-)
;;  - Insert line above/below
;;  - Select word (⌘d) and line (⌘l)
;;  - Optional global minor mode

;;; Code:

(require 'consult)
(require 'project)
(require 'dired)
(require 'xref)
(require 'pulse)
(require 'cl-lib)

;; ─────────────────────────────────────────────────────────────
;; Core Helpers
;; ─────────────────────────────────────────────────────────────

(defun vscode-toggle-command ()
  "Toggle the command palette: open M-x if inactive, cancel if active."
  (interactive)
  (if (minibufferp)
      (keyboard-escape-quit)
    (execute-extended-command nil)))

(defun vscode-open-explorer ()
  "Open project root in Dired if inside a project, else use `dired-jump'."
  (interactive)
  (if (project-current)
      (project-dired)
    (dired-jump)))

(defun vscode-kill-other-buffers ()
  "Kill all other buffers, keeping only the current one."
  (interactive)
  (mapc #'kill-buffer (delq (current-buffer) (buffer-list)))
  (message "Other buffers killed."))

(defun vscode-switch-buffer ()
  "Switch buffer smartly: use project buffer list if available."
  (interactive)
  (if (project-current)
      (call-interactively #'project-switch-to-buffer)
    (consult-buffer)))

(defun vscode-insert-line-below ()
  "Insert a new line below the current one and keep cursor position."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun vscode-insert-line-above ()
  "Insert a new line above the current one and keep cursor position."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode))

(defun vscode-mark-whole-word ()
  "Select the entire word at point, regardless of cursor position."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (if bounds
        (progn
          (goto-char (car bounds))
          (set-mark (cdr bounds)))
      (message "No word at point."))))

(defun vscode-mark-whole-line ()
  "Select the entire current line (like VS Code’s ⌘L)."
  (interactive)
  (beginning-of-line)
  (set-mark (line-end-position)))

;; ─────────────────────────────────────────────────────────────
;; Cursor History (Go Back / Forward)
;; ─────────────────────────────────────────────────────────────

(defgroup vscode-jump nil
  "VSCode-like cursor back/forward history."
  :group 'convenience)

(defcustom vscode-jump-threshold-lines 1
  "Minimum line-distance change to record a new jump."
  :type 'integer)

(defcustom vscode-jump-idle-seconds 0.4
  "Minimum idle time between two recorded positions."
  :type 'number)

(defcustom vscode-jump-max 200
  "Maximum number of positions to keep in history."
  :type 'integer)

(cl-defstruct (vscode--pos (:constructor vscode--pos))
  buffer marker time)

(defvar vscode--jump-list nil
  "List (oldest ... newest) of `vscode--pos'.")

(defvar vscode--jump-index -1
  "Index into `vscode--jump-list'. -1 means 'at newest'.")

(defvar-local vscode--last-recorded-line nil)
(defvar vscode--last-recorded-time 0)

(defun vscode--pos-equal (a b)
  (and a b
       (eq (vscode--pos-buffer a) (vscode--pos-buffer b))
       (equal (marker-position (vscode--pos-marker a))
              (marker-position (vscode--pos-marker b)))))

(defun vscode--now () (float-time (current-time)))

(defun vscode--current-pos ()
  (vscode--pos :buffer (current-buffer)
               :marker (copy-marker (point-marker) t)
               :time (vscode--now)))

(defun vscode--truncate-to (n lst)
  (let ((len (length lst)))
    (if (<= len n) lst (cl-subseq lst (- len n)))))

(defun vscode--push-current-pos ()
  "Push current position to history, truncating any 'forward' tail."
  (let* ((cur (vscode--current-pos))
         (same-as-latest (and vscode--jump-list
                              (vscode--pos-equal cur (car (last vscode--jump-list))))))
    (unless same-as-latest
      (when (>= vscode--jump-index 0)
        (setf vscode--jump-list (cl-subseq vscode--jump-list
                                           0 (- (length vscode--jump-list)
                                                vscode--jump-index))
              vscode--jump-index -1))
      (setq vscode--jump-list
            (vscode--truncate-to vscode-jump-max
                                 (append vscode--jump-list (list cur)))))))

(defun vscode--should-record-move ()
  "Return non-nil if the current move should be recorded."
  (let ((idle (<= vscode-jump-idle-seconds (- (vscode--now) vscode--last-recorded-time)))
        (line (line-number-at-pos)))
    (or (not (eq (current-buffer)
                 (and vscode--jump-list (vscode--pos-buffer (car (last vscode--jump-list))))))
        (and vscode--last-recorded-line
             (>= (abs (- line vscode--last-recorded-line))
                 vscode-jump-threshold-lines))
        idle)))

(defun vscode--post-command-recorder ()
  "Record meaningful cursor moves after each command."
  (when (and (not (minibufferp))
             (not (memq this-command '(vscode-go-back vscode-go-forward))))
    (when (vscode--should-record-move)
      (vscode--push-current-pos)
      (setq vscode--last-recorded-line (line-number-at-pos)
            vscode--last-recorded-time (vscode--now)))))

(defun vscode--goto-pos (pos)
  (when (and pos (buffer-live-p (vscode--pos-buffer pos)))
    (let ((buf (vscode--pos-buffer pos))
          (mk  (vscode--pos-marker pos)))
      (switch-to-buffer buf)
      (goto-char (marker-position mk))
      (pulse-momentary-highlight-one-line (point)))))

(defun vscode-go-back ()
  "Go back to previous cursor location (like VS Code’s Ctrl+-)."
  (interactive)
  (let ((n (length vscode--jump-list)))
    (cond
     ((<= n 1) (message "No previous location."))
     (t
      (when (= vscode--jump-index -1)
        (setq vscode--jump-index 1))
      (if (>= vscode--jump-index (1- n))
          (message "At oldest location.")
        (cl-incf vscode--jump-index)
        (vscode--goto-pos (nth (- n 1 vscode--jump-index) vscode--jump-list)))))))

(defun vscode-go-forward ()
  "Go forward to next cursor location (like VS Code’s Ctrl+Shift+-)."
  (interactive)
  (let ((n (length vscode--jump-list)))
    (cond
     ((or (<= n 1) (<= vscode--jump-index 0))
      (setq vscode--jump-index -1)
      (message "At newest location."))
     (t
      (cl-decf vscode--jump-index)
      (vscode--goto-pos (nth (- n 1 vscode--jump-index) vscode--jump-list))))))

(define-minor-mode vscode-jump-mode
  "Record cursor locations and provide back/forward navigation."
  :init-value t
  :lighter ""
  (if vscode-jump-mode
      (add-hook 'post-command-hook #'vscode--post-command-recorder 90)
    (remove-hook 'post-command-hook #'vscode--post-command-recorder)))

;; ─────────────────────────────────────────────────────────────
;; Minor Mode Definition
;; ─────────────────────────────────────────────────────────────

(defvar vscode-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Main bindings
    (define-key map (kbd "s-P")   #'vscode-toggle-command)
    (define-key map (kbd "s-p")   #'vscode-switch-buffer)
    (define-key map (kbd "s-F")   #'consult-ripgrep)
    (define-key map (kbd "C-<tab>") #'consult-buffer)
    (define-key map (kbd "s-w")   #'kill-this-buffer)
    (define-key map (kbd "s-s")   #'save-buffer)
    (define-key map (kbd "s-E")   #'vscode-open-explorer)
    (define-key map (kbd "C-o")   #'vscode-insert-line-below)
    (define-key map (kbd "s-<return>") #'vscode-insert-line-below)
    (define-key map (kbd "s-S-<return>") #'vscode-insert-line-above)
    (define-key map (kbd "C--")   #'vscode-go-back)
    (define-key map (kbd "C-_")   #'vscode-go-forward)
    (define-key map (kbd "s-d")   #'vscode-mark-whole-word)
    (define-key map (kbd "s-l")   #'vscode-mark-whole-line)
    ;; Prefix map
    (let ((prefix (make-sparse-keymap)))
      (define-key prefix (kbd "w") #'vscode-kill-other-buffers)
      (define-key map (kbd "C-c k") prefix))
    map)
  "Keymap for `vscode-mode'.")

;;;###autoload
(define-minor-mode vscode-mode
  "VSCode-like UX mode for Emacs."
  :init-value nil
  :lighter " VSCode"
  :keymap vscode-mode-map
  (if vscode-mode
      (vscode-jump-mode 1)
    (vscode-jump-mode -1)))

;;;###autoload
(define-globalized-minor-mode global-vscode-mode vscode-mode
  vscode-mode)

;; ─────────────────────────────────────────────────────────────
;; Provide
;; ─────────────────────────────────────────────────────────────

(provide 'vscode)
;;; vscode.el ends here

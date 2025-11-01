;;; pylight-theme.el --- Py Light theme for Emacs -*- lexical-binding: t; -*-
;;
;; Inspired by VSCode "Py Light" theme
;; Author: ChatGPT (converted from VSCode JSON)
;; Version: 1.0
;; License: MIT
;;
;;; Commentary:
;; A light pastel theme resembling the VSCode Py Light color scheme.
;; Background: #eeffcc
;; Foreground: #000000
;;
;;; Code:

(deftheme pylight
  "A light, Python-friendly theme inspired by VSCode Py Light.")

(let ((class '((class color) (min-colors 89)))
      (bg "#eeffcc")
      (fg "#000000")
      (comment "#408090")
      (keyword "#007020")
      (number "#208050")
      (string "#4070a0")
      (function "#06287e")
      (type "#0e84b5")
      (punct "#4070a0")
      (operator "#666666")
      (error "#660000")
      (diff "#434343"))

  (custom-theme-set-faces
   'pylight

   ;; Base
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,fg))))
   `(region ((,class (:background "#cceebb"))))
   `(highlight ((,class (:background "#ddeebb"))))
   `(fringe ((,class (:background ,bg))))
   `(minibuffer-prompt ((,class (:foreground ,keyword :weight bold))))
   `(link ((,class (:foreground "#4B83CD" :underline t))))
   `(error ((,class (:foreground ,error :weight bold))))
   `(warning ((,class (:foreground "#c65d09" :weight bold))))
   `(success ((,class (:foreground "#00A000" :weight bold))))

   ;; Font-lock (syntax highlighting)
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:inherit font-lock-comment-face))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword :weight bold))))
   `(font-lock-type-face ((,class (:foreground ,type :weight bold))))
   `(font-lock-builtin-face ((,class (:foreground ,keyword :weight bold))))
   `(font-lock-function-name-face ((,class (:foreground ,function))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg))))
   `(font-lock-constant-face ((,class (:foreground ,number))))
   `(font-lock-string-face ((,class (:foreground ,string))))
   `(font-lock-number-face ((,class (:foreground ,number))))
   `(font-lock-operator-face ((,class (:foreground ,operator))))
   `(font-lock-punctuation-face ((,class (:foreground ,punct))))
   `(font-lock-warning-face ((,class (:foreground "#FF0000" :weight bold))))

   ;; Diff / VCS
   `(diff-added ((,class (:foreground "#00A000"))))
   `(diff-removed ((,class (:foreground "#A00000"))))
   `(diff-changed ((,class (:foreground "#0044DD"))))
   `(diff-header ((,class (:foreground ,diff))))
   `(diff-file-header ((,class (:foreground ,diff :weight bold))))

   ;; Markdown / org / prose
   `(markdown-header-face-1 ((,class (:foreground ,keyword :weight bold :height 1.2))))
   `(markdown-header-face-2 ((,class (:foreground ,keyword :weight bold :height 1.1))))
   `(markdown-header-face-3 ((,class (:foreground ,keyword :weight bold))))
   `(markdown-inline-code-face ((,class (:foreground "#c65d09" :weight bold))))
   `(markdown-italic-face ((,class (:slant italic))))
   `(markdown-bold-face ((,class (:weight bold))))
   `(markdown-list-face ((,class (:foreground ,keyword))))
   `(markdown-blockquote-face ((,class (:foreground ,comment :slant italic))))

   ;; Org-mode
   `(org-level-1 ((,class (:foreground ,keyword :weight bold :height 1.2))))
   `(org-level-2 ((,class (:foreground ,keyword :weight bold :height 1.1))))
   `(org-level-3 ((,class (:foreground ,keyword :weight bold))))
   `(org-block ((,class (:foreground "#333333"))))
   `(org-quote ((,class (:foreground ,comment :slant italic))))
   `(org-link ((,class (:foreground "#4B83CD" :underline t))))
   `(org-code ((,class (:foreground "#c65d09" :weight bold))))

   
   ;; Mode line styling
   `(mode-line
     ((,class (:background "#ddeebb"
               :foreground "#000000"
               :box (:line-width -1 :color "#ccccaa")
               :height 0.9))))  ;; smaller font

   `(mode-line-inactive
     ((,class (:background "#f0f0e0"
               :foreground "#666666"
               :box (:line-width -1 :color "#ddddcc")
               :height 0.9))))

   `(mode-line-buffer-id
     ((,class (:foreground "#06287e" :weight bold))))  ;; filename color (deep blue)   
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'pylight)
;;; pylight-theme.el ends here

;;; org-imenu.el --- a side org imenu with filters -*- lexical-binding: t -*-

;; Copyright (C) 2022 Nicolas P. Rougier

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/org-imenu
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") ("imenu-list"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  org-imenu allows to have a sidebar (imenu-list) that can be filtered using
;;  a regular org-search query.
;;
;;  Usage:
;;
;;  M-x : org-imenu
;;
;;; NEWS:
;;
;; Version 0.1
;; - First version
;;

;;; Code
(require 'org)
(require 'imenu-list)

(defgroup org-imenu nil
  "Org imenu filter"
  :group 'org-structure)

(defvar org-imenu-filter-history '()
  "Filter history list.")

(defcustom org-imenu-filter-prompt "FILTER: "
  "Prompt for the filter interactive method."
  :group 'org-imenu
  :type 'string)

(defcustom org-imenu-include-todo t
  "Whether to include todo status in Imenu items."
  :group 'org-imenu
  :type 'boolean)

(defcustom org-imenu-include-tags t
  "Whether to include tags in Imenu items."
  :group 'org-imenu
  :type 'boolean)

(defvar org-imenu-filter-function
  (cdr (org-make-tags-matcher "*"))
  "Filter to decide if a headline is kept.

It is usually interactively defined via 'org-imenu-filter-select'
but you can also provide your own function '(filter (todo tags level)
...)'.")

(defvar org-imenu--folding-status t
  "Folding status of the imenu-list")

(defun org-imenu-filter ()
  "Select a new filter using org-mode search syntax."
  
  (interactive)
  (let* ((match (completing-read-multiple
                 org-imenu-filter-prompt
                 org-imenu-filter-history
                 nil nil nil
                 'org-imenu-filter-history))
         (match (mapconcat #'identity match " ")))
    (when (string= "" match)
      (setq match "*"))
    (setq org-imenu-filter-function
          (cdr (org-make-tags-matcher match)))
    (org-imenu-update)))
    
    

(defun org-imenu-filter-format (element todo tags marker)
  "Format ELEMENT, TODO and TAGS as a string in order to insert it the index with the MARKER reference indicating where the element is in the document."

  (let* ((node (org-element-property :raw-value element))
         (node (org-link-display-format (substring-no-properties node)))
         (node (concat
                (when (and org-imenu-include-todo todo)
                  (format "%s " todo))
                node
                (when (and org-imenu-include-tags tags)
                  (propertize
                   (format " :%s:" (mapconcat 'identity  tags ":"))
                   'face 'org-tag))))
         (node (propertize node 'marker marker
                                'org-imenu-marker marker
                                'org-imenu t)))
    node))


(defun org-imenu-filter-get-tree (&optional bound parent-match)
  "Produce a filtered index for Imenu. Index is build from point to BOUND taking into account PARENT-MATCH that indicates if the parent node has been filtered out or not. Filtering is done using org-imenu-filter-current."
  
  (let* ((imenu '()))
    (save-excursion
      (org-with-wide-buffer
       (unless bound
         (setq bound (point-max))
         (goto-char (point-min)))
       (while (re-search-forward org-heading-regexp bound t)
         (let* ((element (org-element-at-point))
                (begin (org-element-property :begin element))
                (end (org-element-property :end element))
                (marker (copy-marker begin))
                (level (org-element-property :level element))
                (todo (org-element-property :todo-keyword element))
                (tags (save-excursion
                         (goto-char begin)
                          (org-get-tags)))
                (match (save-excursion
                         (goto-char begin)
                         (funcall org-imenu-filter-function
                                  nil (org-get-tags) level)))
                (node (org-imenu-filter-format element todo tags marker))
                (children (org-imenu-filter-get-tree end match)))
           (goto-char end)
           (cond ((> level org-imenu-depth)
                  nil)
                 ((> (length children) 0)
                  (add-to-list 'imenu (append (list node) children) t))
                 ((or match parent-match)
                  (add-to-list 'imenu (cons node marker) t)))))))
    imenu))

(defun org-imenu-prev-item ()
  "Move to previous header"
  
  (interactive)
  (with-current-buffer "*Ilist*"
    (search-backward-regexp "^  ")
    (org-imenu-show-item)))

(defun org-imenu-next-item ()
  "Move to next header"
    
  (interactive)
  (with-current-buffer "*Ilist*"
    (forward-line)
    (search-forward-regexp "^  ")
    (org-imenu-show-item)))

(defun org-imenu-prev-parent ()
  "Move to previous section"
    
  (interactive)
  (with-current-buffer "*Ilist*"
    (search-backward-regexp "\\+ " nil nil 1)
    (org-imenu-show-item)))

(defun org-imenu-next-parent ()
  "Move to next section"
    
  (interactive)
  (with-current-buffer "*Ilist*"
    (search-forward-regexp "\\+ ")
    (org-imenu-show-item)))

(defun org-imenu-show-item ()
  "Display the item at point."
  
  (interactive)
  (save-selected-window
    (save-excursion
      (my/imenu-list-ret-dwim))))

(defun org-imenu-goto-item ()
  "Go to the item at point."
  
  (interactive)
  (save-excursion
    (let ((entry (imenu-list--find-entry)))
      (when (imenu--subalist-p entry)
        (setq entry (cons
                     (car entry)
                     (get-text-property 0 'marker (car entry)))))
      (imenu-list--goto-entry entry))))

(defun org-imenu-toggle-folding-all ()
  "Toggle folding for all items."
  
  (interactive)
  (with-current-buffer "*Ilist*"
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\+ " nil t)
        (if org-imenu--folding-status
            (hs-hide-block)
          (hs-show-block)))
      (setq org-imenu--folding-status (not org-imenu--folding-status)))))

(defun org-imenu-toggle-folding-parent ()
  "Toggle folding for the parent of item at point"

  (interactive)
  (hs-toggle-hiding))

(defun org-imenu-increase-depth ()
  "Increase imenu depth"

  (interactive)
  (setq org-imenu-depth (+ org-imenu-depth 1))
  (org-imenu-update))

(defun org-imenu-decrease-depth ()
  "Decrease imenu depth"

  (interactive)
  (setq org-imenu-depth (max 1 (- org-imenu-depth 1)))
  (org-imenu-update))
  
(defun org-imenu-update ()
  "Update org-imenu"

  (interactive)
  (imenu-list-update t)
  (imenu-list-refresh))

(defun org-imenu ()
  "Activate org-imenu."

  (interactive)
  
  ;; Save current heading
  (let ((heading (substring-no-properties (or (org-get-heading t t t t) ""))))
    (when (buffer-base-buffer)
      (switch-to-buffer (buffer-base-buffer))
      (widen))
    (imenu-list-minor-mode)
    (imenu-list-stop-timer)
    (org-imenu-update)

    (when (> (length heading) 0)
      (goto-char (point-min))
      (search-forward heading)
      (imenu-list-display-dwim)))

  ;; Install our keymap
  (with-current-buffer "*Ilist*"
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n")       #'org-imenu-next-item)
      (define-key map (kbd "p")       #'org-imenu-prev-item)
      (define-key map (kbd "N")       #'org-imenu-next-parent)
      (define-key map (kbd "P")       #'org-imenu-prev-parent)
      (define-key map (kbd "+")       #'org-imenu-increase-depth)
      (define-key map (kbd "-")       #'org-imenu-decrease-depth)
      (define-key map (kbd "q")       #'org-imenu-quit)
      (define-key map (kbd "g")       #'org-imenu-update)
      (define-key map (kbd "f")       #'org-imenu-filter)
      (define-key map (kbd "S-<tab>") #'org-imenu-toggle-folding-all)
      (define-key map (kbd "TAB")     #'org-imenu-toggle-folding-parent)
      (define-key map (kbd "SPC")     #'org-imenu-show-item)
      (define-key map (kbd "<ret>")   #'org-imenu-goto-item)
      (use-local-map map)))
  
  (advice-add #'org-imenu-get-tree :override #'org-imenu-filter-get-tree))

(defun org-imenu-quit ()
  "Inactivate org-imenu."

  (interactive)
  (when (get-buffer-window "*Ilist*")
    (progn 
      (quit-window nil (get-buffer-window "*Ilist*"))
      (switch-to-buffer (buffer-base-buffer))
      (widen)))
  (advice-remove #'org-imenu-get-tree #'org-imenu-filter-get-tree))


(provide 'org-imenu)
;;; org-imenu.el ends here

